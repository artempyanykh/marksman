module Marksman.Server

open System
open System.IO

open Microsoft.FSharp.Control

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Logging
open FSharpPlus.GenericBuilders

open Marksman.Config
open Marksman.Cst
open Marksman.Diag
open Marksman.Index
open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Refs
open Marksman.Folder
open Marksman.Workspace
open Marksman.State
open Newtonsoft.Json.Linq

module ServerUtil =
    let logger = LogProvider.getLoggerByName "ServerUtil"

    let extractWorkspaceFolders (par: InitializeParams) : Map<string, FolderId> =
        match par.WorkspaceFolders with
        | Some folders ->
            folders
            |> Array.map (fun { Name = name; Uri = uri } -> name, UriWith.mkRoot uri)
            |> Array.filter (fun (_, folderId) -> Folder.checkWorkspaceFolderWithWarn folderId)
            |> Map.ofArray
        | _ ->
            let rootUri =
                par.RootUri
                |> Option.orElseWith (fun () -> par.RootPath |> Option.map systemPathToUriString)
                |> Option.map UriWith.mkRoot

            match rootUri with
            | None ->
                // No folders are configured. The client can still add folders later using a notification.
                Map.empty
            | Some rootUri ->
                if Folder.checkWorkspaceFolderWithWarn rootUri then
                    let rootName = RootPath.filename rootUri.data

                    Map.ofList [ rootName, rootUri ]
                else
                    Map.empty

    let readWorkspace (userConfig: option<Config>) (roots: Map<string, FolderId>) : list<Folder> =
        seq {
            for KeyValue(name, root) in roots do
                match Folder.tryLoad userConfig name root with
                | Some folder -> yield folder
                | _ -> ()
        }
        |> List.ofSeq

    let calcTextSync
        (userConfigOpt: Option<Config.Config>)
        (workspace: Workspace)
        (clientDesc: ClientDescription)
        =
        let configuredSyncKinds =
            Workspace.folders workspace
            |> Seq.choose Folder.config
            |> Seq.choose (fun x -> x.coreTextSync)
            |> Array.ofSeq

        if not (Array.isEmpty configuredSyncKinds) then
            let commonKind = Array.minBy TextSync.ord configuredSyncKinds
            "workspaceConfig", commonKind
        else
            match userConfigOpt |> Option.bind (fun x -> x.coreTextSync) with
            | Some kind -> "userConfig", kind
            | None ->
                match clientDesc.PreferredTextSyncKind with
                | Some kind -> "clientOption", kind
                | None -> "default", Full

    let mkServerCaps
        (markdownExts: array<string>)
        (textSyncKind: TextSync)
        (par: InitializeParams)
        : ServerCapabilities =
        let workspaceFoldersCaps = {
            Supported = Some true
            ChangeNotifications = Some(Second true)
        }

        let glob = mkWatchGlob markdownExts

        let markdownFilePattern = {
            Glob = glob
            Matches = Some FileOperationPatternKind.File
            Options = Some { IgnoreCase = Some true }
        }

        let markdownFileRegistration = {
            Filters = [| { Scheme = None; Pattern = markdownFilePattern } |]
        }

        let workspaceFileCaps = {
            WorkspaceFileOperationsServerCapabilities.Default with
                DidCreate = Some markdownFileRegistration
                DidDelete = Some markdownFileRegistration
                // VSCode behaves weirdly when communicating file renames, so let's turn this off.
                // Anyway, when the file is renamed VSCode sends
                // - didClose on the old name, and
                // - didOpen on the new one
                // which is enough to keep the state in sync.
                DidRename = None
        }

        let workspaceCaps = {
            WorkspaceFolders = Some workspaceFoldersCaps
            FileOperations = Some workspaceFileCaps
        }

        let syncKind =
            match textSyncKind with
            | Full -> TextDocumentSyncKind.Full
            | Incremental -> TextDocumentSyncKind.Incremental

        let textSyncCaps = {
            TextDocumentSyncOptions.Default with
                OpenClose = Some true
                Change = Some syncKind
        }


        let clientDesc = ClientDescription.ofParams par

        let codeActionOptions = { CodeActionKinds = None; ResolveProvider = Some false }

        let renameOptions =
            if clientDesc.SupportsPrepareRename then
                { PrepareProvider = Some true } |> U2.Second |> Some
            else
                Some(U2.First true)

        {
            ServerCapabilities.Default with
                Workspace = Some workspaceCaps
                WorkspaceSymbolProvider = Some(First <| not clientDesc.IsVSCode)
                TextDocumentSync = Some textSyncCaps
                DocumentSymbolProvider = Some(First <| not clientDesc.IsVSCode)
                CompletionProvider =
                    Some {
                        TriggerCharacters = Some [| '['; '#'; '(' |]
                        ResolveProvider = None
                        AllCommitCharacters = None
                        CompletionItem = None
                    }
                DefinitionProvider = Some true
                HoverProvider = Some true
                ReferencesProvider = Some true
                CodeActionProvider = Some(Second codeActionOptions)
                SemanticTokensProvider =
                    Some {
                        Legend = { TokenTypes = Semato.TokenType.mapping; TokenModifiers = [||] }
                        Range = Some true
                        Full = { Delta = Some false } |> U2.Second |> Some
                    }
                RenameProvider = renameOptions
                CodeLensProvider = Some { ResolveProvider = None }
                ExecuteCommandProvider = Some { commands = Some [||] }
        }

type MarksmanStatusParams = { state: string; docCount: int }

type MarksmanClient(notiSender: ClientNotificationSender, _reqSender: ClientRequestSender) =
    inherit LspClient()

    override this.TextDocumentPublishDiagnostics(par: PublishDiagnosticsParams) =
        notiSender "textDocument/publishDiagnostics" (box par) |> Async.Ignore

    member this.MarksmanUpdateStatus(par: MarksmanStatusParams) =
        notiSender "marksman/status" (box par) |> Async.Ignore

let calcDiagnosticsUpdate
    (prevState: Option<State>)
    (newState: State)
    : seq<PublishDiagnosticsParams> =
    let existingDiag =
        prevState
        |> Option.map State.diag
        |> Option.defaultValue WorkspaceDiag.empty

    let newDiag = State.diag newState

    let allFolders =
        Set.union (Map.keys existingDiag |> Set.ofSeq) (Map.keys newDiag |> Set.ofSeq)

    seq {
        for folderPath in allFolders do
            let existingFolderDiag =
                Map.tryFind folderPath existingDiag |> Option.defaultValue [||]

            let newFolderDiag =
                Map.tryFind folderPath newDiag |> Option.defaultValue [||]

            let allDocs =
                Set.union
                    (Array.map fst newFolderDiag |> Set.ofArray)
                    (Array.map fst existingFolderDiag |> Set.ofArray)

            logger.trace (
                Log.setMessage "Updating folder diag"
                >> Log.addContext "folder" folderPath
                >> Log.addContext "num_docs" allDocs.Count
            )

            for docUri in allDocs do
                let docPath = UriWith.rootedRelToAbs docUri.Raw
                let existingDoc = prevState |> Option.bind (State.tryFindDoc docPath)
                let existingDocVersion = Option.bind Doc.version existingDoc

                let existingDocDiag =
                    existingFolderDiag
                    |> Array.tryFind (fun (uri, _) -> uri = docUri)
                    |> Option.map snd
                    |> Option.defaultValue [||]

                let newDoc = State.tryFindDoc docPath newState
                let newDocVersion = Option.bind Doc.version newDoc

                let newDocDiag =
                    newFolderDiag
                    |> Array.tryFind (fun (uri, _) -> uri = docUri)
                    |> Option.map snd
                    |> Option.defaultValue [||]

                let shouldUpdate =
                    newDocDiag <> existingDocDiag
                    // Diag didn't change but the document was re-opened; re-send the diag
                    || (existingDocVersion = None
                        && Option.isSome newDocVersion
                        && not (Array.isEmpty newDocDiag))

                if shouldUpdate then
                    logger.trace (
                        Log.setMessage "Diagnostic changed, queueing the update"
                        >> Log.addContext "doc" docUri
                    )

                    // TODO: check how the behavior changes when we supply the document version here
                    let publishParams = {
                        Uri = docUri.Uri
                        Diagnostics = newDocDiag
                        Version = None
                    }

                    yield publishParams
    }

type DiagnosticsManager(client: MarksmanClient) =
    let logger = LogProvider.getLoggerByName "BackgroundAgent"

    let agent: MailboxProcessor<State> =
        MailboxProcessor.Start(fun inbox ->
            let rec accumulate lastProcessedState mostRecentState =
                async {
                    // 200ms grace period to avoid recalculating diagnostics during active editing
                    // The diagnostics update still feels pretty much instant, but doing it this
                    // way is much more efficient
                    let! newState = inbox.TryReceive(timeout = 200)

                    match newState with
                    | None -> return! publishOn lastProcessedState mostRecentState
                    | Some newState -> return! accumulate lastProcessedState newState
                }

            and publishOn lastProcessedState mostRecentState =
                async {
                    let diagnostics = calcDiagnosticsUpdate lastProcessedState mostRecentState

                    for update in diagnostics do
                        do! client.TextDocumentPublishDiagnostics(update)

                    return! waitStateUpdate (Some mostRecentState)
                }

            and waitStateUpdate lastProcessedState =
                async {
                    let! newState = inbox.Receive()
                    return! accumulate lastProcessedState newState
                }

            logger.trace (Log.setMessage "Preparing to start background agent")

            waitStateUpdate None)

    member this.UpdateDiagnostics(state: State) : unit = agent.Post(state)

    interface IDisposable with
        member _.Dispose() = (agent :> IDisposable).Dispose()

let queueDiagnosticsUpdate
    (manager: DiagnosticsManager)
    (_prevState: Option<State>)
    (newState: State)
    =
    manager.UpdateDiagnostics(newState)

type StatusMessage = DocCount of int

type StatusManager(client: MarksmanClient) =
    let logger = LogProvider.getLoggerByName "StatusAgent"

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop cnt =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | DocCount newCnt when cnt <> newCnt ->
                        logger.trace (
                            Log.setMessage "StatusAgent sending update"
                            >> Log.addContext "docCount" (cnt, newCnt)
                        )

                        do! client.MarksmanUpdateStatus({ state = "ok"; docCount = newCnt })
                        return! loop newCnt
                    | _ -> return! loop cnt
                }

            logger.trace (Log.setMessage "StatusAgent starting")
            loop 0)

    member this.UpdateDocCount(cnt: int) : unit = agent.Post(DocCount cnt)

    interface IDisposable with
        member _.Dispose() = (agent :> IDisposable).Dispose()

let queueStatusUpdate (manager: StatusManager) (_: Option<State>) (newState: State) : unit =
    let docCount = State.workspace newState |> Workspace.docCount
    manager.UpdateDocCount docCount

type Hook = { name: string; fn: Option<State> -> State -> unit }

type Mutation<'R> = { output: 'R; state: option<State>; hooks: list<Hook> }

module Mutation =
    let empty: Mutation<unit> = { output = (); state = None; hooks = [] }
    let output<'R> (output: 'R) : Mutation<'R> = { output = output; state = None; hooks = [] }
    let state (state: State) : Mutation<unit> = { output = (); state = Some state; hooks = [] }

    let stateOpt (stateOpt: option<State>) : Mutation<unit> =
        match stateOpt with
        | Some state -> { output = (); state = Some state; hooks = [] }
        | None -> empty

    let hooks (hooks: list<Hook>) : Mutation<unit> = { output = (); state = None; hooks = hooks }

type StateMessage =
    | ReadState of AsyncReplyChannel<State>
    | MutateState of (State -> Option<State> * list<Hook>)

    member this.Name =
        match this with
        | ReadState _ -> "ReadState"
        | MutateState _ -> "MutateState"

type StateManager(initState: State) =
    let logger = LogProvider.getLoggerByName "StateManager"
    let asyncResponseTimeout = 5000

    let processHook hook prevState state =
        logger.trace (
            Log.setMessage "Processing a hook"
            >> Log.addContext "name" hook.name
            >> Log.addContext "prevRev" (prevState |> Option.map State.revision)
            >> Log.addContext "curRev" (State.revision state)
        )

        hook.fn prevState state

    let agent: MailboxProcessor<StateMessage> =
        MailboxProcessor.Start
        <| fun inbox ->
            let rec go (prevState: Option<State>) (state: State) (hooks: list<Hook>) =
                async {
                    let! msg = inbox.Receive()

                    logger.trace (
                        Log.setMessage "Received a message" >> Log.addContext "type" msg.Name
                    )

                    match msg with
                    | ReadState chan ->
                        chan.Reply state
                        return! go prevState state hooks
                    | MutateState mutator ->
                        let newState, addedHooks =
                            try
                                mutator state
                            with ex ->
                                // Failing to update a state is a fatal error. We should crash
                                Fatality.abort (Some state) ex

                        // Step 1: run _added_ hooks on the existing state

                        let mutable newHooks = hooks

                        for hook in addedHooks do
                            processHook hook prevState state

                            logger.trace (
                                Log.setMessage "Adding a hook" >> Log.addContext "name" hook.name
                            )

                            newHooks <- hook :: newHooks

                        // Step 2: update the state and run _all_ hooks on the updated state

                        match newState with
                        | Some newState ->
                            logger.trace (
                                Log.setMessage "Updating state"
                                >> Log.addContext "curRev" (State.revision state)
                                >> Log.addContext "nextRev" (State.revision newState)
                            )

                            for hook in newHooks do
                                processHook hook (Some state) newState

                            return! go (Some state) newState newHooks
                        | None -> return! go prevState state newHooks
                }

            go None initState []

    member this.AccessToRead<'R>(f: State -> 'R) : Async<'R> =
        async {
            let! state = agent.PostAndAsyncReply(ReadState, timeout = asyncResponseTimeout)
            return f state
        }

    member this.AccessExclusively<'R>(f: State -> Mutation<'R>) : Async<'R> =
        let mkMsg (chan: AsyncReplyChannel<'R>) =
            let mutator state =
                let mutation = f state
                chan.Reply(mutation.output)
                mutation.state, mutation.hooks

            MutateState mutator

        agent.PostAndAsyncReply(mkMsg, timeout = asyncResponseTimeout)

    interface IDisposable with
        member _.Dispose() = (agent :> IDisposable).Dispose()

type MarksmanServer(client: MarksmanClient) =
    inherit LspServer()

    let diagnosticsManager = new DiagnosticsManager(client)

    let statusManager = new StatusManager(client)

    let mutable stateManager: option<StateManager> = None

    let requireStateManager () =
        stateManager
        |> Option.defaultWith (fun () -> failwith "State is not initialized")

    let withState (f: State -> 'R) =
        let sm = requireStateManager ()
        sm.AccessToRead f

    let withStateExclusive (f: State -> Mutation<'R>) =
        let sm = requireStateManager ()
        sm.AccessExclusively f

    let logger = LogProvider.getLoggerByName "MarksmanServer"

    let tryLoadUserConfig () : option<Config> =
        if File.Exists(Config.userConfigFile) then
            logger.trace (
                Log.setMessage "Found user config"
                >> Log.addContext "config" Config.userConfigFile
            )

            let config = Config.read Config.userConfigFile

            if Option.isNone config then
                logger.error (
                    Log.setMessage "Malformed user config, skipping"
                    >> Log.addContext "config" Config.userConfigFile
                )

            config
        else
            logger.trace (
                Log.setMessage "No user config found"
                >> Log.addContext "path" Config.userConfigFile
            )

            None

    override this.Initialize(par: InitializeParams) : AsyncLspResult<InitializeResult> =
        try
            let workspaceFolders = ServerUtil.extractWorkspaceFolders par
            let clientDesc = ClientDescription.ofParams par

            logger.debug (
                Log.setMessage "Obtained workspace folders"
                >> Log.addContext "workspace" workspaceFolders
            )

            let userConfig = tryLoadUserConfig ()
            let folders = ServerUtil.readWorkspace userConfig workspaceFolders
            let numNotes = folders |> List.sumBy Folder.docCount

            logger.debug (
                Log.setMessage "Completed reading workspace folders"
                >> Log.addContext "numFolders" folders.Length
                >> Log.addContext "numNotes" numNotes
            )

            let workspace = Workspace.ofFolders userConfig folders
            let initState = State.mk clientDesc workspace
            stateManager <- Some(new StateManager(initState))

            // Workspace may contain several folders with their own configuration. However, server
            // capabilities are communicated for the whole workspace. Therefore, we need to collect
            // all configured markdown extensions and ask the client to watch them all, doing per-folder
            // filtering on our own.
            //
            // NOTE: this doesn't address the case when a folder is added to the workspace later on.
            // We'd need to add dynamic registration of capabilities on the server side.
            let configuredExts =
                Workspace.folders workspace
                |> Seq.collect Folder.configuredMarkdownExts
                |> Seq.distinct
                |> Array.ofSeq

            let textSyncKindSource, textSyncKind =
                ServerUtil.calcTextSync userConfig workspace clientDesc

            logger.debug (
                Log.setMessage "Configured text sync"
                >> Log.addContext "source" textSyncKindSource
                >> Log.addContext "kind" textSyncKind
            )

            let serverCaps = ServerUtil.mkServerCaps configuredExts textSyncKind par

            let initResult = { InitializeResult.Default with Capabilities = serverCaps }

            logger.debug (
                Log.setMessage
                    "Finished workspace initialization. Waiting for the `initialized` notification from the client."
            )

            AsyncLspResult.success initResult
        with ex ->
            Fatality.abort None ex

    override this.Initialized(_: InitializedParams) =
        withStateExclusive
        <| fun state ->
            logger.debug (
                Log.setMessage
                    "Received `initialized` notification from the client. Setting up background services."
            )

            let diagHook = queueDiagnosticsUpdate diagnosticsManager
            let mutable newHooks = [ { name = "diag"; fn = diagHook } ]

            if (State.client state).SupportsStatus then
                logger.debug (
                    Log.setMessage "Client supports status notifications. Initializing agent."
                )

                let statusHook = queueStatusUpdate statusManager
                newHooks <- { name = "status"; fn = statusHook } :: newHooks
            else
                logger.debug (
                    Log.setMessage
                        "Client doesn't support status notifications. Agent won't be initialized."
                )

            logger.debug (Log.setMessage "Initialization complete.")

            Mutation.hooks newHooks

    override this.Shutdown() =
        logger.trace (Log.setMessage "Preparing for shutdown")
        async.Return()

    override this.Exit() =
        logger.trace (Log.setMessage "Exiting")
        async.Return()

    override this.TextDocumentDidChange(par: DidChangeTextDocumentParams) =
        withStateExclusive
        <| fun state ->
            let docPath = par.TextDocument.Uri |> UriWith.mkAbs

            let newState =
                match State.tryFindFolderAndDoc docPath state with
                | Some(folder, doc) ->
                    let newDoc = Doc.applyLspChange (Folder.parserSettings folder) par doc

                    let newFolder = Folder.withDoc newDoc folder

                    State.updateFolder newFolder state |> Some
                | _ ->
                    logger.warn (
                        Log.setMessage "Document not found"
                        >> Log.addContext "method" "textDocumentDidChange"
                        >> Log.addContext "uri" docPath
                    )

                    None

            Mutation.stateOpt newState

    override this.TextDocumentDidClose(par: DidCloseTextDocumentParams) =
        withStateExclusive
        <| fun state ->
            let path = par.TextDocument.Uri |> UriWith.mkAbs

            match State.tryFindFolderAndDoc path state with
            | None -> Mutation.empty
            | Some(folder, doc) ->
                let newState =
                    match Folder.closeDoc (Doc.id doc) folder with
                    | Some folder -> State.updateFolder folder state
                    | _ -> State.removeFolder (Folder.id folder) state

                Mutation.state newState

    override this.TextDocumentDidOpen(par: DidOpenTextDocumentParams) =
        withStateExclusive
        <| fun state ->
            let path = par.TextDocument.Uri |> UriWith.mkAbs

            let newState =
                match State.tryFindFolderEnclosing path state with
                | None ->
                    let config = State.userConfigOrDefault state
                    let parserSettings = ParserSettings.OfConfig(config)

                    if isMarkdownFile parserSettings.mdFileExt (AbsPath.toSystem path.data) then
                        let singletonRoot = UriWith.mkRoot par.TextDocument.Uri

                        logger.trace (
                            Log.setMessage "Opening document in single-file mode"
                            >> Log.addContext "uri" par.TextDocument.Uri
                            >> Log.addContext "root" singletonRoot
                        )

                        let doc = Doc.fromLsp parserSettings singletonRoot par.TextDocument
                        let userConfig = (State.workspace state) |> Workspace.userConfig
                        let newFolder = Folder.singleFile doc userConfig
                        State.updateFolder newFolder state
                    else
                        state
                | Some folder ->
                    let parserSettings = Folder.parserSettings folder

                    if isMarkdownFile parserSettings.mdFileExt (AbsPath.toSystem path.data) then
                        let doc = Doc.fromLsp parserSettings (Folder.id folder) par.TextDocument
                        let newFolder = Folder.withDoc doc folder
                        State.updateFolder newFolder state
                    else
                        state

            Mutation.state newState

    override this.WorkspaceDidChangeWorkspaceFolders(par: DidChangeWorkspaceFoldersParams) =
        withStateExclusive
        <| fun state ->
            let newState =
                State.updateFoldersFromLsp par.Event.Added par.Event.Removed state

            Mutation.state newState


    override this.WorkspaceDidCreateFiles(par: CreateFilesParams) =
        withStateExclusive
        <| fun state ->
            let docUris = par.Files |> Array.map (fun fc -> UriWith.mkAbs fc.Uri)

            let mutable newState = state

            for docUri in docUris do
                logger.trace (
                    Log.setMessage "Processing file create not"
                    >> Log.addContext "uri" docUri
                )

                match State.tryFindFolderEnclosing docUri newState with
                | None -> ()
                | Some folder ->
                    let parserSettings = Folder.parserSettings folder

                    if isMarkdownFile parserSettings.mdFileExt (AbsPath.toSystem docUri.data) then
                        match Doc.tryLoad parserSettings (Folder.id folder) (Abs docUri.data) with
                        | Some doc ->
                            let newFolder = Folder.withDoc doc folder
                            newState <- State.updateFolder newFolder newState
                        | _ ->
                            logger.warn (
                                Log.setMessage "Couldn't load created document"
                                >> Log.addContext "uri" docUri
                            )

                            ()

            Mutation.state newState

    override this.WorkspaceDidDeleteFiles(par: DeleteFilesParams) =
        withStateExclusive
        <| fun state ->
            let mutable newState = state

            let deletedUris = par.Files |> Array.map (fun x -> UriWith.mkAbs x.Uri)

            for uri in deletedUris do
                logger.trace (
                    Log.setMessage "Processing file delete not"
                    >> Log.addContext "uri" uri
                )

                match State.tryFindFolderAndDoc uri state with
                | None -> ()
                | Some(folder, doc) ->
                    match Folder.withoutDoc (Doc.id doc) folder with
                    | None -> newState <- State.removeFolder (Folder.id folder) newState
                    | Some newFolder -> newState <- State.updateFolder newFolder newState

            Mutation.state newState


    override this.WorkspaceSymbol(pars) =
        withState
        <| fun state ->
            let ws = State.workspace state
            let symbols = Symbols.workspaceSymbols pars.Query ws
            LspResult.success (Some <| First symbols)

    override this.TextDocumentDocumentSymbol(par: DocumentSymbolParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> UriWith.mkAbs

            let client = (State.client state)

            let getSymbols =
                Symbols.docSymbols client.SupportsHierarchy client.IsEmacs

            let response = State.tryFindDoc docUri state |> Option.map getSymbols
            LspResult.success response

    override this.TextDocumentCompletion(par: CompletionParams) =
        withState
        <| fun state ->
            logger.trace (Log.setMessage "Completion request start")

            let pos = par.Position
            let docUri = par.TextDocument.Uri |> UriWith.mkAbs

            let candidates =
                monad' {
                    let! folder, doc = State.tryFindFolderAndDoc docUri state

                    let maxCompletions = (Folder.configOrDefault folder).ComplCandidates()

                    match
                        Compl.findCandidatesInDoc folder doc pos
                        |> Seq.truncate maxCompletions
                        |> Array.ofSeq
                    with
                    | [||] -> return! None
                    | candidates ->
                        let isIncomplete = Array.length candidates >= maxCompletions

                        {
                            IsIncomplete = isIncomplete
                            Items = candidates
                            ItemDefaults = None
                        }
                }

            LspResult.success candidates

    override this.TextDocumentDefinition(par: TextDocumentPositionParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> UriWith.mkAbs

            let goto =
                monad' {
                    let! folder, srcDoc = State.tryFindFolderAndDoc docUri state

                    let! atPos = Doc.index srcDoc |> Index.linkAtPos par.Position
                    let refs = Dest.tryResolveElement folder srcDoc atPos

                    let locs =
                        refs
                        |> Seq.map (fun ref -> {
                            Uri = ref |> Dest.doc |> Doc.uri
                            Range = (Dest.range ref)
                        })
                        |> Array.ofSeq

                    if locs.Length = 0 then return! None
                    else if locs.Length = 1 then GotoResult.Single locs[0]
                    else GotoResult.Multiple locs
                }

            LspResult.success goto

    override this.TextDocumentHover(par: TextDocumentPositionParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> UriWith.mkAbs

            let hover =
                monad {
                    let! folder, srcDoc = State.tryFindFolderAndDoc docUri state

                    let! atPos = Doc.index srcDoc |> Index.linkAtPos par.Position
                    // NOTE: Due to ambiguity there may be several sources for hover. Since hover
                    // request requires a single result we return the first. When links are not
                    // ambiguous this is OK, otherwise the author is to blame for ambiguity anyway.
                    let! ref = Dest.tryResolveElement folder srcDoc atPos |> Seq.tryHead

                    let destScope = Dest.scope ref

                    let content =
                        (Dest.doc >> Doc.text <| ref).Substring destScope
                        |> markdown
                        |> MarkupContent

                    let hover = { Contents = content; Range = None }

                    hover
                }

            LspResult.success hover



    override this.TextDocumentReferences(par: ReferenceParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> UriWith.mkAbs

            match State.tryFindFolderAndDoc docUri state with
            | Some(folder, curDoc) ->
                let locs =
                    match Cst.elementAtPos par.Position (Doc.cst curDoc) with
                    | None ->
                        logger.warn (
                            Log.setMessage "Could not find an element for 'TextDocumentReferences'"
                            >> Log.addContext "doc" curDoc.Id
                            >> Log.addContext "pos" par.Position
                        )

                        None
                    | Some atPos ->
                        let referencingEls =
                            Dest.findElementRefs par.Context.IncludeDeclaration folder curDoc atPos

                        let toLoc (doc, el) = { Uri = Doc.uri doc; Range = Element.range el }

                        referencingEls |> Seq.map toLoc |> Array.ofSeq |> Some

                LspResult.success locs
            | None -> LspResult.success None

    override this.TextDocumentSemanticTokensFull(par: SemanticTokensParams) =
        withState
        <| fun state ->
            let docPath = par.TextDocument.Uri |> UriWith.mkAbs

            let tokens =
                monad' {
                    let! _, doc = State.tryFindFolderAndDoc docPath state
                    let data = Semato.Token.ofIndexEncoded (Doc.index doc)
                    { ResultId = None; Data = data }
                }

            LspResult.success tokens

    override this.TextDocumentSemanticTokensRange(par: SemanticTokensRangeParams) =
        withState
        <| fun state ->
            let docPath = par.TextDocument.Uri |> UriWith.mkAbs
            let range = par.Range

            let tokens =
                monad' {
                    let! _, doc = State.tryFindFolderAndDoc docPath state
                    let data = Semato.Token.ofIndexEncodedInRange (Doc.index doc) range
                    { ResultId = None; Data = data }
                }

            LspResult.success tokens

    override this.TextDocumentCodeAction(opts: CodeActionParams) =
        withStateExclusive
        <| fun state ->
            let docPath = opts.TextDocument.Uri |> UriWith.mkAbs

            let codeAction title kind edit = {
                Title = title
                Kind = kind
                Diagnostics = None
                Command = None
                Data = None
                IsPreferred = Some false
                Disabled = None
                Edit = Some edit
            }

            match State.tryFindFolderAndDoc docPath state with
            | None -> Mutation.output (LspResult.success None)
            | Some(folder, doc) ->
                let config = Folder.configOrDefault folder

                let tocAction =
                    if config.CaTocEnable() then
                        CodeActions.tableOfContents opts.Range opts.Context config doc
                        |> Option.toArray
                        |> Array.map (fun ca ->
                            let wsEdit =
                                (CodeActions.documentEdit ca.edit ca.newText opts.TextDocument.Uri)

                            let caKind = Some CodeActionKind.Source

                            codeAction ca.name caKind wsEdit)
                    else
                        [||]

                let createMissingFileAction =
                    if config.CaCreateMissingFileEnable() then
                        CodeActions.createMissingFile opts.Range opts.Context doc folder
                        |> Option.toArray
                        |> Array.map (fun ca ->
                            let wsEdit = CodeActions.createFile ca.newFileUri
                            let caKind = Some CodeActionKind.QuickFix
                            codeAction ca.name caKind wsEdit)
                    else
                        [||]

                let codeActions: TextDocumentCodeActionResult =
                    Array.concat [| tocAction; createMissingFileAction |]
                    |> Array.map U2.Second

                Mutation.output (LspResult.success (Some codeActions))


    override this.TextDocumentRename(pars) =
        withStateExclusive
        <| fun state ->
            let docPath = pars.TextDocument.Uri |> UriWith.mkAbs

            let edit: option<LspResult<option<WorkspaceEdit>>> =
                monad' {
                    let! folder, srcDoc = State.tryFindFolderAndDoc docPath state

                    let renameResult =
                        Refactor.rename
                            (State.client state).SupportsDocumentEdit
                            folder
                            srcDoc
                            pars.Position
                            pars.NewName

                    return (Refactor.RenameResult.toLsp renameResult)
                }

            match edit with
            | None -> Mutation.output (Ok None)
            | Some result -> Mutation.output result

    override this.TextDocumentPrepareRename(pars) =
        withStateExclusive
        <| fun state ->
            let docPath = pars.TextDocument.Uri |> UriWith.mkAbs

            let renameRange =
                monad' {
                    let! _, srcDoc = State.tryFindFolderAndDoc docPath state
                    return! Refactor.renameRange srcDoc pars.Position
                }

            Mutation.output (renameRange |> Option.map PrepareRenameResult.Range |> Ok)


    override this.TextDocumentCodeLens(pars) =
        withState
        <| fun state ->
            let docPath = pars.TextDocument.Uri |> UriWith.mkAbs

            let lenses =
                monad' {
                    let! folder, srcDoc = State.tryFindFolderAndDoc docPath state
                    Lenses.forDoc (State.client state) folder srcDoc
                }

            LspResult.success lenses

    override this.WorkspaceExecuteCommand(pars) =
        if pars.Command = Lenses.findReferencesLens then
            // Code lenses need an associated command. We provide a dummy implementation here
            // because 'showing references' need to be handled by the client and it's a bit too much
            // hassle to do this for every client.
            AsyncLspResult.success (JToken.FromObject(0))
        else
            AsyncLspResult.invalidParams $"Command {pars.Command} is unsupported"

    override this.Dispose() =
        (statusManager :> IDisposable).Dispose()
        (diagnosticsManager :> IDisposable).Dispose()

        match stateManager with
        | Some stateManager -> (stateManager :> IDisposable).Dispose()
        | _ -> ()
