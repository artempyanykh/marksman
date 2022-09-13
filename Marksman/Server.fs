module Marksman.Server

open System
open System.IO

open Microsoft.FSharp.Control

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Logging
open FSharpPlus.GenericBuilders

open Marksman.Cst
open Marksman.Diag
open Marksman.Misc
open Marksman.Workspace
open Marksman.State
open Marksman.Index
open Marksman.Refs

let extractWorkspaceFolders (par: InitializeParams) : Map<string, RootPath> =
    match par.WorkspaceFolders with
    | Some folders ->
        folders
        |> Array.map (fun { Name = name; Uri = uri } -> name, RootPath.ofString uri)
        |> Map.ofArray
    | _ ->
        let rootPath = par.RootUri |> Option.orElse par.RootPath

        match rootPath with
        | None ->
            // No folders are configured. The client can still add folders later using a notification.
            Map.empty
        | Some rootPath ->
            let rootUri = PathUri.ofString rootPath

            let rootName = Path.GetFileName(rootUri.LocalPath)

            Map.ofList [ rootName, RootPath.ofPath rootUri ]

let readWorkspace (roots: Map<string, RootPath>) : list<Folder> =
    seq {
        for KeyValue (name, root) in roots do
            match Folder.tryLoad name root with
            | Some folder -> yield folder
            | _ -> ()
    }
    |> List.ofSeq

let mkServerCaps (par: InitializeParams) : ServerCapabilities =
    let workspaceFoldersCaps =
        { Supported = Some true; ChangeNotifications = Some true }

    let markdownFilePattern =
        { Glob = "**/*.md"
          Matches = Some FileOperationPatternKind.File
          Options = Some { FileOperationPatternOptions.Default with IgnoreCase = Some true } }

    let markdownFileRegistration =
        { Filters = [| { Scheme = None; Pattern = markdownFilePattern } |] }

    let workspaceFileCaps =
        { WorkspaceFileOperationsServerCapabilities.Default with
            DidCreate = Some markdownFileRegistration
            DidDelete = Some markdownFileRegistration
            // VSCode behaves weirdly when communicating file renames, so let's turn this off.
            // Anyway, when the file is renamed VSCode sends
            // - didClose on the old name, and
            // - didOpen on the new one
            // which is enough to keep the state in sync.
            DidRename = None }

    let workspaceCaps =
        { WorkspaceServerCapabilities.Default with
            WorkspaceFolders = Some workspaceFoldersCaps
            FileOperations = Some workspaceFileCaps }

    let textSyncCaps =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Change = Some TextDocumentSyncKind.Incremental }


    let clientDesc = ClientDescription.ofParams par

    let codeActionOptions =
        { CodeActionKinds = None; ResolveProvider = Some false }

    let renameOptions =
        if clientDesc.SupportsPrepareRename then
            { PrepareProvider = Some true } |> U2.Second |> Some
        else
            Some(U2.First true)

    { ServerCapabilities.Default with
        Workspace = Some workspaceCaps
        WorkspaceSymbolProvider = Some(not clientDesc.IsVSCode)
        TextDocumentSync = Some textSyncCaps
        DocumentSymbolProvider = Some(not clientDesc.IsVSCode)
        CompletionProvider =
            Some
                { TriggerCharacters = Some [| '['; '#'; '(' |]
                  ResolveProvider = None
                  AllCommitCharacters = None }
        DefinitionProvider = Some true
        HoverProvider = Some true
        ReferencesProvider = Some true
        CodeActionProvider = Some codeActionOptions
        SemanticTokensProvider =
            Some
                { Legend = { TokenTypes = Semato.TokenType.mapping; TokenModifiers = [||] }
                  Range = Some true
                  Full = { Delta = Some false } |> U2.Second |> Some }
        RenameProvider = renameOptions }

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
                let existingDoc = prevState |> Option.bind (State.tryFindDoc docUri)
                let existingDocVersion = Option.bind Doc.version existingDoc

                let existingDocDiag =
                    existingFolderDiag
                    |> Array.tryFind (fun (uri, _) -> uri = docUri)
                    |> Option.map snd
                    |> Option.defaultValue [||]

                let newDoc = State.tryFindDoc docUri newState
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

                    let publishParams = { Uri = docUri.DocumentUri; Diagnostics = newDocDiag }

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
                        let newState, addedHooks = mutator state

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

    override this.Initialize(par: InitializeParams) : AsyncLspResult<InitializeResult> =
        let workspaceFolders = extractWorkspaceFolders par

        logger.debug (
            Log.setMessage "Obtained workspace folders"
            >> Log.addContext "workspace" workspaceFolders
        )

        let folders = readWorkspace workspaceFolders

        let numNotes = folders |> List.sumBy Folder.docCount

        logger.debug (
            Log.setMessage "Completed reading workspace folders"
            >> Log.addContext "numFolders" folders.Length
            >> Log.addContext "numNotes" numNotes
        )

        let clientDesc = ClientDescription.ofParams par

        let initState = State.mk clientDesc (Workspace.ofFolders folders)

        stateManager <- Some(new StateManager(initState))

        let serverCaps = mkServerCaps par

        let initResult =
            { InitializeResult.Default with Capabilities = serverCaps }

        logger.debug (
            Log.setMessage
                "Finished workspace initialization. Waiting for the `initialized` notification from the client."
        )

        AsyncLspResult.success initResult


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
            let docUri = par.TextDocument.Uri |> PathUri.ofString

            let newState =
                match State.tryFindFolderAndDoc docUri state with
                | Some (folder, doc) ->
                    let newDoc = Doc.applyLspChange par doc
                    let newFolder = Folder.withDoc newDoc folder

                    State.updateFolder newFolder state |> Some
                | _ ->
                    logger.warn (
                        Log.setMessage "Document not found"
                        >> Log.addContext "method" "textDocumentDidChange"
                        >> Log.addContext "uri" docUri
                    )

                    None

            Mutation.stateOpt newState

    override this.TextDocumentDidClose(par: DidCloseTextDocumentParams) =
        withStateExclusive
        <| fun state ->
            let path = par.TextDocument.Uri |> PathUri.ofString
            let folder = State.tryFindFolderEnclosing path state

            match folder with
            | None -> Mutation.empty
            | Some folder ->
                let newState =
                    match Folder.closeDoc path folder with
                    | Some folder -> State.updateFolder folder state
                    | _ -> State.removeFolder (Folder.id folder) state

                Mutation.state newState


    override this.TextDocumentDidOpen(par: DidOpenTextDocumentParams) =
        withStateExclusive
        <| fun state ->
            let path = par.TextDocument.Uri |> PathUri.ofString

            let newFolder =
                match State.tryFindFolderEnclosing path state with
                | None ->
                    let singletonRoot =
                        Path.GetDirectoryName path.LocalPath |> RootPath.ofString

                    let doc = Doc.fromLsp singletonRoot par.TextDocument
                    Folder.singleFile doc
                | Some folder ->
                    let doc = Doc.fromLsp (Folder.rootPath folder) par.TextDocument
                    Folder.withDoc doc folder

            let newState = State.updateFolder newFolder state
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
            let docUris = par.Files |> Array.map (fun fc -> PathUri.ofString fc.Uri)

            let mutable newState = state

            for docUri in docUris do
                logger.trace (
                    Log.setMessage "Processing file create not"
                    >> Log.addContext "uri" docUri
                )

                match State.tryFindFolderEnclosing docUri newState with
                | None -> ()
                | Some folder ->
                    match Doc.tryLoad (Folder.rootPath folder) docUri with
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

            let deletedUris = par.Files |> Array.map (fun x -> PathUri.ofString x.Uri)

            for uri in deletedUris do
                logger.trace (
                    Log.setMessage "Processing file delete not"
                    >> Log.addContext "uri" uri
                )

                match State.tryFindFolderAndDoc uri state with
                | None -> ()
                | Some (folder, doc) ->
                    match Folder.withoutDoc (Doc.path doc) folder with
                    | None -> newState <- State.removeFolder (Folder.id folder) newState
                    | Some newFolder -> newState <- State.updateFolder newFolder newState

            Mutation.state newState


    override this.WorkspaceSymbol(pars) =
        withState
        <| fun state ->
            let ws = State.workspace state
            let symbols = Symbols.workspaceSymbols pars.Query ws
            LspResult.success (Some symbols)

    override this.TextDocumentDocumentSymbol(par: DocumentSymbolParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> PathUri.ofString

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
            let docUri = par.TextDocument.Uri |> PathUri.ofString

            let candidates =
                monad' {
                    let! folder = State.tryFindFolderEnclosing docUri state

                    match Compl.findCandidates pos docUri folder with
                    | [||] -> return! None
                    | candidates -> { IsIncomplete = true; Items = candidates }
                }

            LspResult.success candidates

    override this.TextDocumentDefinition(par: TextDocumentPositionParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> PathUri.ofString

            let goto =
                monad {
                    let! folder = State.tryFindFolderEnclosing docUri state
                    let! srcDoc = Folder.tryFindDocByPath docUri folder
                    let! atPos = Doc.index srcDoc |> Index.linkAtPos par.Position
                    let! uref = Uref.ofElement atPos

                    let refs = Dest.tryResolveUref uref srcDoc folder

                    let locs =
                        refs
                        |> Seq.map (fun ref ->
                            { Uri = ref |> Dest.doc |> Doc.uri; Range = (Dest.range ref) })
                        |> Array.ofSeq

                    if locs.Length = 0 then return! None
                    else if locs.Length = 1 then GotoResult.Single locs[0]
                    else GotoResult.Multiple locs
                }

            LspResult.success goto

    override this.TextDocumentHover(par: TextDocumentPositionParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> PathUri.ofString

            let hover =
                monad {
                    let! folder = State.tryFindFolderEnclosing docUri state
                    let! srcDoc = Folder.tryFindDocByPath docUri folder
                    let! atPos = Doc.index srcDoc |> Index.linkAtPos par.Position
                    let! uref = Uref.ofElement atPos
                    // NOTE: Due to ambiguity there may be several sources for hover. Since hover
                    // request requires a single result we return the first. When links are not
                    // ambiguous this is OK, otherwise the author is to blame for ambiguity anyway.
                    let! ref = Dest.tryResolveUref uref srcDoc folder |> Seq.tryHead

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
            let docUri = par.TextDocument.Uri |> PathUri.ofString

            let locs =
                monad' {
                    let! folder = State.tryFindFolderEnclosing docUri state
                    let! curDoc = Folder.tryFindDocByPath docUri folder
                    let! atPos = Cst.elementAtPos par.Position (Doc.cst curDoc)

                    let referencingEls =
                        Dest.findElementRefs par.Context.IncludeDeclaration folder curDoc atPos

                    let toLoc (doc, el) = { Uri = Doc.uri doc; Range = Element.range el }

                    referencingEls |> Seq.map toLoc |> Array.ofSeq
                }

            let locs = Option.map Array.ofSeq locs

            LspResult.success locs

    override this.TextDocumentSemanticTokensFull(par: SemanticTokensParams) =
        withState
        <| fun state ->
            let docPath = par.TextDocument.Uri |> PathUri.ofString

            let tokens =
                monad' {
                    let! folder = State.tryFindFolderEnclosing docPath state
                    let! doc = Folder.tryFindDocByPath docPath folder
                    let data = Semato.Token.ofIndexEncoded (Doc.index doc)
                    { ResultId = None; Data = data }
                }

            LspResult.success tokens

    override this.TextDocumentSemanticTokensRange(par: SemanticTokensRangeParams) =
        withState
        <| fun state ->
            let docPath = par.TextDocument.Uri |> PathUri.ofString
            let range = par.Range

            let tokens =
                monad' {
                    let! folder = State.tryFindFolderEnclosing docPath state
                    let! doc = Folder.tryFindDocByPath docPath folder
                    let data = Semato.Token.ofIndexEncodedInRange (Doc.index doc) range
                    { ResultId = None; Data = data }
                }

            LspResult.success tokens

    override this.TextDocumentCodeAction(opts: CodeActionParams) =
        withStateExclusive
        <| fun state ->
            let docPath = opts.TextDocument.Uri |> PathUri.ofString

            let codeAction title edit =
                { Title = title
                  Kind = Some CodeActionKind.Source
                  Diagnostics = None
                  Command = None
                  Data = None
                  IsPreferred = Some false
                  Disabled = None
                  Edit = Some edit }

            let tocAction =
                State.tryFindDoc docPath state
                |> Option.bind CodeActions.tableOfContents
                |> Option.toArray
                |> Array.map (fun ca ->
                    let wsEdit =
                        (CodeActions.documentEdit ca.edit ca.newText opts.TextDocument.Uri)

                    codeAction ca.name wsEdit)

            let codeActions: TextDocumentCodeActionResult =
                tocAction |> Array.map U2.Second

            Mutation.output (LspResult.success (Some codeActions))


    override this.TextDocumentRename(pars) =
        withStateExclusive
        <| fun state ->
            let docPath = pars.TextDocument.Uri |> PathUri.ofString

            let edit: option<LspResult<option<WorkspaceEdit>>> =
                monad' {
                    let! folder = State.tryFindFolderEnclosing docPath state
                    let! srcDoc = Folder.tryFindDocByPath docPath folder

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
            let docPath = pars.TextDocument.Uri |> PathUri.ofString

            let renameRange =
                monad' {
                    let! folder = State.tryFindFolderEnclosing docPath state
                    let! srcDoc = Folder.tryFindDocByPath docPath folder
                    return! Refactor.renameRange srcDoc pars.Position
                }

            Mutation.output (renameRange |> Option.map PrepareRenameResult.Range |> Ok)

    override this.Dispose() =
        (statusManager :> IDisposable).Dispose()
        (diagnosticsManager :> IDisposable).Dispose()

        match stateManager with
        | Some stateManager -> (stateManager :> IDisposable).Dispose()
        | _ -> ()
