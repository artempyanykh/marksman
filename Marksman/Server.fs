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
open Marksman.Toc
open Marksman.Refs

let extractWorkspaceFolders (par: InitializeParams) : Map<string, PathUri> =
    match par.WorkspaceFolders with
    | Some folders ->
        folders
        |> Array.map (fun { Name = name; Uri = uri } -> name, PathUri.fromString uri)
        |> Map.ofArray
    | _ ->
        let rootPath =
            par.RootUri
            |> Option.orElse par.RootPath
            |> Option.defaultWith (fun () -> failwith $"No folders configured in workspace: {par}")

        let rootUri = PathUri.fromString rootPath

        let rootName = Path.GetFileName(rootUri.LocalPath)

        Map.ofList [ rootName, rootUri ]

let readWorkspace (roots: Map<string, PathUri>) : list<Folder> =
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

let headingToSymbolInfo (docUri: PathUri) (h: Node<Heading>) : SymbolInformation =
    let name = Heading.name h.data
    let name = $"H{h.data.level}: {name}"
    let kind = SymbolKind.String

    let location = { Uri = docUri.DocumentUri; Range = h.range }

    let sym =
        { Name = name
          Kind = kind
          Location = location
          ContainerName = None }

    sym

let rec headingToDocumentSymbol (isEmacs: bool) (h: Node<Heading>) : DocumentSymbol =
    let name = Heading.name h.data
    let kind = SymbolKind.String
    let range = h.data.scope
    let selectionRange = h.range

    let children =
        h.data.children
        |> Element.pickHeadings
        |> Array.map (headingToDocumentSymbol isEmacs)

    let children =
        if Array.isEmpty children then
            None
        else if isEmacs then
            // Emacs' imenu with consult/counsel/etc. doesn't allow selecting intermediate
            // nodes that have children. As a workaround we add a '.' this node.
            let thisHeading =
                { Name = "."
                  Detail = None
                  Kind = kind
                  Range = selectionRange
                  SelectionRange = selectionRange
                  Children = None }

            Some(Array.append [| thisHeading |] children)
        else
            Some children

    { Name = name
      Detail = None
      Kind = kind
      Range = range
      SelectionRange = selectionRange
      Children = children }

type MarksmanStatusParams = { state: string; docCount: int }

type MarksmanClient(notiSender: ClientNotificationSender, _reqSender: ClientRequestSender) =
    inherit LspClient()

    override this.TextDocumentPublishDiagnostics(par: PublishDiagnosticsParams) =
        notiSender "textDocument/publishDiagnostics" (box par) |> Async.Ignore

    member this.MarksmanUpdateStatus(par: MarksmanStatusParams) =
        notiSender "marksman/status" (box par) |> Async.Ignore

type DiagnosticsMessage = PublishDiagnostics of PublishDiagnosticsParams

type DiagnosticsManager(client: MarksmanClient) =
    let logger = LogProvider.getLoggerByName "BackgroundAgent"

    let agent: MailboxProcessor<DiagnosticsMessage> =
        MailboxProcessor.Start(fun inbox ->
            let rec processMessages () =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | PublishDiagnostics pars ->
                        logger.trace (
                            Log.setMessage "Updating document diagnostic"
                            >> Log.addContext "uri" pars.Uri
                            >> Log.addContext "numEntries" pars.Diagnostics.Length
                        )

                        do! client.TextDocumentPublishDiagnostics(pars)

                    return! processMessages ()
                }

            logger.trace (Log.setMessage "Preparing to start background agent")

            processMessages ())

    member this.UpdateDiagnostics(par: PublishDiagnosticsParams) : unit =
        agent.Post(PublishDiagnostics par)

    interface IDisposable with
        member _.Dispose() = (agent :> IDisposable).Dispose()

let queueDiagnosticsUpdate
    (manager: DiagnosticsManager)
    (prevState: Option<State>)
    (newState: State)
    : unit =
    let existingDiag =
        prevState
        |> Option.map (fun x -> x.Diag)
        |> Option.defaultValue WorkspaceDiag.empty

    let newDiag = newState.Diag

    let allFolders =
        Set.union (Map.keys existingDiag |> Set.ofSeq) (Map.keys newDiag |> Set.ofSeq)

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
            let existingDocDiag =
                existingFolderDiag
                |> Array.tryFind (fun (uri, _) -> uri = docUri)
                |> Option.map snd
                |> Option.defaultValue [||]

            let newDocDiag =
                newFolderDiag
                |> Array.tryFind (fun (uri, _) -> uri = docUri)
                |> Option.map snd
                |> Option.defaultValue [||]

            if newDocDiag <> existingDocDiag then
                logger.trace (
                    Log.setMessage "Diagnostic changed, queueing the update"
                    >> Log.addContext "doc" docUri
                )

                let publishParams = { Uri = docUri.DocumentUri; Diagnostics = newDocDiag }

                manager.UpdateDiagnostics(publishParams)

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

        let numNotes = folders |> List.sumBy (fun x -> x.docs.Count)

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

        AsyncLspResult.success initResult


    override this.Initialized(_: InitializedParams) =
        withStateExclusive
        <| fun state ->
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
            let docUri = par.TextDocument.Uri |> PathUri.fromString

            let doc = State.tryFindDocument docUri state

            let newState =
                match doc with
                | Some doc ->
                    let newDoc = Doc.applyLspChange par doc

                    State.updateDocument newDoc state |> Some
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
            let path = par.TextDocument.Uri |> PathUri.fromString
            let folder = State.tryFindFolderEnclosing path state

            match folder with
            | None -> Mutation.empty
            | Some folder ->
                let docFromDisk = Doc.load folder.root path

                let newState =
                    match docFromDisk with
                    | Some doc -> State.updateDocument doc state
                    | _ -> State.removeDocument path state

                Mutation.state newState


    override this.TextDocumentDidOpen(par: DidOpenTextDocumentParams) =
        withStateExclusive
        <| fun state ->
            let path = par.TextDocument.Uri |> PathUri.fromString

            let folder = State.tryFindFolderEnclosing path state

            match folder with
            | None -> Mutation.empty
            | Some folder ->
                let document = Doc.fromLspDocument folder.root par.TextDocument
                let newState = State.updateDocument document state
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
            let docUris = par.Files |> Array.map (fun fc -> PathUri.fromString fc.Uri)

            let mutable newState = state

            for docUri in docUris do
                logger.trace (
                    Log.setMessage "Processing file create not"
                    >> Log.addContext "uri" docUri
                )

                let folder = State.tryFindFolderEnclosing docUri newState

                match folder with
                | None -> ()
                | Some folder ->
                    match Doc.load folder.root docUri with
                    | Some doc -> newState <- State.updateDocument doc newState
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

            let deletedUris =
                par.Files |> Array.map (fun x -> PathUri.fromString x.Uri)

            for uri in deletedUris do
                logger.trace (
                    Log.setMessage "Processing file delete not"
                    >> Log.addContext "uri" uri
                )

                newState <- State.removeDocument uri newState

            Mutation.state newState

    override this.TextDocumentDocumentSymbol(par: DocumentSymbolParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> PathUri.fromString

            let getSymbols (doc: Doc) =

                if (State.client state).SupportsHierarchy then
                    let topLevelHeadings =
                        doc.cst |> Seq.collect (Element.asHeading >> Option.toList)

                    topLevelHeadings
                    |> Seq.map (headingToDocumentSymbol (State.client state).IsEmacs)
                    |> Array.ofSeq
                    |> Second
                else
                    let allHeadings = Index.headings doc.index

                    allHeadings
                    |> Seq.map (headingToSymbolInfo docUri)
                    |> Array.ofSeq
                    |> First

            let response = State.tryFindDocument docUri state |> Option.map getSymbols

            LspResult.success response

    override this.TextDocumentCompletion(par: CompletionParams) =
        withState
        <| fun state ->
            logger.trace (Log.setMessage "Completion request start")

            let pos = par.Position
            let docUri = par.TextDocument.Uri |> PathUri.fromString

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
            let docUri = par.TextDocument.Uri |> PathUri.fromString

            let goto =
                monad {
                    let! folder = State.tryFindFolderEnclosing docUri state
                    let! srcDoc = Folder.tryFindDocByPath docUri folder
                    let! atPos = Doc.linkAtPos par.Position srcDoc
                    let! uref = Uref.ofElement atPos
                    let! ref = Ref.tryResolveUref uref srcDoc folder

                    GotoResult.Single
                        { Uri = (Ref.doc ref).path.DocumentUri; Range = (Ref.range ref) }
                }

            LspResult.success goto

    override this.TextDocumentHover(par: TextDocumentPositionParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> PathUri.fromString

            let hover =
                monad {
                    let! folder = State.tryFindFolderEnclosing docUri state
                    let! srcDoc = Folder.tryFindDocByPath docUri folder
                    let! atPos = Doc.linkAtPos par.Position srcDoc
                    let! uref = Uref.ofElement atPos
                    let! ref = Ref.tryResolveUref uref srcDoc folder

                    let destScope = Ref.scope ref

                    let content =
                        (Ref.doc ref).text.Substring destScope |> markdown |> MarkupContent

                    let hover = { Contents = content; Range = None }

                    hover
                }

            LspResult.success hover



    override this.TextDocumentReferences(par: ReferenceParams) =
        withState
        <| fun state ->
            let docUri = par.TextDocument.Uri |> PathUri.fromString

            let locs =
                monad' {
                    let! folder = State.tryFindFolderEnclosing docUri state
                    let! curDoc = Folder.tryFindDocByPath docUri folder
                    let! atPos = Cst.elementAtPos par.Position curDoc.cst

                    let referencingEls =
                        Ref.findElementRefs par.Context.IncludeDeclaration folder curDoc atPos

                    let toLoc (doc, el) = { Uri = doc.path.DocumentUri; Range = Element.range el }

                    referencingEls |> Seq.map toLoc |> Array.ofSeq
                }

            let locs = Option.map Array.ofSeq locs

            LspResult.success locs

    override this.TextDocumentSemanticTokensFull(par: SemanticTokensParams) =
        withState
        <| fun state ->
            let docPath = par.TextDocument.Uri |> PathUri.fromString

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
            let docPath = par.TextDocument.Uri |> PathUri.fromString
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
            let docPath = opts.TextDocument.Uri |> PathUri.fromString

            let documentBeginning = Range.Mk(0, 0, 0, 0)

            let editAt rangeOpt text =
                let textEdit =
                    { NewText = text
                      Range = Option.defaultValue documentBeginning rangeOpt }

                let mp = Map.ofList [ opts.TextDocument.Uri, [| textEdit |] ]

                { Changes = Some mp; DocumentChanges = None }

            let toc =
                monad' {
                    let! document = State.tryFindDocument docPath state
                    let! toc = TableOfContents.mk document.index

                    let rendered = TableOfContents.render toc
                    let detected = TableOfContents.detect document.text

                    let result = ($"{rendered}", detected)

                    result
                }

            let codeAction title edit =
                { Title = title
                  Kind = Some CodeActionKind.Source
                  Diagnostics = None
                  Command = None
                  Data = None
                  IsPreferred = Some false
                  Disabled = None
                  Edit = Some edit }

            let codeActions =
                match toc with
                | None -> Array.empty
                | Some (render, existing) ->
                    match existing with
                    | None ->
                        [| U2.Second(codeAction "Create a Table of Contents" (editAt None render)) |]
                    | Some oldTocRange ->
                        [| U2.Second(
                               codeAction
                                   "Update the Table of Contents"
                                   (editAt (Some oldTocRange) render)
                           ) |]

            Mutation.output (LspResult.success (Some codeActions))


    override this.TextDocumentRename(pars) =
        withStateExclusive
        <| fun state ->
            let docPath = pars.TextDocument.Uri |> PathUri.fromString

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
            let docPath = pars.TextDocument.Uri |> PathUri.fromString

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
