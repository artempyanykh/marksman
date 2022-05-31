module Marksman.Server

open System.Collections.Generic
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
        HoverProvider = Some true }

let rec headingToSymbolInfo (docUri: PathUri) (h: Node<Heading>) : SymbolInformation [] =
    let name = Heading.name h.data
    let name = $"H{h.data.level}: {name}"
    let kind = SymbolKind.String

    let location = { Uri = docUri.DocumentUri; Range = h.range }

    let sym =
        { Name = name
          Kind = kind
          Location = location
          ContainerName = None }

    let children =
        h.data.children
        |> Element.pickHeadings
        |> Array.collect (headingToSymbolInfo docUri)

    Array.append [| sym |] children

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

type MarksmanStatusParams = { State: string; DocCount: int }

type MarksmanClient(notSender: ClientNotificationSender, _reqSender: ClientRequestSender) =
    inherit LspClient()

    override this.TextDocumentPublishDiagnostics(par: PublishDiagnosticsParams) =
        notSender "textDocument/publishDiagnostics" (box par) |> Async.Ignore

    member this.MarksmanUpdateStatus(par: MarksmanStatusParams) =
        notSender "marksman/status" (box par) |> Async.Ignore

type DiagMessage =
    | Start
    | Stop
    | EnqueueDiagnostic of PublishDiagnosticsParams

type DiagAgent(client: MarksmanClient) =
    let logger = LogProvider.getLoggerByName "BackgroundAgent"

    let agent: MailboxProcessor<DiagMessage> =
        MailboxProcessor.Start (fun inbox ->
            let mutable shouldStart = false
            let mutable shouldStop = false

            let diagQueue: Queue<PublishDiagnosticsParams> = Queue()

            let processDiagQueue () =
                async {
                    if shouldStart && not shouldStop then
                        match diagQueue.TryDequeue() with
                        | false, _ -> () // do nothing, continue processing messages
                        | true, first ->
                            logger.trace (
                                Log.setMessage "Updating document diagnostic"
                                >> Log.addContext "uri" first.Uri
                                >> Log.addContext "numEntries" first.Diagnostics.Length
                            )

                            do! client.TextDocumentPublishDiagnostics(first)
                }

            let rec processMessages () =
                async {
                    do! processDiagQueue ()
                    let! msg = inbox.Receive()

                    match msg with
                    | Start ->
                        logger.trace (Log.setMessage "Starting background agent")
                        shouldStart <- true
                    | Stop ->
                        logger.trace (Log.setMessage "Stopping background agent")
                        shouldStop <- true
                    | EnqueueDiagnostic pars -> diagQueue.Enqueue(pars)

                    do! processDiagQueue ()

                    if not shouldStop then return! processMessages () else ()
                }

            logger.trace (Log.setMessage "Preparing to start background agent")

            processMessages ())

    member this.EnqueueDiagnostic(par: PublishDiagnosticsParams) : unit =
        agent.Post(EnqueueDiagnostic par)

    member this.Start() : unit = agent.Post(Start)
    member this.Stop() : unit = agent.Post(Stop)

type StatusMessage = DocCount of int

type StatusAgent(client: MarksmanClient) =
    let logger = LogProvider.getLoggerByName "StatusAgent"

    let agent =
        MailboxProcessor.Start (fun inbox ->
            let rec loop cnt =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | DocCount newCnt when cnt <> newCnt ->
                        logger.trace (
                            Log.setMessage "StatusAgent sending update"
                            >> Log.addContext "docCount" (cnt, newCnt)
                        )

                        do! client.MarksmanUpdateStatus({ State = "ok"; DocCount = newCnt })
                        return! loop newCnt
                    | _ -> return! loop cnt
                }

            logger.trace (Log.setMessage "StatusAgent starting")
            loop 0)

    member this.UpdateDocCount(cnt: int) : unit = agent.Post(DocCount cnt)


type MarksmanServer(client: MarksmanClient) =
    inherit LspServer()
    let mutable state: option<State> = None

    let diagAgent = DiagAgent(client)

    let mutable statusAgent: option<StatusAgent> = None

    let logger = LogProvider.getLoggerByName "MarksmanServer"

    let requireState () : State =
        Option.defaultWith (fun _ -> failwith "State was not initialized") state

    let queueDiagUpdate (existingDiag: WorkspaceDiag) (newDiag: WorkspaceDiag) : unit =
        let state = requireState ()

        for folder in State.workspace state |> Workspace.folders do
            let newFolderDiag =
                Map.tryFind folder.root newDiag |> Option.defaultValue [||]

            let existingFolderDiag =
                Map.tryFind folder.root existingDiag |> Option.defaultValue [||]

            for docUri, docDiag in newFolderDiag do
                let existingDocDiag =
                    existingFolderDiag
                    |> Array.tryFind (fun (uri, _) -> uri = docUri)
                    |> Option.map snd
                    |> Option.defaultValue [||]

                if docDiag <> existingDocDiag then
                    logger.trace (
                        Log.setMessage "Diagnostic changed, queueing the update"
                        >> Log.addContext "doc" docUri
                    )

                    let publishParams = { Uri = docUri.DocumentUri; Diagnostics = docDiag }

                    diagAgent.EnqueueDiagnostic(publishParams)

    let updateState (newState: State) : unit =
        let curState = state

        logger.trace (
            Log.setMessage "Updating state"
            >> Log.addContext "curRev" (curState |> Option.map State.revision)
        )

        let existingWorkspaceDiag =
            curState |> Option.map State.diag |> Option.defaultValue Map.empty

        let newWorkspaceDiag = State.diag newState

        let docCount = State.workspace newState |> Workspace.docCount

        statusAgent |> Option.iter (fun x -> x.UpdateDocCount(docCount))

        state <- Some newState

        logger.trace (
            Log.setMessage "Updated state"
            >> Log.addContext "newRev" (State.revision newState)
        )

        queueDiagUpdate existingWorkspaceDiag newWorkspaceDiag

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

        let state = State.mk clientDesc (Workspace.ofFolders folders)

        updateState state

        let serverCaps = mkServerCaps par

        let initResult =
            { InitializeResult.Default with Capabilities = serverCaps }

        AsyncLspResult.success initResult


    override this.Initialized(_: InitializedParams) =
        let state = requireState ()

        if (State.client state).SupportsStatus then
            logger.debug (
                Log.setMessage "Client supports status notifications. Initializing agent."
            )

            statusAgent <- StatusAgent(client) |> Some

            statusAgent
            |> Option.iter (fun x -> x.UpdateDocCount(State.workspace state |> Workspace.docCount))
        else
            logger.debug (
                Log.setMessage
                    "Client doesn't support status notifications. Agent won't be initialized."
            )

        diagAgent.Start()

        async.Return()

    override this.Shutdown() =
        logger.trace (Log.setMessage "Preparing for shutdown")
        diagAgent.Stop()
        async.Return()

    override this.Exit() =
        logger.trace (Log.setMessage "Exiting")
        async.Return()

    override this.TextDocumentDidChange(par: DidChangeTextDocumentParams) =
        let state = requireState ()

        let docUri = par.TextDocument.Uri |> PathUri.fromString

        let doc = State.tryFindDocument docUri state

        match doc with
        | Some doc ->
            let newDoc = Doc.applyLspChange par doc

            let newState = State.updateDocument newDoc state

            updateState newState
        | _ ->
            logger.warn (
                Log.setMessage "Document not found"
                >> Log.addContext "method" "textDocumentDidChange"
                >> Log.addContext "uri" docUri
            )

        async.Return()

    override this.TextDocumentDidClose(par: DidCloseTextDocumentParams) =
        let path = par.TextDocument.Uri |> PathUri.fromString

        let state = requireState ()
        let folder = State.tryFindFolderEnclosing path state

        match folder with
        | None -> ()
        | Some folder ->
            let docFromDisk = Doc.load folder.root path

            let newState =
                match docFromDisk with
                | Some doc -> State.updateDocument doc (requireState ())
                | _ -> State.removeDocument path (requireState ())

            updateState newState

        async.Return()

    override this.TextDocumentDidOpen(par: DidOpenTextDocumentParams) =
        let state = requireState ()

        let path = par.TextDocument.Uri |> PathUri.fromString

        let folder = State.tryFindFolderEnclosing path state

        match folder with
        | None -> ()
        | Some folder ->
            let document = Doc.fromLspDocument folder.root par.TextDocument

            let newState = State.updateDocument document (requireState ())

            updateState newState

        async.Return()

    override this.WorkspaceDidChangeWorkspaceFolders(par: DidChangeWorkspaceFoldersParams) =
        let state = requireState ()

        let newState =
            State.updateFoldersFromLsp par.Event.Added par.Event.Removed state

        updateState newState
        async.Return()


    override this.WorkspaceDidCreateFiles(par: CreateFilesParams) =
        let docUris = par.Files |> Array.map (fun fc -> PathUri.fromString fc.Uri)

        let mutable newState = requireState ()

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

        updateState newState
        async.Return()

    override this.WorkspaceDidDeleteFiles(par: DeleteFilesParams) =
        let mutable newState = requireState ()

        let deletedUris =
            par.Files |> Array.map (fun x -> PathUri.fromString x.Uri)

        for uri in deletedUris do
            logger.trace (
                Log.setMessage "Processing file delete not"
                >> Log.addContext "uri" uri
            )

            newState <- State.removeDocument uri newState

        updateState newState
        async.Return()

    override this.TextDocumentDocumentSymbol(par: DocumentSymbolParams) =
        let state = requireState ()

        let docUri = par.TextDocument.Uri |> PathUri.fromString

        let getSymbols (doc: Doc) =
            let headings = Index.headings doc.Index

            if (State.client state).SupportsHierarchy then
                headings
                |> Seq.map (headingToDocumentSymbol (State.client state).IsEmacs)
                |> Array.ofSeq
                |> Second
            else
                headings
                |> Seq.collect (headingToSymbolInfo docUri)
                |> Array.ofSeq
                |> First

        let response = State.tryFindDocument docUri state |> Option.map getSymbols

        AsyncLspResult.success response

    override this.TextDocumentCompletion(par: CompletionParams) =
        logger.trace (Log.setMessage "Completion request start")

        let state = requireState ()

        let pos = par.Position
        let docUri = par.TextDocument.Uri |> PathUri.fromString

        let candidates =
            monad' {
                let! folder = State.tryFindFolderEnclosing docUri state

                match Comp.findCandidates pos docUri folder with
                | [||] -> return! None
                | candidates -> { IsIncomplete = true; Items = candidates }
            }

        AsyncLspResult.success candidates

    override this.TextDocumentDefinition(par: TextDocumentPositionParams) =
        let state = requireState ()

        let docUri = par.TextDocument.Uri |> PathUri.fromString

        let goto =
            monad {
                let! folder = State.tryFindFolderEnclosing docUri state
                let! srcDoc = Folder.tryFindDoc docUri folder
                let! atPos = Doc.linkAtPos par.Position srcDoc

                match atPos with
                | WL { data = wl } ->
                    logger.trace (
                        Log.setMessage "Searching definition of a wiki-link"
                        >> Log.addContext "wiki" (WikiLink.fmt wl)
                    )

                    let! linkTarget = Folder.tryFindWikiLinkTarget srcDoc wl folder

                    let destRange = LinkTarget.range linkTarget
                    let destDoc = LinkTarget.doc linkTarget

                    let location =
                        GotoResult.Single { Uri = destDoc.path.DocumentUri; Range = destRange }

                    location
                | ML { data = MdLink.IL (_, url, _) as link } ->
                    logger.trace (
                        Log.setMessage "Searching definition of an inline link"
                        >> Log.addContext "link" (MdLink.fmt link)
                    )

                    let! docUrl = url |> Option.map DocUrl.ofUrlNode

                    let! linkTarget = Folder.tryFindInlineLinkTarget docUrl srcDoc folder
                    let targetUri = (LinkTarget.doc linkTarget).path.DocumentUri
                    let targetRange = LinkTarget.range linkTarget

                    let location = GotoResult.Single { Uri = targetUri; Range = targetRange }

                    location
                | ML { data = link } ->
                    logger.trace (
                        Log.setMessage "Searching definition of a link reference"
                        >> Log.addContext "ref" (MdLink.fmt link)
                    )

                    let! label = MdLink.referenceLabel link
                    let! def = Doc.linkDefByLabel label.text srcDoc

                    let location =
                        GotoResult.Single { Uri = srcDoc.path.DocumentUri; Range = def.range }

                    location
                | _ -> return! None
            }

        AsyncLspResult.success goto

    override this.TextDocumentHover(par: TextDocumentPositionParams) =
        let state = requireState ()

        let docUri = par.TextDocument.Uri |> PathUri.fromString

        let hover =
            monad {
                let! folder = State.tryFindFolderEnclosing docUri state
                let! srcDoc = Folder.tryFindDoc docUri folder
                let! atPos = Doc.linkAtPos par.Position srcDoc

                let! linkTarget =
                    match atPos with
                    | WL wl -> Folder.tryFindWikiLinkTarget srcDoc wl.data folder
                    | ML { data = MdLink.IL (_, url, _) } ->
                        url
                        |> Option.map DocUrl.ofUrlNode
                        |> Option.bind (fun docUrl ->
                            Folder.tryFindInlineLinkTarget docUrl srcDoc folder)
                    | _ -> None

                let destScope = LinkTarget.scope linkTarget

                let content =
                    (LinkTarget.doc linkTarget).text.Substring destScope
                    |> markdown
                    |> MarkupContent

                let hover = { Contents = content; Range = None }

                hover
            }

        AsyncLspResult.success hover

    override this.Dispose() = ()
