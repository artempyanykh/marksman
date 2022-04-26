module Marksman.Server

open System
open System.IO
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open LanguageServerProtocol.Logging
open Marksman.Misc
open Marksman.Parser
open Misc
open Text

type Document =
    { path: PathUri
      version: option<int>
      text: Text
      elements: array<Element> }

module Document =
    let logger =
        LogProvider.getLoggerByName "Document"

    let applyChange (change: DidChangeTextDocumentParams) (document: Document) : Document =
        let newVersion = change.TextDocument.Version

        logger.trace (
            Log.setMessage "Processing text change"
            >> Log.addContext "uri" document.path
            >> Log.addContext "currentVersion" document.version
            >> Log.addContext "newVersion" newVersion
        )

        // Sanity checking
        match newVersion, document.version with
        | Some newVersion, Some curVersion ->
            let expectedVersion =
                curVersion + change.ContentChanges.Length

            if expectedVersion <> newVersion then
                logger.warn (
                    Log.setMessage "Unexpected document version"
                    >> Log.addContext "uri" document.path
                    >> Log.addContext "currentVersion" curVersion
                    >> Log.addContext "newVersion" newVersion
                )
        | _ -> ()

        let newText =
            applyTextChange change.ContentChanges document.text

        let newElements = scrapeText newText

        { document with
            version = newVersion
            text = newText
            elements = newElements }

    let fromTextDocument (item: TextDocumentItem) : Document =
        let path = item.Uri |> Uri |> PathUri
        let text = mkText item.Text
        let elements = scrapeText text

        { path = path
          version = Some item.Version
          text = text
          elements = elements }


type Folder =
    { name: string
      root: PathUri
      documents: Map<PathUri, Document> }

module Folder =
    let tryFindDocument (uri: PathUri) (folder: Folder) : option<Document> = Map.tryFind uri folder.documents

type ClientDescription =
    { info: ClientInfo option
      caps: ClientCapabilities }
    member this.IsVSCode: bool =
        this.info
        |> Option.exists (fun x -> x.Name = "Visual Studio Code")

module ClientDescription =
    let fromParams (par: InitializeParams) : ClientDescription =
        let caps =
            par.Capabilities
            |> Option.defaultValue
                { Workspace = None
                  TextDocument = None
                  Experimental = None }

        { info = par.ClientInfo; caps = caps }

type State =
    { client: ClientDescription
      folders: Map<PathUri, Folder>
      revision: int }

module State =
    let logger =
        LogProvider.getLoggerByName "State"

    let tryFindFolder (uri: PathUri) (state: State) : option<Folder> =
        let root =
            state.folders
            |> Map.tryFindKey (fun root _ -> uri.AbsolutePath.StartsWith(root.AbsolutePath))

        root
        |> Option.map (fun root -> state.folders[root])

    let findFolder (uri: PathUri) (state: State) : Folder =
        tryFindFolder uri state
        |> Option.defaultWith (fun _ -> failwith $"Expected folder now found: {uri}")

    let tryFindDocument (uri: PathUri) (state: State) : option<Document> =
        tryFindFolder uri state
        |> Option.map (Folder.tryFindDocument uri)
        |> Option.flatten

    let updateDocument (newDocument: Document) (state: State) : State =
        let folder =
            findFolder newDocument.path state

        let newContent =
            folder.documents
            |> Map.add newDocument.path newDocument

        let newFolder =
            { folder with documents = newContent }

        let newFolders =
            state.folders |> Map.add newFolder.root newFolder

        { state with folders = newFolders }

    let removeDocument (path: PathUri) (state: State) : State =
        let folder = findFolder path state

        let newFolder =
            { folder with documents = Map.remove path folder.documents }

        { state with folders = Map.add folder.root newFolder state.folders }


let extractWorkspaceFolders (par: InitializeParams) : Map<string, PathUri> =
    match par.WorkspaceFolders with
    | Some folders ->
        folders
        |> Array.map (fun { Name = name; Uri = uri } -> name, Uri(uri) |> PathUri)
        |> Map.ofArray
    | _ ->
        let rootPath =
            par.RootUri
            |> Option.orElse par.RootPath
            |> Option.defaultWith (fun () -> failwith $"No folders configured in workspace: {par}")

        let rootUri = Uri(rootPath) |> PathUri

        let rootName =
            Path.GetFileName(rootUri.AbsolutePath)

        Map.ofList [ rootName, rootUri ]

let readDocument (path: PathUri) : option<Document> =
    try
        let content =
            (new StreamReader(path.AbsolutePath)).ReadToEnd()

        let text = mkText content
        let elements = scrapeText text

        Some
            { path = path
              text = text
              elements = elements
              version = None }
    with
    | :? FileNotFoundException -> None


let rec readRoot (root: PathUri) : seq<PathUri * Document> =
    let logger =
        LogProvider.getLoggerByName "readRoot"

    let di = DirectoryInfo(root.AbsolutePath)

    try
        let files = di.GetFiles("*.md")
        let dirs = di.GetDirectories()

        seq {
            for file in files do
                let pathUri = PathUri(Uri(file.FullName))
                let document = readDocument pathUri

                match document with
                | Some document -> yield pathUri, document
                | _ -> ()

            for dir in dirs do
                yield! readRoot (PathUri(Uri(dir.FullName)))
        }
    with
    | :? UnauthorizedAccessException as exn ->
        logger.warn (
            Log.setMessage "Couldn't read the root folder"
            >> Log.addContext "root" root
            >> Log.addException exn
        )

        Seq.empty
    | :? DirectoryNotFoundException as exn ->
        logger.warn (
            Log.setMessage "The root folder doesn't exist"
            >> Log.addContext "root" root
            >> Log.addException exn
        )

        Seq.empty


let readWorkspace (roots: Map<string, PathUri>) : list<Folder> =
    seq {
        for KeyValue (name, root) in roots do
            let content = readRoot root |> Map.ofSeq

            yield
                { name = name
                  root = root
                  documents = content }
    }
    |> List.ofSeq

let mkServerCaps (_pars: InitializeParams) : ServerCapabilities =
    let workspaceFoldersCaps =
        { Supported = Some true
          // TODO
          ChangeNotifications = Some false }

    let markdownFilePattern =
        { Glob = "**/*.md"
          Matches = Some FileOperationPatternKind.File
          Options = Some { FileOperationPatternOptions.Default with IgnoreCase = Some true } }

    let _markdownFileRegistration =
        { Filters =
            [| { Scheme = None
                 Pattern = markdownFilePattern } |] }

    let workspaceFileCaps =
        { WorkspaceFileOperationsServerCapabilities.Default with
            // TODO
            DidCreate = None
            DidDelete = None
            WillRename = None }

    let workspaceCaps =
        { WorkspaceServerCapabilities.Default with
            WorkspaceFolders = Some workspaceFoldersCaps
            FileOperations = Some workspaceFileCaps }

    let textSyncCaps =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Change = Some TextDocumentSyncKind.Incremental }

    { ServerCapabilities.Default with
        Workspace = Some workspaceCaps
        TextDocumentSync = Some textSyncCaps }

type MarksmanClient(_notSender: ClientNotificationSender, _reqSender: ClientRequestSender) =
    inherit LspClient()

type MarksmanServer(_client: MarksmanClient) =
    inherit LspServer()
    let mutable state: option<State> = None

    let logger =
        LogProvider.getLoggerByName "MarksmanServer"

    let updateState (newState: State) : unit =
        logger.trace (Log.setMessage $"Updating state: revision {newState.revision}")

        let newState =
            { newState with revision = newState.revision + 1 }

        state <- Some newState

        logger.trace (Log.setMessage $"Updated state: revision {newState.revision}")

    let requireState () : State =
        Option.defaultWith (fun _ -> failwith "State was not initialized") state

    override this.Initialize(par: InitializeParams) : AsyncLspResult<InitializeResult> =
        let workspaceFolders =
            extractWorkspaceFolders par

        logger.debug (
            Log.setMessage "Obtained workspace folders"
            >> Log.addContext "workspace" workspaceFolders
        )

        let folders = readWorkspace workspaceFolders

        let numNotes =
            folders |> List.sumBy (fun x -> x.documents.Count)

        logger.debug (
            Log.setMessage "Completed reading workspace folders"
            >> Log.addContext "numFolders" folders.Length
            >> Log.addContext "numNotes" numNotes
        )

        let state =
            { client = ClientDescription.fromParams par
              folders =
                folders
                |> List.map (fun x -> x.root, x)
                |> Map.ofList
              revision = 0 }

        updateState state

        let serverCaps = mkServerCaps par

        let initResult =
            { InitializeResult.Default with Capabilities = serverCaps }

        AsyncLspResult.success initResult


    override this.TextDocumentDidChange(par: DidChangeTextDocumentParams) =
        let state = requireState ()

        let docUri =
            par.TextDocument.Uri |> Uri |> PathUri

        let doc = State.tryFindDocument docUri state

        match doc with
        | Some doc ->
            let newDoc = Document.applyChange par doc

            let newState =
                State.updateDocument newDoc state

            updateState newState
        | _ ->
            logger.warn (
                Log.setMessage "Document not found"
                >> Log.addContext "method" "textDocumentDidChange"
                >> Log.addContext "uri" docUri
            )

        async.Return()

    override this.TextDocumentDidClose(par: DidCloseTextDocumentParams) =
        let path =
            par.TextDocument.Uri |> Uri |> PathUri

        let docFromDisk = readDocument path

        let newState =
            match docFromDisk with
            | Some doc -> State.updateDocument doc (requireState ())
            | _ -> State.removeDocument path (requireState ())

        updateState newState
        async.Return()

    override this.TextDocumentDidOpen(par: DidOpenTextDocumentParams) =
        let document =
            Document.fromTextDocument par.TextDocument

        let newState =
            State.updateDocument document (requireState ())

        updateState newState
        async.Return()


    override this.Dispose() = ()
