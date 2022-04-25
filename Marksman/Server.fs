module Marksman.Server

open System
open System.IO
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open LanguageServerProtocol.Logging
open Misc

type FolderData =
    { name: string
      root: PathUri
      content: Map<PathUri, string> }

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

let rec readRoot (root: PathUri) : seq<PathUri * string> =
    let logger =
        LogProvider.getLoggerByName "readRoot"

    let di = DirectoryInfo(root.AbsolutePath)

    try
        let files = di.GetFiles("*.md")
        let dirs = di.GetDirectories()

        seq {
            for file in files do
                let content =
                    (new StreamReader(file.FullName)).ReadToEnd()

                yield PathUri(Uri(file.FullName)), content

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


let readWorkspace (roots: Map<string, PathUri>) : list<FolderData> =
    seq {
        for KeyValue (name, root) in roots do
            let content = readRoot root |> Map.ofSeq

            yield
                { name = name
                  root = root
                  content = content }
    }
    |> List.ofSeq

let mkServerCaps (_pars: InitializeParams) : ServerCapabilities =
    let workspaceFoldersCaps =
        { Supported = Some true
          ChangeNotifications = Some true }

    let markdownFilePattern =
        { Glob = "**/*.md"
          Matches = Some FileOperationPatternKind.File
          Options = Some { FileOperationPatternOptions.Default with IgnoreCase = Some true } }

    let markdownFileRegistration =
        { Filters =
            [| { Scheme = None
                 Pattern = markdownFilePattern } |] }

    let workspaceFileCaps =
        { WorkspaceFileOperationsServerCapabilities.Default with
            DidCreate = Some markdownFileRegistration
            DidDelete = Some markdownFileRegistration
            WillRename = Some markdownFileRegistration }

    let workspaceCaps =
        { WorkspaceServerCapabilities.Default with
            WorkspaceFolders = Some workspaceFoldersCaps
            FileOperations = Some workspaceFileCaps }

    { ServerCapabilities.Default with Workspace = Some workspaceCaps }

type MarksmanClient(_notSender: ClientNotificationSender, _reqSender: ClientRequestSender) =
    inherit LspClient()

type MarksmanServer(_client: MarksmanClient) =
    inherit LspServer()

    let logger =
        LogProvider.getLoggerByName "MarksmanServer"

    override this.Initialize(pars: InitializeParams) : AsyncLspResult<InitializeResult> =
        let workspaceFolders =
            extractWorkspaceFolders pars

        logger.debug (
            Log.setMessage "Obtained workspace folders"
            >> Log.addContext "workspace" workspaceFolders
        )

        let folderDatas =
            readWorkspace workspaceFolders

        let numNotes =
            folderDatas
            |> List.sumBy (fun x -> x.content.Count)

        logger.debug (
            Log.setMessage "Completed reading workspace folders"
            >> Log.addContext "numFolders" folderDatas.Length
            >> Log.addContext "numNotes" numNotes
        )

        let serverCaps = mkServerCaps pars

        let initResult =
            { InitializeResult.Default with Capabilities = serverCaps }

        AsyncLspResult.success initResult

    override this.Dispose() = ()
