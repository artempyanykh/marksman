module Marksman.Workspace

open System
open System.IO
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open FSharpPlus.Operators

open Marksman.Parser
open Marksman.Text
open Marksman.Misc
open Marksman.Cst
open Marksman.Index

open Microsoft.FSharp.Core

type Doc =
    { path: PathUri
      rootPath: PathUri
      version: option<int>
      text: Text
      cst: Cst
      index: Index }

module Doc =
    let logger = LogProvider.getLoggerByName "Doc"

    let mk path rootPath version text =
        let cst = parseText text
        let index = Index.ofCst cst

        { path = path
          rootPath = rootPath
          version = version
          text = text
          cst = cst
          index = index }

    let text doc = doc.text

    let withText newText doc =
        let newCst = parseText newText
        let newIndex = Index.ofCst newCst
        { doc with text = newText; cst = newCst; index = newIndex }


    let applyLspChange (change: DidChangeTextDocumentParams) (doc: Doc) : Doc =
        let newVersion = change.TextDocument.Version

        logger.trace (
            Log.setMessage "Processing text change"
            >> Log.addContext "uri" doc.path
            >> Log.addContext "currentVersion" doc.version
            >> Log.addContext "newVersion" newVersion
        )

        // Sanity checking
        match newVersion, doc.version with
        | Some newVersion, Some curVersion when curVersion > 0 ->
            let expectedVersion = curVersion + change.ContentChanges.Length

            if expectedVersion <> newVersion then
                logger.warn (
                    Log.setMessage "Unexpected document version"
                    >> Log.addContext "uri" doc.path
                    >> Log.addContext "currentVersion" curVersion
                    >> Log.addContext "newVersion" newVersion
                )
        | _ -> ()

        let newText = applyTextChange change.ContentChanges doc.text

        { withText newText doc with version = newVersion }

    let fromLsp (root: PathUri) (item: TextDocumentItem) : Doc =
        let path = PathUri.fromString item.Uri
        let text = mkText item.Text

        mk path root (Some item.Version) text

    let tryLoad (root: PathUri) (path: PathUri) : option<Doc> =
        try
            let content =
                using (new StreamReader(path.LocalPath)) (fun f -> f.ReadToEnd())

            let text = mkText content

            Some(mk path root None text)
        with :? FileNotFoundException ->
            None

    let uri (doc: Doc) : DocumentUri = doc.path.DocumentUri

    let rootPath (doc: Doc) : PathUri = doc.rootPath
    let path (doc: Doc) : PathUri = doc.path

    let pathFromRoot (doc: Doc) =
        let docPath = doc.path.LocalPath
        let folderPath = doc.rootPath.LocalPath
        Path.GetRelativePath(folderPath, docPath)

    let title (doc: Doc) : option<Node<Heading>> = Index.title doc.index

    let index (doc: Doc) : Index = doc.index

    let cst (doc: Doc) : Cst = doc.cst

    let name (doc: Doc) : string =
        match title doc with
        | Some { data = hd } -> Heading.name hd
        | None -> pathFromRoot doc |> Path.GetFileNameWithoutExtension

    let slug (doc: Doc) : Slug = name doc |> Slug.ofString

    let headings (doc: Doc) : seq<Node<Heading>> = Index.headings doc.index

    let linkDefs (doc: Doc) : array<Node<MdLinkDef>> = Index.linkDefs doc.index

    let linkDefByLabel (label: string) (doc: Doc) : option<Node<MdLinkDef>> =
        linkDefs doc
        |> Seq.tryFind (fun { data = def } -> def.label.text = label)

    let linkAtPos (pos: Position) (doc: Doc) : option<Element> = Index.linkAtPos pos doc.index

    let version (doc: Doc) : option<int> = doc.version

type Folder =
    | MultiFile of name: string * root: PathUri * docs: Map<PathUri, Doc>
    | SingleFile of Doc

module Folder =
    let private logger = LogProvider.getLoggerByName "Folder"

    let private ignoreFiles = [ ".ignore"; ".gitignore"; ".hgignore" ]

    let singleFile doc = SingleFile doc

    let multiFile name root docs = MultiFile(name, root, docs)

    let docs: Folder -> seq<Doc> =
        function
        | SingleFile doc -> Seq.singleton doc
        | MultiFile (_, _, docs) -> Map.values docs

    let keyPath: Folder -> PathUri =
        function
        | MultiFile (_, root, _) -> root
        | SingleFile doc -> doc.path

    let rootPath: Folder -> PathUri =
        function
        | MultiFile (_, root, _) -> root
        | SingleFile doc -> doc.rootPath

    let tryFindDocByPath (uri: PathUri) : Folder -> option<Doc> =
        function
        | SingleFile doc -> Some doc |> Option.filter (fun x -> x.path = uri)
        | MultiFile (_, _, docs) -> Map.tryFind uri docs

    let private readIgnoreFiles (root: PathUri) : array<string> =
        let lines = ResizeArray()

        for file in ignoreFiles do
            let path = Path.Combine(root.LocalPath, file)

            if File.Exists(path) then
                logger.trace (Log.setMessage "Reading ignore globs" >> Log.addContext "file" path)

                try
                    let content = using (new StreamReader(path)) (fun f -> f.ReadToEnd())
                    lines.AddRange(content.Lines())
                with
                | :? FileNotFoundException
                | :? IOException ->
                    logger.trace (
                        Log.setMessage "Failed to read ignore globs"
                        >> Log.addContext "file" path
                    )

        lines.ToArray()

    let private loadDocs (root: PathUri) : seq<Doc> =

        let rec collect (cur: PathUri) (ignoreFns: list<string -> bool>) =
            let ignores = readIgnoreFiles cur |> buildGlobs
            let ignoreFn = shouldBeIgnored ignores cur.LocalPath
            let ignoreFns = ignoreFn :: ignoreFns

            let di = DirectoryInfo(cur.LocalPath)

            try
                let files = di.GetFiles("*.md")
                let dirs = di.GetDirectories()

                seq {
                    for file in files do
                        if not (shouldBeIgnoredByAny ignoreFns file.FullName) then
                            let pathUri = PathUri.fromString file.FullName

                            let document = Doc.tryLoad root pathUri

                            match document with
                            | Some document -> yield document
                            | _ -> ()
                        else
                            logger.trace (
                                Log.setMessage "Skipping ignored file"
                                >> Log.addContext "file" file.FullName
                            )

                    for dir in dirs do
                        if not (shouldBeIgnoredByAny ignoreFns dir.FullName) then
                            yield! collect (PathUri.fromString dir.FullName) ignoreFns
                        else
                            logger.trace (
                                Log.setMessage "Skipping ignored directory"
                                >> Log.addContext "file" dir.FullName
                            )
                }
            with
            | :? UnauthorizedAccessException as exn ->
                logger.warn (
                    Log.setMessage "Couldn't read the folder"
                    >> Log.addContext "dir" cur
                    >> Log.addException exn
                )

                Seq.empty
            | :? DirectoryNotFoundException as exn ->
                logger.warn (
                    Log.setMessage "The folder doesn't exist"
                    >> Log.addContext "dir" cur
                    >> Log.addException exn
                )

                Seq.empty

        collect root []

    let tryLoad (name: string) (root: PathUri) : option<Folder> =
        logger.trace (Log.setMessage "Loading folder documents" >> Log.addContext "uri" root)

        if Directory.Exists(root.LocalPath) then

            let documents =
                loadDocs root |> Seq.map (fun doc -> doc.path, doc) |> Map.ofSeq

            MultiFile(name = name, root = root, docs = documents) |> Some
        else
            logger.warn (
                Log.setMessage "Folder path doesn't exist"
                >> Log.addContext "uri" root
            )

            None

    let withDoc (newDoc: Doc) : Folder -> Folder =
        function
        | MultiFile (name, root, docs) ->
            if newDoc.rootPath <> root then
                failwith
                    $"Updating a folder with an unrelated doc: folder={root}; doc={newDoc.rootPath}"

            MultiFile(name = name, root = root, docs = Map.add newDoc.path newDoc docs)
        | SingleFile existingDoc ->
            if newDoc.path <> existingDoc.path then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={existingDoc.path}; doc={newDoc.rootPath}"

            SingleFile newDoc

    let withoutDoc (docPath: PathUri) : Folder -> option<Folder> =
        function
        | MultiFile (name, root, docs) ->
            MultiFile(name = name, root = root, docs = Map.remove docPath docs)
            |> Some
        | SingleFile doc ->
            if doc.path <> docPath then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={doc.path}; doc={docPath}"
            else
                None

    let closeDoc (docPath: PathUri) (folder: Folder) : option<Folder> =
        match folder with
        | MultiFile (_, root, _) ->
            match Doc.tryLoad root docPath with
            | Some doc -> withDoc doc folder |> Some
            | _ -> withoutDoc docPath folder
        | SingleFile doc ->
            if doc.path <> docPath then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={doc.path}; doc={docPath}"
            else
                None

    let filterDocsBySlug (slug: Slug) (folder: Folder) : seq<Doc> =
        let isMatchingDoc doc = Doc.slug doc = slug

        docs folder |> Seq.filter isMatchingDoc

    let tryFindDocByUrl (folderRelUrl: string) (folder: Folder) : option<Doc> =
        let urlEncoded = folderRelUrl.AbsPathUrlEncode()

        let isMatchingDoc (doc: Doc) =
            let docUrl = (Doc.pathFromRoot doc).AbsPathUrlEncode()
            docUrl = urlEncoded

        docs folder |> Seq.tryFind isMatchingDoc

    let docCount: Folder -> int =
        function
        | SingleFile _ -> 1
        | MultiFile (_, _, docs) -> docs.Values.Count

type Workspace = { folders: Map<PathUri, Folder> }

module Workspace =
    let ofFolders (folders: seq<Folder>) : Workspace =
        { folders = folders |> Seq.map (fun f -> Folder.keyPath f, f) |> Map.ofSeq }

    let folders (workspace: Workspace) : seq<Folder> =
        seq {
            for KeyValue (_, f) in workspace.folders do
                yield f
        }

    let tryFindFolderByPath (root: PathUri) (workspace: Workspace) : option<Folder> =
        Map.tryFind root workspace.folders

    let tryFindFolderEnclosing (innerPath: PathUri) (workspace: Workspace) : option<Folder> =
        workspace.folders
        |> Map.tryPick (fun root folder ->
            if innerPath.LocalPath.StartsWith(root.LocalPath) then
                Some folder
            else
                None)

    let withoutFolder (root: PathUri) (workspace: Workspace) : Workspace =
        { workspace with folders = Map.remove root workspace.folders }

    let withoutFolders (roots: seq<PathUri>) (workspace: Workspace) : Workspace =
        let newFolders = roots |> Seq.fold (flip Map.remove) workspace.folders

        { workspace with folders = newFolders }

    let withFolder (folder: Folder) (workspace: Workspace) : Workspace =
        { workspace with folders = Map.add (Folder.keyPath folder) folder workspace.folders }

    let withFolders (folders: seq<Folder>) (workspace: Workspace) : Workspace =
        let newFolders =
            folders
            |> Seq.fold (fun fs f -> Map.add (Folder.keyPath f) f fs) workspace.folders

        { workspace with folders = newFolders }

    let docCount (workspace: Workspace) : int =
        workspace.folders.Values |> Seq.sumBy Folder.docCount
