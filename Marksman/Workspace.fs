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

open Microsoft.Extensions.FileSystemGlobbing
open Microsoft.FSharp.Core

type Doc =
    { path: PathUri
      rootPath: PathUri
      version: option<int>
      text: Text
      cst: Cst
      index: Index }

    member this.RelPath: string =
        let docPath = this.path.LocalPath
        let folderPath = this.rootPath.LocalPath

        Path.GetRelativePath(folderPath, docPath)

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

    let fromLspDocument (root: PathUri) (item: TextDocumentItem) : Doc =
        let path = PathUri.fromString item.Uri
        let text = mkText item.Text

        mk path root (Some item.Version) text

    let load (root: PathUri) (path: PathUri) : option<Doc> =
        try
            let content =
                using (new StreamReader(path.LocalPath)) (fun f -> f.ReadToEnd())

            let text = mkText content

            Some(mk path root None text)
        with :? FileNotFoundException ->
            None

    let title (doc: Doc) : option<Node<Heading>> = Index.title doc.index

    let index (doc: Doc) : Index = doc.index

    let name (doc: Doc) : string =
        match title doc with
        | Some { data = hd } -> Heading.name hd
        | None -> doc.RelPath |> Path.GetFileNameWithoutExtension

    let slug (doc: Doc) : Slug = name doc |> Slug.ofString

    let headings (doc: Doc) : seq<Node<Heading>> = Index.headings doc.index

    let headingBySlug (nameSlug: Slug) (document: Doc) : option<Node<Heading>> =
        document.index |> Index.tryFindHeadingBySlug nameSlug

    let linkDefs (doc: Doc) : array<Node<MdLinkDef>> = Index.linkDefs doc.index

    let linkDefByLabel (label: string) (doc: Doc) : option<Node<MdLinkDef>> =
        linkDefs doc
        |> Seq.tryFind (fun { data = def } -> def.label.text = label)

    let linkAtPos (pos: Position) (doc: Doc) : option<Element> = Index.linkAtPos pos doc.index

    let version (doc: Doc) : option<int> = doc.version

type Folder = { name: string; root: PathUri; docs: Map<PathUri, Doc> }

module Folder =
    let private logger = LogProvider.getLoggerByName "Folder"
    let private ignoreFiles = [ ".ignore"; ".gitignore"; ".hgignore" ]

    let tryFindDocByPath (uri: PathUri) (folder: Folder) : option<Doc> = Map.tryFind uri folder.docs

    let docs (folder: Folder) : seq<Doc> = seq { for doc in folder.docs |> Map.values -> doc }

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

    let buildGlobs (patterns: array<string>) : Matcher =
        let matcher = Matcher().AddInclude("**")

        matcher.AddExcludePatterns([| ".git"; ".hg" |])
        matcher.AddExcludePatterns(patterns)
        matcher

    let shouldBeIgnored (ignores: Matcher) (root: string) (fullFilePath: string) : bool =
        // let relPath = fullFilePath.TrimPrefix(root).AsUnixAbsPath()
        let shouldIgnore = ignores.Match(root, fullFilePath).HasMatches |> not
        shouldIgnore

    let private loadDocs (root: PathUri) (ignores: Matcher) : seq<Doc> =
        let shouldBeIgnoredHere = shouldBeIgnored ignores root.LocalPath

        let rec collect (cur: PathUri) =
            let di = DirectoryInfo(cur.LocalPath)

            try
                let files = di.GetFiles("*.md")
                let dirs = di.GetDirectories()

                seq {
                    for file in files do
                        if not (shouldBeIgnoredHere file.FullName) then
                            let pathUri = PathUri.fromString file.FullName

                            let document = Doc.load root pathUri

                            match document with
                            | Some document -> yield document
                            | _ -> ()
                        else
                            logger.trace (
                                Log.setMessage "Skipping ignored file"
                                >> Log.addContext "file" file.FullName
                            )

                    for dir in dirs do
                        if not (shouldBeIgnoredHere dir.FullName) then
                            yield! collect (PathUri.fromString dir.FullName)
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

        collect root

    let tryLoad (name: string) (root: PathUri) : option<Folder> =
        logger.trace (Log.setMessage "Loading folder documents" >> Log.addContext "uri" root)

        if Directory.Exists(root.LocalPath) then
            let ignores = readIgnoreFiles root |> buildGlobs

            let documents =
                loadDocs root ignores
                |> Seq.map (fun doc -> doc.path, doc)
                |> Map.ofSeq

            { name = name; root = root; docs = documents } |> Some
        else
            logger.warn (
                Log.setMessage "Folder path doesn't exist"
                >> Log.addContext "uri" root
            )

            None

    let loadDoc (uri: PathUri) (folder: Folder) : Folder =
        match Doc.load folder.root uri with
        | Some doc -> { folder with docs = Map.add uri doc folder.docs }
        | None -> folder

    let removeDoc (uri: PathUri) (folder: Folder) : Folder =
        { folder with docs = Map.remove uri folder.docs }

    let addDoc (doc: Doc) (folder: Folder) : Folder =
        { folder with docs = Map.add doc.path doc folder.docs }


    let tryFindDocBySlug (slug: Slug) (folder: Folder) : option<Doc> =
        let isMatchingDoc doc = Doc.slug doc = slug

        folder.docs |> Map.values |> Seq.tryFind isMatchingDoc

    let tryFindDocByUrl (folderRelUrl: string) (folder: Folder) : option<Doc> =
        let urlEncoded = folderRelUrl.AbsPathUrlEncode()

        let isMatchingDoc (doc: Doc) =
            let docUrl = doc.RelPath.AbsPathUrlEncode()
            docUrl = urlEncoded

        folder.docs |> Map.values |> Seq.tryFind isMatchingDoc

    let docCount (folder: Folder) : int = folder.docs.Values.Count

type Workspace = { folders: Map<PathUri, Folder> }

module Workspace =
    let ofFolders (folders: seq<Folder>) : Workspace =
        { folders = folders |> Seq.map (fun f -> f.root, f) |> Map.ofSeq }

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
        { workspace with folders = Map.add folder.root folder workspace.folders }

    let withFolders (folders: seq<Folder>) (workspace: Workspace) : Workspace =
        let newFolders =
            folders
            |> Seq.fold (fun fs f -> Map.add f.root f fs) workspace.folders

        { workspace with folders = newFolders }

    let docCount (workspace: Workspace) : int =
        workspace.folders.Values |> Seq.sumBy Folder.docCount
