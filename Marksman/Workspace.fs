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
        { doc with cst = newCst; index = newIndex }


    let applyLspChange (change: DidChangeTextDocumentParams) (document: Doc) : Doc =
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
            let expectedVersion = curVersion + change.ContentChanges.Length

            if expectedVersion <> newVersion then
                logger.warn (
                    Log.setMessage "Unexpected document version"
                    >> Log.addContext "uri" document.path
                    >> Log.addContext "currentVersion" curVersion
                    >> Log.addContext "newVersion" newVersion
                )
        | _ -> ()

        let newText = applyTextChange change.ContentChanges document.text

        { withText newText document with version = newVersion }

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
        with
        | :? FileNotFoundException -> None

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


[<RequireQualifiedAccess>]
type LinkTarget =
    | Doc of Doc
    | Heading of Doc * Node<Heading>

module LinkTarget =
    let doc: LinkTarget -> Doc =
        function
        | LinkTarget.Doc doc -> doc
        | LinkTarget.Heading (doc, _) -> doc

    let range: LinkTarget -> Range =
        function
        | LinkTarget.Doc doc ->
            Doc.title doc
            |> Option.map Node.range
            |> Option.defaultWith doc.text.FullRange
        | LinkTarget.Heading (_, heading) -> heading.range

    let scope: LinkTarget -> Range =
        function
        | LinkTarget.Doc doc -> doc.text.FullRange()
        | LinkTarget.Heading (_, heading) -> heading.data.scope


type Folder = { name: string; root: PathUri; docs: Map<PathUri, Doc> }

module Folder =
    let private logger = LogProvider.getLoggerByName "Folder"

    let tryFindDoc (uri: PathUri) (folder: Folder) : option<Doc> = Map.tryFind uri folder.docs

    let docs (folder: Folder) : seq<Doc> = seq { for doc in folder.docs |> Map.values -> doc }

    let rec private loadDocs (root: PathUri) : seq<Doc> =
        let di = DirectoryInfo(root.LocalPath)

        try
            let files = di.GetFiles("*.md")
            let dirs = di.GetDirectories()

            seq {
                for file in files do
                    let pathUri = PathUri.fromString file.FullName

                    let document = Doc.load root pathUri

                    match document with
                    | Some document -> yield document
                    | _ -> ()

                for dir in dirs do
                    yield! loadDocs (PathUri.fromString dir.FullName)
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

    let tryLoad (name: string) (root: PathUri) : option<Folder> =
        logger.trace (Log.setMessage "Loading folder documents" >> Log.addContext "uri" root)

        if Directory.Exists(root.LocalPath) then
            let documents =
                loadDocs root |> Seq.map (fun doc -> doc.path, doc) |> Map.ofSeq

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
        let matchingDoc doc = Doc.slug doc = slug

        folder.docs |> Map.values |> Seq.tryFind matchingDoc

    let tryFindWikiLinkTarget
        (sourceDoc: Doc)
        (wl: WikiLink)
        (folder: Folder)
        : option<LinkTarget> =
        // Discover target doc.
        let destDocName = WikiLink.destDoc wl

        let destDoc =
            match destDocName with
            | None -> Some sourceDoc
            | Some destDocName -> tryFindDocBySlug (Slug.ofString destDocName) folder

        match destDoc with
        | None -> None
        | Some destDoc ->
            // Discover target heading.
            // When target heading is specified but can't be found, the whole thing turns into None.
            match WikiLink.destHeading wl with
            | None ->
                match Doc.title destDoc with
                | Some title -> LinkTarget.Heading(destDoc, title) |> Some
                | None -> LinkTarget.Doc(destDoc) |> Some
            | Some headingName ->
                match Doc.headingBySlug (Slug.ofString headingName) destDoc with
                | Some heading -> LinkTarget.Heading(destDoc, heading) |> Some
                | _ -> None

    let tryFindInlineLinkTarget
        (docUrl: DocUrl)
        (srcDoc: Doc)
        (folder: Folder)
        : option<LinkTarget> =
        let targetDocUrl =
            docUrl.url
            |> Option.map Node.text
            |> Option.defaultWith (fun () -> srcDoc.RelPath.AbsPathUrlEncode())

        let isMatchingDoc (doc: Doc) = doc.RelPath.AbsPathUrlEncode() = targetDocUrl

        let matchingDoc = folder.docs |> Map.values |> Seq.tryFind isMatchingDoc

        let tryMatch (doc: Doc) =
            if doc.RelPath.AbsPathUrlEncode() = targetDocUrl then
                match docUrl.anchor with
                | Some anchor ->
                    match Doc.headingBySlug (Slug.ofString anchor.text) doc with
                    | Some h -> LinkTarget.Heading(doc, h) |> Some
                    | None -> None
                | None -> LinkTarget.Doc doc |> Some
            else
                None

        matchingDoc >>= tryMatch

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
