module Marksman.Domain

open System
open System.IO
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open FSharpPlus.Operators

open Marksman.Parser
open Marksman.Text
open Marksman.Misc

type Doc =
    { path: PathUri
      relPath: string
      version: option<int>
      text: Text
      elements: array<Element> }

module Doc =
    let logger = LogProvider.getLoggerByName "Doc"

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

        let newElements = parseText newText

        { document with
            version = newVersion
            text = newText
            elements = newElements }

    let pathFromFolder (folderPath: PathUri) (docPath: PathUri) : string =
        let docPath = docPath.LocalPath
        let folderPath = folderPath.LocalPath

        Path.GetRelativePath(folderPath, docPath)

    let fromLspDocument (root: PathUri) (item: TextDocumentItem) : Doc =
        let path = PathUri.fromString item.Uri
        let text = mkText item.Text
        let elements = parseText text

        { path = path
          relPath = pathFromFolder root path
          version = Some item.Version
          text = text
          elements = elements }


    let load (root: PathUri) (path: PathUri) : option<Doc> =
        try
            let content =
                using (new StreamReader(path.LocalPath)) (fun f -> f.ReadToEnd())

            let text = mkText content
            let elements = parseText text

            Some
                { path = path
                  relPath = pathFromFolder root path
                  text = text
                  elements = elements
                  version = None }
        with
        | :? FileNotFoundException -> None

    let title (doc: Doc) : option<Node<Heading>> =
        let isTitle el =
            Element.asHeading el
            |> Option.map (fun x -> x.data.level = 1)
            |> Option.defaultValue false

        let titleOpt =
            doc.elements
            |> Array.tryFind isTitle
            |> Option.map (function
                | H h -> h
                | other -> failwith $"Expected heading: {other}")

        titleOpt

    let name (doc: Doc) : string =
        match title doc with
        | Some { data = hd } -> Heading.name hd
        | None -> doc.relPath |> Path.GetFileNameWithoutExtension

    let slug (doc: Doc) : Slug = name doc |> Slug.ofString

    let elementsAll (document: Doc) : seq<Element> =
        let rec collect els =
            seq {
                for el in els do
                    yield el

                    match el with
                    | H h -> yield! collect h.data.children
                    | WL _
                    | ML _
                    | MLD _ -> ()
            }

        collect document.elements

    let headings (document: Doc) : seq<Node<Heading>> =
        seq {
            for el in elementsAll document do
                match Element.asHeading el with
                | Some h -> yield h
                | _ -> ()
        }

    let headingBySlug (nameSlug: Slug) (document: Doc) : option<Node<Heading>> =
        let matchingHeading { data = h } = Heading.slug h = nameSlug

        headings document |> Seq.tryFind matchingHeading

    let linkDefs (doc: Doc) : seq<Node<MdLinkDef>> =
        seq {
            for el in elementsAll doc do
                match Element.asLinkDef el with
                | Some def -> yield def
                | _ -> ()
        }

    let linkDefByLabel (label: string) (doc: Doc) : option<Node<MdLinkDef>> =
        linkDefs doc
        |> Seq.tryFind (fun { data = def } -> def.label.text = label)

    let linkAtPos (pos: Position) (doc: Doc) : option<Element> =
        elementsAll doc
        |> Seq.filter Element.isLink
        |> Seq.tryFind (fun el ->
            let range = Element.range el
            range.Start <= pos && pos < range.End)


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

    let tryFindDocument (uri: PathUri) (folder: Folder) : option<Doc> = Map.tryFind uri folder.docs

    let rec private loadDocuments (root: PathUri) : seq<Doc> =
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
                    yield! loadDocuments (PathUri.fromString dir.FullName)
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
                loadDocuments root |> Seq.map (fun doc -> doc.path, doc) |> Map.ofSeq

            { name = name; root = root; docs = documents } |> Some
        else
            logger.warn (
                Log.setMessage "Folder path doesn't exist"
                >> Log.addContext "uri" root
            )

            None

    let loadDocument (uri: PathUri) (folder: Folder) : Folder =
        match Doc.load folder.root uri with
        | Some doc -> { folder with docs = Map.add uri doc folder.docs }
        | None -> folder

    let removeDocument (uri: PathUri) (folder: Folder) : Folder =
        { folder with docs = Map.remove uri folder.docs }

    let addDocument (doc: Doc) (folder: Folder) : Folder =
        { folder with docs = Map.add doc.path doc folder.docs }


    let tryFindDocumentBySlug (slug: Slug) (folder: Folder) : option<Doc> =
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
            | Some destDocName -> tryFindDocumentBySlug (Slug.ofString destDocName) folder

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
            |> Option.defaultWith (fun () -> srcDoc.relPath.AbsPathUrlEncode())

        let isMatchingDoc doc = doc.relPath.AbsPathUrlEncode() = targetDocUrl

        let matchingDoc = folder.docs |> Map.values |> Seq.tryFind isMatchingDoc

        let tryMatch doc =
            if doc.relPath.AbsPathUrlEncode() = targetDocUrl then
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
