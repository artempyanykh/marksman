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

[<RequireQualifiedAccess>]
type DocRef =
    | Title of title: TextNode
    | Url of url: TextNode

/// Unresolved reference.
[<RequireQualifiedAccess>]
type URef =
    | Doc of DocRef
    | Heading of doc: option<DocRef> * heading: TextNode
    | LinkDef of TextNode

module URef =
    let ofElement (el: Element) : option<URef> =
        match el with
        | WL wl ->
            match wl.data.doc, wl.data.heading with
            | Some doc, Some heading -> URef.Heading(Some(DocRef.Title doc), heading) |> Some
            | Some doc, None -> URef.Doc(DocRef.Title doc) |> Some
            | None, Some heading -> URef.Heading(None, heading) |> Some
            | None, None -> None
        | ML ml ->
            match ml.data with
            | MdLink.IL (_, Some url, _) ->
                let docUrl = DocUrl.ofUrlNode url

                match docUrl.url, docUrl.anchor with
                | Some url, Some anchor -> URef.Heading(Some(DocRef.Url url), anchor) |> Some
                | Some url, None -> URef.Doc(DocRef.Url url) |> Some
                | None, Some anchor -> URef.Heading(None, anchor) |> Some
                | None, None -> None
            | MdLink.IL (_, None, _) -> None
            | MdLink.RS label
            | MdLink.RC label
            | MdLink.RF (_, label) -> Some(URef.LinkDef label)
        | H _
        | MLD _ -> None

/// Resolved reference.
[<RequireQualifiedAccess>]
type Ref =
    | Doc of Doc
    | Heading of Doc * Node<Heading>
    | LinkDef of Doc * Node<MdLinkDef>

module Ref =
    let doc: Ref -> Doc =
        function
        | Ref.Doc doc -> doc
        | Ref.Heading (doc, _) -> doc
        | Ref.LinkDef (doc, _) -> doc

    let element: Ref -> Element option =
        function
        | Ref.Doc d -> Doc.title d |>> H
        | Ref.Heading (_, h) -> Some(H h)
        | Ref.LinkDef (_, ld) -> Some(MLD ld)

    let range: Ref -> Range =
        function
        | Ref.Doc doc ->
            Doc.title doc
            |> Option.map Node.range
            |> Option.defaultWith doc.text.FullRange
        | Ref.Heading (_, heading) -> heading.range
        | Ref.LinkDef (_, linkDef) -> linkDef.range

    let scope: Ref -> Range =
        function
        | Ref.Doc doc -> doc.text.FullRange()
        | Ref.Heading (_, heading) -> heading.data.scope
        | Ref.LinkDef (_, linkDef) -> linkDef.range

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

    let tryFindDocByPath (uri: PathUri) (folder: Folder) : option<Doc> = Map.tryFind uri folder.docs

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
        let isMatchingDoc doc = Doc.slug doc = slug

        folder.docs |> Map.values |> Seq.tryFind isMatchingDoc

    let tryFindDocByUrl (url: string) (folder: Folder) : option<Doc> =
        let isMatchingDoc (doc: Doc) = doc.RelPath.AbsPathUrlEncode() = url.AbsPathUrlEncode()

        folder.docs |> Map.values |> Seq.tryFind isMatchingDoc

    let tryFindDocByRef (docRef: DocRef) (folder: Folder) : option<Doc> =
        match docRef with
        | DocRef.Title title -> tryFindDocBySlug (Slug.ofString title.text) folder
        | DocRef.Url url -> tryFindDocByUrl url.text folder

    let docCount (folder: Folder) : int = folder.docs.Values.Count

    let resolveRef (uref: URef) (srcDoc: Doc) (folder: Folder) : option<Ref> =
        match uref with
        | URef.LinkDef label ->
            let ld = srcDoc.index |> Index.tryFindLinkDef label.text
            ld |>> fun x -> Ref.LinkDef(srcDoc, x)
        | URef.Doc docRef ->
            let doc = tryFindDocByRef docRef folder
            doc |>> Ref.Doc
        | URef.Heading (docRef, heading) ->
            let doc =
                docRef >>= (flip tryFindDocByRef) folder |> Option.defaultValue srcDoc

            let heading =
                doc.index |> Index.tryFindHeadingBySlug (Slug.ofString heading.text)

            heading |>> fun h -> Ref.Heading(doc, h)

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
