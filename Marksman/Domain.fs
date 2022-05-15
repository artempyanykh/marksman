module Marksman.Domain

open System
open System.IO
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open Text
open Parser
open Misc

type Document =
    { path: PathUri
      name: DocName
      version: option<int>
      text: Text
      elements: array<Element> }

module Document =
    let logger = LogProvider.getLoggerByName "Document"

    let applyLspChange (change: DidChangeTextDocumentParams) (document: Document) : Document =
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

        { document with version = newVersion; text = newText; elements = newElements }

    let documentName (folderPath: PathUri) (docPath: PathUri) : DocName =
        let docPath = docPath.LocalPath
        let folderPath = folderPath.LocalPath

        let docRelPath = Path.GetRelativePath(folderPath, docPath)

        let docName = Path.GetFileNameWithoutExtension(docRelPath)

        docName

    let fromLspDocument (root: PathUri) (item: TextDocumentItem) : Document =
        let path = PathUri.fromString item.Uri
        let text = mkText item.Text
        let elements = parseText text

        { path = path
          name = documentName root path
          version = Some item.Version
          text = text
          elements = elements }


    let load (root: PathUri) (path: PathUri) : option<Document> =
        try
            let content = using (new StreamReader(path.LocalPath)) (fun f -> f.ReadToEnd())

            let text = mkText content
            let elements = parseText text

            Some
                { path = path
                  name = documentName root path
                  text = text
                  elements = elements
                  version = None }
        with
        | :? FileNotFoundException -> None

    let title (doc: Document) : option<Node<Heading>> =
        let isTitle el =
            Element.asHeading el |> Option.map (fun x -> x.data.level = 1) |> Option.defaultValue false

        let titleOpt =
            doc.elements
            |> Array.tryFind isTitle
            |> Option.map (function
                | H h -> h
                | other -> failwith $"Expected heading: {other}")

        titleOpt

    let elementsAll (document: Document) : seq<Element> =
        let rec collect els =
            seq {
                for el in els do
                    yield el

                    match el with
                    | H h -> yield! collect h.data.children
                    | WL _
                    | ML _ -> ()
            }

        collect document.elements

    let headings (document: Document) : seq<Node<Heading>> =
        seq {
            for el in elementsAll document do
                match Element.asHeading el with
                | Some h -> yield h
                | _ -> ()
        }

    let headingByName (name: string) (document: Document) : option<Node<Heading>> =
        let nameSlug = name.Slug()
        let matchingHeading h = (Heading.title h.data).Slug() = nameSlug
        headings document |> Seq.tryFind matchingHeading

    let elementAtPos (pos: Position) (doc: Document) : option<Element> =
        elementsAll doc
        |> Seq.tryFind (fun el ->
            let range = Element.range el
            range.Start <= pos && pos < range.End)

type Folder = { name: string; root: PathUri; documents: Map<PathUri, Document> }

module Folder =
    let private logger = LogProvider.getLoggerByName "Folder"

    let tryFindDocument (uri: PathUri) (folder: Folder) : option<Document> = Map.tryFind uri folder.documents

    let rec private loadDocuments (root: PathUri) : seq<Document> =
        let logger = LogProvider.getLoggerByName "readRoot"

        let di = DirectoryInfo(root.LocalPath)

        try
            let files = di.GetFiles("*.md")
            let dirs = di.GetDirectories()

            seq {
                for file in files do
                    let pathUri = PathUri.fromString file.FullName

                    let document = Document.load root pathUri

                    match document with
                    | Some document -> yield document
                    | _ -> ()

                for dir in dirs do
                    yield! loadDocuments (PathUri.fromString dir.FullName)
            }
        with
        | :? UnauthorizedAccessException as exn ->
            logger.warn (
                Log.setMessage "Couldn't read the root folder" >> Log.addContext "root" root >> Log.addException exn
            )

            Seq.empty
        | :? DirectoryNotFoundException as exn ->
            logger.warn (
                Log.setMessage "The root folder doesn't exist" >> Log.addContext "root" root >> Log.addException exn
            )

            Seq.empty

    let tryLoad (name: string) (root: PathUri) : option<Folder> =
        if Directory.Exists(root.LocalPath) then
            let documents =
                loadDocuments root |> Seq.map (fun doc -> doc.path, doc) |> Map.ofSeq

            { name = name; root = root; documents = documents } |> Some
        else
            logger.warn (Log.setMessage "Folder path doesn't exist" >> Log.addContext "uri" root)

            None

    let loadDocument (uri: PathUri) (folder: Folder) : Folder =
        match Document.load folder.root uri with
        | Some doc -> { folder with documents = Map.add uri doc folder.documents }
        | None -> folder

    let removeDocument (uri: PathUri) (folder: Folder) : Folder =
        { folder with documents = Map.remove uri folder.documents }

    let addDocument (doc: Document) (folder: Folder) : Folder =
        { folder with documents = Map.add doc.path doc folder.documents }


    let tryFindDocumentByName (name: DocName) (folder: Folder) : option<Document> =
        folder.documents |> Map.values |> Seq.tryFind (fun doc -> doc.name = name)

    let findDocumentByName (name: DocName) (folder: Folder) : Document =
        tryFindDocumentByName name folder
        |> Option.defaultWith (fun () -> failwith $"Requested document doesn't exist: {name}")

    let tryFindWikiLinkTarget
        (sourceDoc: Document)
        (wl: WikiLink)
        (folder: Folder)
        : option<Document * Option<Node<Heading>>> =
        // Discover target doc.
        let destDocName = WikiLink.destDoc wl

        let destDoc =
            match destDocName with
            | None -> Some sourceDoc
            | Some destDocName -> tryFindDocumentByName destDocName folder

        match destDoc with
        | None -> None
        | Some destDoc ->
            // Discover target heading.
            // When target heading is specified but can't be found, the whole thing turns into None.
            match WikiLink.destHeading wl with
            | None -> Some(destDoc, Document.title destDoc)
            | Some headingName ->
                match Document.headingByName headingName destDoc with
                | Some _ as heading -> Some(destDoc, heading)
                | _ -> None

    let docCount (folder: Folder) : int = folder.documents.Values.Count

type Workspace = { folders: Map<PathUri, Folder> }

module Workspace =
    let ofFolders (folders: seq<Folder>) : Workspace =
        { folders = folders |> Seq.map (fun f -> f.root, f) |> Map.ofSeq }

    let folders (workspace: Workspace) : seq<Folder> =
        seq {
            for KeyValue (_, f) in workspace.folders do
                yield f
        }

    let tryFindFolderByPath (root: PathUri) (workspace: Workspace) : option<Folder> = Map.tryFind root workspace.folders

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
            folders |> Seq.fold (fun fs f -> Map.add f.root f fs) workspace.folders

        { workspace with folders = newFolders }

    let docCount (workspace: Workspace) : int = workspace.folders.Values |> Seq.sumBy Folder.docCount
