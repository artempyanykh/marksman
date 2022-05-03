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
      version: option<int>
      text: Text
      elements: array<Element> }

module Document =
    let logger =
        LogProvider.getLoggerByName "Document"

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

        let newElements = parseText newText

        { document with
            version = newVersion
            text = newText
            elements = newElements }

    let fromLspDocument (item: TextDocumentItem) : Document =
        let path = PathUri.fromString item.Uri
        let text = mkText item.Text
        let elements = parseText text

        { path = path
          version = Some item.Version
          text = text
          elements = elements }


    let load (path: PathUri) : option<Document> =
        try
            let content =
                (new StreamReader(path.AbsolutePath)).ReadToEnd()

            let text = mkText content
            let elements = parseText text

            Some
                { path = path
                  text = text
                  elements = elements
                  version = None }
        with
        | :? FileNotFoundException -> None

    let title (doc: Document) : option<Heading> =
        let isTitle el =
            Element.asHeading el
            |> Option.map (fun x -> x.level = 1)
            |> Option.defaultValue false

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
                    | H h -> yield! collect h.children
                    | R _
                    | CP _ -> ()
            }

        collect document.elements

    let headings (document: Document) : seq<Heading> =
        seq {
            for el in elementsAll document do
                match Element.asHeading el with
                | Some h -> yield h
                | _ -> ()
        }

    let headingByName (name: string) (document: Document) : option<Heading> =
        let nameSlug = name.Slug()
        let matchingHeading h = (Heading.title h).Slug() = nameSlug
        headings document |> Seq.tryFind matchingHeading

    let elementAtPos (pos: Position) (doc: Document) : option<Element> =
        elementsAll doc
        |> Seq.tryFind (fun el ->
            let range = Element.range el
            range.Start <= pos && pos < range.End)

type Folder =
    { name: string
      root: PathUri
      documents: Map<PathUri, Document> }

module Folder =
    let private logger =
        LogProvider.getLoggerByName "Folder"

    let tryFindDocument (uri: PathUri) (folder: Folder) : option<Document> = Map.tryFind uri folder.documents

    let rec private loadDocuments (root: PathUri) : seq<PathUri * Document> =
        let logger =
            LogProvider.getLoggerByName "readRoot"

        let di = DirectoryInfo(root.AbsolutePath)

        try
            let files = di.GetFiles("*.md")
            let dirs = di.GetDirectories()

            seq {
                for file in files do
                    let pathUri =
                        PathUri.fromString file.FullName

                    let document = Document.load pathUri

                    match document with
                    | Some document -> yield pathUri, document
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
        if Directory.Exists(root.AbsolutePath) then
            let documents =
                loadDocuments root |> Map.ofSeq

            { name = name
              root = root
              documents = documents }
            |> Some
        else
            logger.warn (
                Log.setMessage "Folder path doesn't exist"
                >> Log.addContext "uri" root
            )

            None

    let loadDocument (uri: PathUri) (folder: Folder) : Folder =
        match Document.load uri with
        | Some doc -> { folder with documents = Map.add uri doc folder.documents }
        | None -> folder

    let removeDocument (uri: PathUri) (folder: Folder) : Folder =
        { folder with documents = Map.remove uri folder.documents }

    let addDocument (doc: Document) (folder: Folder) : Folder =
        { folder with documents = Map.add doc.path doc folder.documents }

    let documentName (doc: Document) (folder: Folder) : DocName =
        let docPath = doc.path.AbsolutePath
        let folderPath = folder.root.AbsolutePath

        let docRelPath =
            Path.GetRelativePath(folderPath, docPath)

        let docName =
            Path.GetFileNameWithoutExtension(docRelPath)

        docName

    let tryFindDocumentByName (name: DocName) (folder: Folder) : option<Document> =
        folder.documents
        |> Map.values
        |> Seq.tryFind (fun doc -> documentName doc folder = name)

    let findDocumentByName (name: DocName) (folder: Folder) : Document =
        tryFindDocumentByName name folder
        |> Option.defaultWith (fun () -> failwith $"Requested document doesn't exist: {name}")

    let findCompletionCandidates (pos: Position) (docUri: PathUri) (folder: Folder) : array<CompletionItem> =
        let doc = tryFindDocument docUri folder

        match doc with
        | None -> [||]
        | Some doc ->
            let curDocName = documentName doc folder

            let isAtPoint =
                function
                | CP cp -> cp.range.End = pos
                | R x -> x.range.Start <= pos && pos < x.range.End
                | _ -> false

            let atPoint =
                Document.elementsAll doc |> Seq.tryFind isAtPoint

            match atPoint with
            | None -> [||]
            | Some atPoint ->
                let wantedDoc, wantedHeading =
                    match atPoint with
                    | R ref ->
                        let destDoc =
                            Dest.destDoc ref.dest
                            // Absence of explicit doc means completion inside the current doc
                            |> Option.defaultValue curDocName
                            |> Some

                        destDoc, Dest.destHeading ref.dest
                    | CP cp -> CompletionPoint.destNote cp, None
                    | _ -> None, None

                // Now we have 2 modes of completion to tackle
                match wantedDoc, wantedHeading with
                // Plain doc name completion
                | Some wantedDoc, None ->
                    let docs =
                        Map.values folder.documents
                        |> Seq.map (fun doc -> doc, Document.title doc, documentName doc folder)

                    let isMatchingDoc (_, title: option<Heading>, name) =
                        let titleMatch =
                            title
                            |> Option.map (fun t -> wantedDoc.IsSubSequenceOf(t.text))
                            |> Option.defaultValue false

                        let nameMatch =
                            wantedDoc.IsSubSequenceOf(name)

                        titleMatch || nameMatch

                    let matchingDocs =
                        docs |> Seq.filter isMatchingDoc

                    let toCompletionItem (_doc, title, name) =
                        let label =
                            Option.map Heading.text title
                            |> Option.defaultValue name

                        let detail =
                            if Option.isSome title then
                                Some name
                            else
                                None

                        let dest = Dest.Doc name
                        let existingText = Element.text atPoint

                        let newText =
                            Dest.fmtStyled (Dest.isWikiBracket existingText) (Dest.isPipeDelimiter existingText) dest

                        let textEdit =
                            { Range = Element.range atPoint
                              NewText = newText }

                        { CompletionItem.Create(label) with
                            Detail = detail
                            TextEdit = Some textEdit
                            FilterText = Some newText }

                    matchingDocs
                    |> Seq.map toCompletionItem
                    |> Array.ofSeq
                // Heading completion inside an already specified doc
                | Some wantedDoc, Some wantedHeading ->
                    let targetDoc =
                        Map.values folder.documents
                        |> Seq.tryFind (fun doc -> documentName doc folder = wantedDoc)

                    match targetDoc with
                    | None -> [||]
                    | Some targetDoc ->
                        let matchingHeadings =
                            seq {
                                for el in Document.elementsAll targetDoc do
                                    match Element.asHeading el with
                                    | Some h when
                                        h.level <> 1
                                        && wantedHeading.IsSubSequenceOf(Heading.title h)
                                        ->
                                        yield h
                                    | _ -> ()
                            }

                        let toCompletionItem heading =
                            // TODO: consider using slug for heading
                            let label = Heading.title heading

                            let destDoc =
                                if wantedDoc = curDocName then
                                    None
                                else
                                    Some wantedDoc

                            let dest = Dest.Heading(destDoc, label)
                            let existingText = Element.text atPoint

                            let newText =
                                Dest.fmtStyled
                                    (Dest.isWikiBracket existingText)
                                    (Dest.isPipeDelimiter existingText)
                                    dest

                            let textEdit =
                                { Range = Element.range atPoint
                                  NewText = newText }

                            { CompletionItem.Create(label) with
                                TextEdit = Some textEdit
                                FilterText = Some newText }

                        matchingHeadings
                        |> Seq.map toCompletionItem
                        |> Array.ofSeq
                | _ -> [||]

    let tryFindReferenceTarget (sourceDoc: Document) (ref: Ref) (folder: Folder) : option<Document * Option<Heading>> =
        // Discover target doc.
        let destDocName = Dest.destDoc ref.dest

        let destDoc =
            match destDocName with
            | None -> Some sourceDoc
            | Some destDocName -> tryFindDocumentByName destDocName folder

        match destDoc with
        | None -> None
        | Some destDoc ->
            // Discover target heading.
            // When target heading is specified but can't be found, the whole thing turns into None.
            match Dest.destHeading ref.dest with
            | None -> Some(destDoc, Document.title destDoc)
            | Some headingName ->
                match Document.headingByName headingName destDoc with
                | Some _ as heading -> Some(destDoc, heading)
                | _ -> None
