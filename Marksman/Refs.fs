module Marksman.Refs

open System
open System.IO

open FSharpPlus.Data
open FSharpPlus.Operators
open FSharpPlus.GenericBuilders
open Ionide.LanguageServerProtocol.Types

open Marksman.Cst
open Marksman.Index
open Marksman.Misc
open Marksman.Workspace

type InternName = InternName of string

module InternName =
    let name (InternName s) = s

    let ofUrl (configuredExts: array<string>) url : option<InternName> =
        if Uri.IsWellFormedUriString(url, UriKind.Absolute) then
            None
        else if isPotentiallyMarkdownFile configuredExts url then
            Some(InternName url)
        else
            None

    let tryResolveToRootPath
        (folderPath: RootPath)
        (srcDocPath: PathUri)
        (url: string)
        : option<string> =
        if Uri.IsWellFormedUriString(url, UriKind.Absolute) then
            None
        else if url.StartsWith('/') then
            Some(url.AbsPathUrlEncodedToRelPath())
        else
            try
                let srcDirComponents =
                    Path
                        .GetDirectoryName(srcDocPath.LocalPath)
                        .Split(Path.DirectorySeparatorChar)

                // Make sure to retain the root component of the path
                if srcDocPath.LocalPath.StartsWith('/') && srcDirComponents[0] = "" then
                    srcDirComponents[0] <- "/"

                let urlComponents = url.AbsPathUrlEncodedToRelPath().Split('\\', '/')
                let allComponents = Array.append srcDirComponents urlComponents
                let targetPath = Path.Combine(allComponents)
                let targetPathNormalized = (PathUri.ofString targetPath).LocalPath
                let folderPathNormalized = (RootPath.path folderPath).LocalPath

                let rooted =
                    if targetPathNormalized.StartsWith(folderPathNormalized) then
                        let relPath =
                            Path.GetRelativePath(folderPathNormalized, targetPathNormalized)

                        let relPathNormalized =
                            String.concat "/" (relPath.Split(Path.DirectorySeparatorChar))

                        Some relPathNormalized
                    else
                        None

                rooted
            with
            | :? UriFormatException
            | :? InvalidOperationException -> None

type InternNameNode = Node<InternName>

module InternNameNode =
    let mk name range : InternNameNode = { text = InternName.name name; range = range; data = name }

    let ofTextUnchecked { text = text; range = range } : InternNameNode = mk (InternName text) range

    let ofTextChecked
        (configuredExts: array<string>)
        { text = text; range = range }
        : option<InternNameNode> =
        InternName.ofUrl configuredExts text
        |> Option.map (fun sym -> mk sym range)

/// Unresolved reference.
[<RequireQualifiedAccess>]
type Uref =
    | Doc of InternNameNode
    | Heading of doc: option<InternNameNode> * heading: TextNode
    | LinkDef of TextNode

module Uref =
    let ofElement (configuredExts: array<string>) (el: Element) : option<Uref> =
        match el with
        | WL wl ->
            match wl.data.doc, wl.data.heading with
            | Some doc, Some heading ->
                Uref.Heading(Some(InternNameNode.ofTextUnchecked doc), heading)
                |> Some
            | Some doc, None -> Uref.Doc(InternNameNode.ofTextUnchecked doc) |> Some
            | None, Some heading -> Uref.Heading(None, heading) |> Some
            | None, None -> None
        | ML ml ->
            match ml.data with
            | MdLink.IL (_, Some url, _) ->
                let docUrl = Url.ofUrlNode url

                match docUrl.url, docUrl.anchor with
                | Some url, Some anchor ->
                    InternNameNode.ofTextChecked configuredExts url
                    |> Option.map (fun sym -> Uref.Heading(Some(sym), anchor))
                | Some url, None ->
                    InternNameNode.ofTextChecked configuredExts url |> Option.map Uref.Doc
                | None, Some anchor -> Uref.Heading(None, anchor) |> Some
                | None, None -> None
            | MdLink.IL (_, None, _) -> None
            | MdLink.RS label
            | MdLink.RC label
            | MdLink.RF (_, label) -> Some(Uref.LinkDef label)
        | H _
        | YML _
        | T _
        | MLD _ -> None

    let hasExplicitDoc =
        function
        | Uref.Doc _ -> true
        | Uref.Heading(doc = Some _) -> true
        | Uref.Heading(doc = None) -> false
        | Uref.LinkDef _ -> false

[<RequireQualifiedAccess>]
type FileLinkKind =
    | FilePath
    | FileName
    | FileStem
    | Title

type FileLink = { link: string; kind: FileLinkKind; dest: Doc }

module FileLink =
    let dest { dest = dest } = dest

    let tryMatchDoc (folder: Folder) (srcDoc: Doc) (InternName name) (doc: Doc) : option<FileLink> =
        let titleLink =
            if (Slug.ofString name) = (Doc.slug doc) then
                Some { link = name; kind = FileLinkKind.Title; dest = doc }
            else
                None

        let linkRootPath =
            InternName.tryResolveToRootPath (Folder.rootPath folder) (Doc.path srcDoc) name

        let fileStemLink, fileNameLink, filePathLink =
            match linkRootPath with
            | Some linkRootPath ->
                let docFileStem = Path.GetFileNameWithoutExtension(Doc.pathFromRoot doc)

                let fileStemLink =
                    if name.AbsPathUrlEncode() = docFileStem.AbsPathUrlEncode() then
                        Some { link = name; kind = FileLinkKind.FileStem; dest = doc }
                    else
                        None

                let docFileName = Path.GetFileName(Doc.pathFromRoot doc)

                let fileNameLink =
                    if name.AbsPathUrlEncode() = docFileName.AbsPathUrlEncode() then
                        Some { link = name; kind = FileLinkKind.FileName; dest = doc }
                    else
                        None

                let filePathLink =
                    try
                        let nameFileStem = Path.GetFileNameWithoutExtension(name)

                        if
                            nameFileStem.AbsPathUrlEncode() = docFileStem.AbsPathUrlEncode()
                            && linkRootPath
                                .AbsPathUrlEncode()
                                .IsSubStringOf((Doc.pathFromRoot doc).AbsPathUrlEncode())
                        then
                            Some { link = name; kind = FileLinkKind.FilePath; dest = doc }
                        else
                            None
                    with :? ArgumentException ->
                        None

                fileStemLink, fileNameLink, filePathLink
            | None -> None, None, None

        let completionStyle = (Folder.configOrDefault folder).ComplWikiStyle()

        let fileLink =
            fileStemLink
            |> Option.orElse fileNameLink
            |> Option.orElse filePathLink

        match titleLink, fileLink with
        | Some _, None -> titleLink
        | None, Some _ -> fileLink
        | None, None -> None
        | Some _, Some _ when completionStyle = Config.TitleSlug -> titleLink
        | Some _, Some _ -> fileLink

    let isFuzzyMatchDoc (folder: Folder) (srcDoc: Doc) (InternName name) (doc: Doc) : bool =
        let byTitle = Slug.isSubString (Slug.ofString name) (Doc.slug doc)

        let byPath () =
            match
                InternName.tryResolveToRootPath (Folder.rootPath folder) (Doc.path srcDoc) name
            with
            | Some linkRootPath ->
                linkRootPath
                    .AbsPathUrlEncode()
                    .IsSubStringOf((Doc.pathFromRoot doc).AbsPathUrlEncode())
            | None -> false

        byTitle || byPath ()

    let filterMatchingDocs (folder: Folder) (srcDoc: Doc) (name: InternName) : seq<FileLink> =
        Folder.docs folder |> Seq.choose (tryMatchDoc folder srcDoc name)

    let filterFuzzyMatchingDocs (folder: Folder) (srcDoc: Doc) (name: InternName) : seq<Doc> =
        Folder.docs folder |> Seq.filter (isFuzzyMatchDoc folder srcDoc name)

type DocLink =
    | Explicit of FileLink
    | Implicit of Doc

module DocLink =
    let doc =
        function
        | Explicit { dest = doc }
        | Implicit doc -> doc

    let isSame this other = doc this = doc other

/// Resolved reference.
[<RequireQualifiedAccess>]
type Dest =
    | Doc of FileLink
    | Heading of DocLink * Node<Heading>
    | LinkDef of Doc * Node<MdLinkDef>

module Dest =
    let doc: Dest -> Doc =
        function
        | Dest.Doc { dest = doc }
        | Dest.LinkDef (doc, _) -> doc
        | Dest.Heading (docLink, _) -> DocLink.doc docLink

    let element: Dest -> Element option =
        function
        | Dest.Doc { kind = FileLinkKind.Title; dest = d } -> Doc.title d |>> H
        | Dest.Doc _ -> None
        | Dest.Heading (_, h) -> Some(H h)
        | Dest.LinkDef (_, ld) -> Some(MLD ld)

    let range: Dest -> Range =
        function
        | Dest.Doc { dest = doc } ->
            Doc.title doc
            |> Option.map Node.range
            |> Option.defaultWith (Doc.text doc).FullRange
        | Dest.Heading (_, heading) -> heading.range
        | Dest.LinkDef (_, linkDef) -> linkDef.range

    let scope: Dest -> Range =
        function
        | Dest.Doc { dest = doc } -> (Doc.text doc).FullRange()
        | Dest.Heading (_, heading) -> heading.data.scope
        | Dest.LinkDef (_, linkDef) -> linkDef.range

    let uri (ref: Dest) : DocumentUri = doc ref |> Doc.uri

    let location (ref: Dest) : Location = { Uri = uri ref; Range = range ref }

    let overlapsWith this other =
        match this, other with
        | Dest.Doc { dest = thisDoc }, Dest.Doc { dest = otherDoc }
        | Dest.Doc { dest = thisDoc }, Dest.Heading (Explicit { dest = otherDoc }, _) ->
            thisDoc = otherDoc
        | Dest.Doc _, _ -> false
        | Dest.Heading (thisDocLink, thisHeading), Dest.Heading (otherDocLink, otherHeading) ->
            DocLink.isSame thisDocLink otherDocLink && thisHeading = otherHeading
        | _, _ -> this = other


    let tryResolveUref (uref: Uref) (srcDoc: Doc) (folder: Folder) : seq<Dest> =
        match uref with
        | Uref.LinkDef label ->
            let ld =
                srcDoc
                |> Doc.index
                |> Index.tryFindLinkDef (LinkLabel.ofString label.text)

            match ld |>> fun x -> Dest.LinkDef(srcDoc, x) with
            | None -> Seq.empty
            | Some ld -> [ ld ]
        | Uref.Doc docName ->
            let doc = FileLink.filterMatchingDocs folder srcDoc docName.data
            doc |>> Dest.Doc
        | Uref.Heading (docName, heading) ->
            let matchingDocs =
                docName
                |> Option.map (fun docName ->
                    FileLink.filterMatchingDocs folder srcDoc docName.data
                    |> Seq.map Explicit)
                |> Option.defaultValue [ Implicit srcDoc ]

            seq {
                for doc in matchingDocs do
                    let headings =
                        doc
                        |> DocLink.doc
                        |> Doc.index
                        |> Index.filterHeadingBySlug (Slug.ofString heading.text)

                    for h in headings do
                        yield Dest.Heading(doc, h)
            }

    let tryResolveElement (folder: Folder) (doc: Doc) (element: Element) : seq<Dest> =
        let configuredExts =
            (Folder.configOrDefault folder).CoreMarkdownFileExtensions()

        match Uref.ofElement configuredExts element with
        | Some uref -> tryResolveUref uref doc folder
        | None -> Seq.empty

    let resolveLinks (folder: Folder) (doc: Doc) : Map<Element, array<Dest>> =
        let links = Index.links (Doc.index doc)

        links
        |> Seq.collect (fun link ->
            match tryResolveElement folder doc link |> Seq.toArray with
            | refs -> [ link, refs ])
        |> Map.ofSeq

    let private findReferencingElements
        (refMap: Map<Element, array<Dest>>)
        (target: Dest)
        : seq<Element * array<Dest>> =
        seq {
            for KeyValue (el, refs) in refMap do
                let matchingDest = Array.filter (overlapsWith target) refs

                if not (Array.isEmpty matchingDest) then
                    yield el, matchingDest
        }
        |> Seq.sortBy (fun (el, _) -> Element.rangeStart el)

    /// Finds elements referencing `el`.
    /// When `el` is a link, it's resolved to its destination first and then references to the
    /// destination are found.
    let findElementRefs
        (includeDecl: bool)
        (folder: Folder)
        (srcDoc: Doc)
        (el: Element)
        : seq<Doc * Element * array<Dest>> =
        let declsToFind =
            match el with
            | MLD ld -> [| Dest.LinkDef(srcDoc, ld) |]
            | H h when Heading.isTitle h.data ->
                [| Dest.Doc { link = h.text; kind = FileLinkKind.Title; dest = srcDoc } |]
            | H h -> [| Dest.Heading(Implicit srcDoc, h) |]
            | link when Element.isLink link ->
                let linkToDecl = resolveLinks folder srcDoc
                Map.tryFind link linkToDecl |> Option.defaultValue [||]
            | _ -> [||]

        let resolveDecl includeDecl declToFind =
            let targetDocs =
                match declToFind with
                | Dest.LinkDef _ -> [ srcDoc ]
                | Dest.Heading _
                | Dest.Doc _ -> Folder.docs folder |> List.ofSeq

            let referencingEls =
                seq {
                    for targetDoc in targetDocs do
                        let targetDocRefs = resolveLinks folder targetDoc

                        let backRefs =
                            findReferencingElements targetDocRefs declToFind
                            |> Seq.map (fun (el, dest) -> targetDoc, el, dest)

                        yield! backRefs
                }

            let declEl =
                if includeDecl then
                    let decl =
                        monad' {
                            let! el = element declToFind
                            let doc = doc declToFind
                            doc, el, [| declToFind |]
                        }

                    decl |> Option.toArray
                else
                    [||]

            Seq.append declEl referencingEls


        declsToFind |> Seq.collect (resolveDecl includeDecl)
