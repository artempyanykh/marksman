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
open Marksman.Paths
open Marksman.Names
open Marksman.Workspace

type InternNameNode = Node<InternName>

module InternNameNode =
    let ofWikiUnchecked (src: DocId) (node: WikiEncodedNode) =
        let name = InternName.mkUnchecked src (WikiEncoded.decode node.data)
        { text = node.text; range = node.range; data = name }

    let ofUrlChecked
        (configuredExts: array<string>)
        (src: DocId)
        ({ text = text; range = range; data = data }: UrlEncodedNode)
        : option<InternNameNode> =
        InternName.mkChecked configuredExts src (UrlEncoded.decode data)
        |> Option.map (fun name -> { text = text; range = range; data = name })

/// Unresolved reference.
[<RequireQualifiedAccess>]
type Uref =
    | Doc of InternNameNode
    | Heading of doc: option<InternNameNode> * heading: HeadingNode
    | LinkDef of TextNode

and HeadingNode =
    | Wiki of WikiEncodedNode
    | Url of UrlEncodedNode

    member this.DecodedText =
        match this with
        | Wiki n -> n.text.UrlDecode()
        | Url n -> n.text.UrlDecode()

module Uref =
    let ofElement (configuredExts: array<string>) (srcId: DocId) (el: Element) : option<Uref> =
        match el with
        | WL wl ->
            match wl.data.doc, wl.data.heading with
            | Some doc, Some heading ->
                Uref.Heading(Some(InternNameNode.ofWikiUnchecked srcId doc), Wiki heading)
                |> Some
            | Some doc, None -> Uref.Doc(InternNameNode.ofWikiUnchecked srcId doc) |> Some
            | None, Some heading -> Uref.Heading(None, Wiki heading) |> Some
            | None, None -> None
        | ML ml ->
            match ml.data with
            | MdLink.IL (_, Some url, _) ->
                let docUrl = Url.ofUrlNode url

                match docUrl.url, docUrl.anchor with
                | Some url, Some anchor ->
                    InternNameNode.ofUrlChecked configuredExts srcId url
                    |> Option.map (fun sym -> Uref.Heading(Some(sym), Url anchor))
                | Some url, None ->
                    InternNameNode.ofUrlChecked configuredExts srcId url
                    |> Option.map Uref.Doc
                | None, Some anchor -> Uref.Heading(None, Url anchor) |> Some
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

    let isFuzzyMatchDoc (name: InternName) (doc: Doc) : bool =
        let byTitle = Slug.isSubString (InternName.slug name) (Doc.slug doc)

        let byPath () =
            match InternName.tryAsPath name with
            | Some linkRootPath ->
                (linkRootPath |> InternPath.toRel |> RelPath.toSystem)
                    .AbsPathUrlEncode()
                    .IsSubStringOf((Doc.pathFromRoot doc |> RelPath.toSystem).AbsPathUrlEncode())
            | None -> false

        byTitle || byPath ()

    let filterMatchingDocs (folder: Folder) (name: InternName) : seq<FileLink> =
        let completionStyle = (Folder.configOrDefault folder).ComplWikiStyle()

        let byTitle: Map<Doc, FileLinkKind> =
            Folder.filterDocsBySlug (InternName.name name |> Slug.ofString) folder
            |> Seq.map (fun doc -> doc, FileLinkKind.Title)
            |> Map.ofSeq

        let byPath: Map<Doc, FileLinkKind> =
            InternName.tryAsPath name
            |> Option.map (fun path ->
                let relPath = InternPath.toRel path

                let docs = Folder.filterDocsByInternPath path folder

                docs
                |> Seq.map (fun doc ->
                    let docPath = Doc.pathFromRoot doc

                    let linkKind =
                        if docPath = relPath then
                            FileLinkKind.FilePath
                        else if (RelPath.filenameStem docPath) = (RelPath.filenameStem relPath) then
                            FileLinkKind.FileStem
                        else
                            FileLinkKind.FileName

                    doc, linkKind))
            |> Option.defaultValue []
            |> Map.ofSeq

        let main, aux =
            if completionStyle = Config.TitleSlug then
                byTitle, byPath
            else
                byPath, byTitle

        let merged =
            let aux = Map.keys main |> Seq.fold (flip Map.remove) aux
            Map.fold (fun m k v -> Map.add k v m) aux main

        let link = InternName.name name

        seq {
            for KeyValue (doc, linkKind) in merged do
                yield { link = link; kind = linkKind; dest = doc }
        }


    let filterFuzzyMatchingDocs (folder: Folder) (name: InternName) : seq<Doc> =
        Folder.docs folder |> Seq.filter (isFuzzyMatchDoc name)

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
    | Tag of Doc * Node<Tag>

module Dest =
    let doc: Dest -> Doc =
        function
        | Dest.Doc { dest = doc }
        | Dest.LinkDef (doc, _) -> doc
        | Dest.Heading (docLink, _) -> DocLink.doc docLink
        | Dest.Tag (doc, _) -> doc

    let element: Dest -> Element option =
        function
        | Dest.Doc { kind = FileLinkKind.Title; dest = d } -> Doc.title d |>> H
        | Dest.Doc _ -> None
        | Dest.Heading (_, h) -> Some(H h)
        | Dest.LinkDef (_, ld) -> Some(MLD ld)
        | Dest.Tag (_, t) -> Some(T t)

    let range: Dest -> Range =
        function
        | Dest.Doc { dest = doc } ->
            Doc.title doc
            |> Option.map Node.range
            |> Option.defaultWith (Doc.text doc).FullRange
        | Dest.Heading (_, heading) -> heading.range
        | Dest.LinkDef (_, linkDef) -> linkDef.range
        | Dest.Tag (_, tag) -> tag.range

    let scope: Dest -> Range =
        function
        | Dest.Doc { dest = doc } -> (Doc.text doc).FullRange()
        | Dest.Heading (_, heading) -> heading.data.scope
        | Dest.LinkDef (_, linkDef) -> linkDef.range
        | Dest.Tag (_, tag) -> tag.range

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
            let doc = FileLink.filterMatchingDocs folder docName.data
            doc |>> Dest.Doc
        | Uref.Heading (docName, heading) ->
            let matchingDocs =
                docName
                |> Option.map (fun docName ->
                    FileLink.filterMatchingDocs folder docName.data |> Seq.map Explicit)
                |> Option.defaultValue [ Implicit srcDoc ]

            seq {
                for doc in matchingDocs do
                    let headings =
                        doc
                        |> DocLink.doc
                        |> Doc.index
                        |> Index.filterHeadingBySlug (Slug.ofString heading.DecodedText)

                    for h in headings do
                        yield Dest.Heading(doc, h)
            }

    let tryResolveElement (folder: Folder) (doc: Doc) (element: Element) : seq<Dest> =
        let configuredExts =
            (Folder.configOrDefault folder).CoreMarkdownFileExtensions()

        match Uref.ofElement configuredExts (Doc.id doc) element with
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
            | T t -> [| Dest.Tag(srcDoc, t) |]
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
                | Dest.Tag _ -> Folder.docs folder |> List.ofSeq

            let referencingEls =
                seq {
                    for targetDoc in targetDocs do
                        match declToFind with
                        | Dest.Tag(_, tag) ->
                            let targets =
                                Doc.index targetDoc
                                |> Index.filterTagsByName tag.data.name.text
                                |> Seq.filter (fun t -> T t <> el)
                                |> Seq.map (fun t -> targetDoc, T t, [| Dest.Tag(targetDoc, t) |])

                            yield! targets
                        | _ ->
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
