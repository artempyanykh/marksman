module Marksman.Refs

open FSharpPlus.Data
open FSharpPlus.Operators
open FSharpPlus.GenericBuilders
open Ionide.LanguageServerProtocol.Types

open Marksman.Config
open Marksman.Cst
open Marksman.Index
open Marksman.Misc
open Marksman.Paths
open Marksman.Names
open Marksman.Doc
open Marksman.Folder
open Marksman.Structure

type InternNameNode = Node<InternName>

module InternNameNode =
    let ofWikiUnchecked (src: DocId) (node: WikiEncodedNode) =
        let name = InternName.mkUnchecked src (WikiEncoded.decode node.data)
        { text = node.text; range = node.range; data = name }

    let ofUrlChecked
        (configuredExts: seq<string>)
        (src: DocId)
        ({ text = text; range = range; data = data }: UrlEncodedNode)
        : option<InternNameNode> =
        InternName.mkChecked configuredExts src (UrlEncoded.decode data)
        |> Option.map (fun name -> { text = text; range = range; data = name })

[<RequireQualifiedAccess>]
type FileLinkKind =
    | FilePath
    | FileName
    | FileStem
    | Title

module FileLinkKind =
    let detect (complStyle: ComplWikiStyle) (srcDoc: DocId) (name: string) : Doc -> FileLinkKind =
        let nameSlug = Slug.ofString name
        let namePath = InternName.tryAsPath { src = srcDoc; name = name }

        let linkKindAsPath doc =
            namePath
            |> Option.map (fun path ->
                let relPath = InternPath.toRel path
                let docPath = Doc.pathFromRoot doc

                if docPath = relPath then
                    FileLinkKind.FilePath
                else if (RelPath.filenameStem docPath) = (RelPath.filenameStem relPath) then
                    FileLinkKind.FileStem
                else
                    FileLinkKind.FileName)

        fun destDoc ->
            match complStyle, Doc.slug destDoc = nameSlug, linkKindAsPath destDoc with
            | TitleSlug, true, _
            | _, true, None -> FileLinkKind.Title
            | _, _, Some fileKind -> fileKind
            | _, _, None -> FileLinkKind.FileName

type FileLink = { link: string; kind: FileLinkKind; doc: Doc }

module FileLink =
    let doc fileLink = fileLink.doc

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

        let detectLinkKind =
            FileLinkKind.detect completionStyle name.src name.name

        let matchingDocs = Folder.filterDocsByName name folder

        seq {
            for doc in matchingDocs do
                let kind = detectLinkKind doc
                { link = name.name; kind = kind; doc = doc }
        }


    let filterFuzzyMatchingDocs (folder: Folder) (name: InternName) : seq<Doc> =
        Folder.docs folder |> Seq.filter (isFuzzyMatchDoc name)

type DocLink =
    | Explicit of FileLink
    | Implicit of Doc

module DocLink =
    let doc =
        function
        | Explicit { doc = doc }
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
        | Dest.Doc { doc = doc }
        | Dest.LinkDef (doc, _) -> doc
        | Dest.Heading (docLink, _) -> DocLink.doc docLink
        | Dest.Tag (doc, _) -> doc

    let element: Dest -> Element option =
        function
        | Dest.Doc { kind = FileLinkKind.Title; doc = d } -> Doc.title d |>> H
        | Dest.Doc _ -> None
        | Dest.Heading (_, h) -> Some(H h)
        | Dest.LinkDef (_, ld) -> Some(MLD ld)
        | Dest.Tag (_, t) -> Some(T t)

    let range: Dest -> Range =
        function
        | Dest.Doc { doc = doc } ->
            Doc.title doc
            |> Option.map Node.range
            |> Option.defaultWith (Doc.text doc).FullRange
        | Dest.Heading (_, heading) -> heading.range
        | Dest.LinkDef (_, linkDef) -> linkDef.range
        | Dest.Tag (_, tag) -> tag.range

    let scope: Dest -> Range =
        function
        | Dest.Doc { doc = doc } -> (Doc.text doc).FullRange()
        | Dest.Heading (_, heading) -> heading.data.scope
        | Dest.LinkDef (_, linkDef) -> linkDef.range
        | Dest.Tag (_, tag) -> tag.range

    let uri (ref: Dest) : DocumentUri = doc ref |> Doc.uri

    let location (ref: Dest) : Location = { Uri = uri ref; Range = range ref }

    let overlapsWith this other =
        match this, other with
        | Dest.Doc { doc = thisDoc }, Dest.Doc { doc = otherDoc }
        | Dest.Doc { doc = thisDoc }, Dest.Heading (Explicit { doc = otherDoc }, _) ->
            thisDoc = otherDoc
        | Dest.Doc _, _ -> false
        | Dest.Heading (thisDocLink, thisHeading), Dest.Heading (otherDocLink, otherHeading) ->
            DocLink.isSame thisDocLink otherDocLink && thisHeading = otherHeading
        | _, _ -> this = other

    let tryResolveElement (folder: Folder) (doc: Doc) (element: Element) : seq<Dest> =
        let complStyle = (Folder.configOrDefault folder).ComplWikiStyle()

        match Doc.structure doc |> Structure.tryFindSymbolForConcrete element with
        | None -> Seq.empty
        | Some srcSym ->
            let scopedSym = Syms.Sym.scopedToDoc doc.Id srcSym
            let destSyms = Folder.conn folder |> Conn.Query.resolve scopedSym

            let detectFileLink destDoc =
                match srcSym |> Syms.Sym.asRef with
                | Some (Syms.SectionRef (Some name, _)) ->
                    let kind = FileLinkKind.detect complStyle doc.Id name destDoc
                    { link = name; kind = kind; doc = destDoc }
                | _ -> failwith $"Link kind cannot be determined for {srcSym} symbol"

            let detectDocLink destDoc =
                match srcSym |> Syms.Sym.asRef with
                | Some (Syms.SectionRef (Some name, _)) ->
                    let kind = FileLinkKind.detect complStyle doc.Id name destDoc
                    let fileLink = { link = name; kind = kind; doc = destDoc }
                    Explicit fileLink
                | Some (Syms.SectionRef (None, _)) -> Implicit destDoc
                | _ -> failwith $"Link kind cannot be determined for {srcSym} symbol"

            seq {
                for destScope, destSym in destSyms do
                    match destScope with
                    | Syms.Scope.Global -> ()
                    | Syms.Scope.Doc destDocId ->
                        let destDoc = Folder.findDocById destDocId folder

                        match destSym with
                        | Syms.Sym.Def Syms.Doc -> Dest.Doc(detectFileLink destDoc)
                        | Syms.Sym.Def (Syms.Header _) ->
                            let docLink = detectDocLink destDoc

                            yield!
                                destDoc.Structure
                                |> Structure.findConcreteForSymbol destSym
                                |> Seq.choose Element.asHeading
                                |> Seq.map (fun node -> Dest.Heading(docLink, node))
                        | Syms.Sym.Def (Syms.LinkDef _) ->
                            yield!
                                destDoc.Structure
                                |> Structure.findConcreteForSymbol destSym
                                |> Seq.choose Element.asLinkDef
                                |> Seq.map (fun node -> Dest.LinkDef(destDoc, node))
                        | Syms.Sym.Ref _
                        | Syms.Sym.Tag _ -> ()
            }

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
                [| Dest.Doc { link = h.text; kind = FileLinkKind.Title; doc = srcDoc } |]
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
                        | Dest.Tag (_, tag) ->
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
