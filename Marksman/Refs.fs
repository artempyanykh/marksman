module Marksman.Refs

open FSharpPlus.Data
open FSharpPlus.Operators
open Ionide.LanguageServerProtocol.Types

open Marksman.Config
open Marksman.Misc
open Marksman.Paths
open Marksman.Names
open Marksman.Doc
open Marksman.Folder
open Marksman.Structure
open Marksman.Syms

type InternNameNode = Cst.Node<InternName>

module InternNameNode =
    let ofWikiUnchecked (src: DocId) (node: Cst.WikiEncodedNode) : InternNameNode =
        let name = InternName.mkUnchecked src (WikiEncoded.decode node.data)
        { text = node.text; range = node.range; data = name }

    let ofUrlChecked
        (configuredExts: seq<string>)
        (src: DocId)
        ({ text = text; range = range; data = data }: Cst.UrlEncodedNode)
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
            |> Option.bind (fun path ->
                let relPath = InternPath.toRel path
                let docPath = Doc.pathFromRoot doc

                if docPath = relPath then
                    Some FileLinkKind.FilePath
                else if (RelPath.filenameStem docPath) = (RelPath.filenameStem relPath) then
                    Some FileLinkKind.FileStem
                else if (RelPath.filename docPath) = (RelPath.filename relPath) then
                    Some FileLinkKind.FileName
                else
                    None)

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

/// Resolved reference.
[<RequireQualifiedAccess>]
type Dest =
    | Doc of FileLink
    | Heading of DocLink * Cst.Node<Cst.Heading>
    | LinkDef of Doc * Cst.Node<Cst.MdLinkDef>
    | Tag of Doc * Cst.Node<Cst.Tag>

module Dest =
    let doc: Dest -> Doc =
        function
        | Dest.Doc { doc = doc }
        | Dest.LinkDef(doc, _) -> doc
        | Dest.Heading(docLink, _) -> DocLink.doc docLink
        | Dest.Tag(doc, _) -> doc

    let range: Dest -> Range =
        function
        | Dest.Doc { doc = doc } ->
            Doc.title doc
            |> Option.map (fun x -> x.range)
            |> Option.defaultWith (Doc.text doc).FullRange
        | Dest.Heading(_, heading) -> heading.range
        | Dest.LinkDef(_, linkDef) -> linkDef.range
        | Dest.Tag(_, tag) -> tag.range

    let scope: Dest -> Range =
        function
        | Dest.Doc { doc = doc } -> (Doc.text doc).FullRange()
        | Dest.Heading(_, heading) -> heading.data.scope
        | Dest.LinkDef(_, linkDef) -> linkDef.range
        | Dest.Tag(_, tag) -> tag.range

    let uri (ref: Dest) : DocumentUri = doc ref |> Doc.uri

    let location (ref: Dest) : Location = { Uri = uri ref; Range = range ref }

    let detectFileLink complStyle srcDocId srcSym destDoc =
        match srcSym |> Sym.asRef with
        | Some(CrossRef r) ->
            let kind = FileLinkKind.detect complStyle srcDocId r.Doc destDoc
            { link = r.Doc; kind = kind; doc = destDoc }
        | _ -> failwith $"Link kind cannot be determined for {srcSym} symbol"

    let detectDocLink complStyle srcDocId srcSym destDoc =
        match srcSym |> Sym.asRef with
        | Some(CrossRef r) ->
            let kind = FileLinkKind.detect complStyle srcDocId r.Doc destDoc
            let fileLink = { link = r.Doc; kind = kind; doc = destDoc }
            Explicit fileLink
        | Some(IntraRef(IntraSection _)) -> Implicit destDoc
        | _ -> failwith $"Link kind cannot be determined for {srcSym} symbol"

    let private tryResolveSym (folder: Folder) (doc: Doc) (srcSym: Sym) : seq<Dest> =
        let complStyle = (Folder.configOrDefault folder).ComplWikiStyle()

        let scopedSym = Sym.scopedToDoc doc.Id srcSym
        let destSyms = Folder.conn folder |> Conn.Query.resolve scopedSym

        let detectFileLink = detectFileLink complStyle doc.Id srcSym
        let detectDocLink = detectDocLink complStyle doc.Id srcSym

        seq {
            for destScope, destSym in destSyms do
                match destScope with
                | Scope.Global -> ()
                | Scope.Doc destDocId ->
                    let destDoc = Folder.findDocById destDocId folder

                    match destSym with
                    | Sym.Def Doc -> Dest.Doc(detectFileLink destDoc)
                    | Sym.Def(Header _) ->
                        let docLink = detectDocLink destDoc

                        yield!
                            destDoc.Structure
                            |> Structure.findConcreteForSymbol destSym
                            |> Seq.choose Cst.Element.asHeading
                            |> Seq.map (fun node -> Dest.Heading(docLink, node))
                    | Sym.Def(LinkDef _) ->
                        yield!
                            destDoc.Structure
                            |> Structure.findConcreteForSymbol destSym
                            |> Seq.choose Cst.Element.asLinkDef
                            |> Seq.map (fun node -> Dest.LinkDef(destDoc, node))
                    | Sym.Ref _
                    | Sym.Tag _ -> ()
        }

    let tryResolveElement (folder: Folder) (doc: Doc) (element: Cst.Element) : seq<Dest> =
        match Doc.structure doc |> Structure.tryFindSymbolForConcrete element with
        | None -> Seq.empty
        | Some sym -> tryResolveSym folder doc sym

    let private findTagRefs includeDecl folder srcDocId srcEl tag =
        let srcDoc = Folder.findDocById srcDocId folder

        let refs =
            Folder.conn folder |> Conn.Query.resolve (Scope.Global, Sym.Tag tag)

        let refs =
            seq {
                for scope, ref in refs do
                    match scope with
                    | Scope.Global -> ()
                    | Scope.Doc destDocId ->
                        let destDoc = Folder.findDocById destDocId folder
                        let els = destDoc.Structure |> Structure.findConcreteForSymbol ref

                        yield! els |> Seq.map (fun el -> destDoc, el)
            }

        let refs = refs |> Seq.filter (fun (d, e) -> d <> srcDoc || e <> srcEl)
        if includeDecl then Seq.append [ srcDoc, srcEl ] refs else refs

    let private findDefRefs includeDecl folder inDocId srcEl def =
        let inDoc = Folder.findDocById inDocId folder

        let decls =
            if includeDecl then
                match srcEl with
                | None ->
                    inDoc.Structure
                    |> Structure.findConcreteForSymbol (Sym.Def def)
                    |> Set.toSeq
                | Some srcEl -> Seq.singleton srcEl
            else
                Seq.empty

        // This logic is a bit involved but the idea is that whenever we invoke 'find references'
        // on a level 1 header (aka a title) we should also look for references to the document
        // itself
        let defs, filter =
            match def with
            | LinkDef _ -> Seq.singleton def, konst true
            | Header(level, _) when level > 1 -> Seq.singleton def, konst true
            | Doc ->
                let headers =
                    inDoc.Structure.Symbols
                    |> Seq.choose Sym.asDef
                    |> Seq.filter Def.isHeader

                Seq.append [ Def.Doc ] headers, Sym.isRefWithExplicitDoc
            | Header(_, id) ->
                let headers =
                    inDoc.Structure.Symbols
                    |> Seq.choose Sym.asDef
                    |> Seq.filter Def.isHeader

                let filter sym =
                    Sym.isRefWithExplicitDoc sym
                    // This is required to pick up internal references to titles, e.g
                    // # Title
                    // [[#title]]
                    || Sym.asRef sym
                       |> Option.bind Ref.trySection
                       |> Option.exists (fun name -> name = Slug.ofString id)

                Seq.append [ Def.Doc ] headers, filter

        seq {
            for decl in decls do
                yield inDoc, decl

            for def in defs do
                let refs =
                    Folder.conn folder
                    |> Conn.Query.resolve (Scope.Doc inDocId, Sym.Def def)
                    |> Seq.filter (fun (_, sym) -> filter sym)

                for scope, ref in refs do
                    match scope with
                    | Scope.Global -> ()
                    | Scope.Doc destDocId ->
                        let destDoc = Folder.findDocById destDocId folder

                        yield!
                            destDoc.Structure
                            |> Structure.findConcreteForSymbol ref
                            |> Seq.map (fun el -> destDoc, el)
        }

    let private findRefRefs includeDecl folder inDocId ref =
        let extractDef (scope, sym) =
            match scope, Sym.asDef sym with
            | Scope.Doc docId, Some def -> Some(docId, def)
            | _ -> None

        let defs =
            Folder.conn folder
            |> Conn.Query.resolve (Scope.Doc inDocId, Sym.Ref ref)
            |> Seq.choose extractDef

        seq {
            for doc, def in defs do
                yield! findDefRefs includeDecl folder doc None def
        }

    /// Finds elements referencing `el`.
    /// When `el` is a link, it's resolved to its destination first and then references to the
    /// destination are found.
    let findElementRefs
        (includeDecl: bool)
        (folder: Folder)
        (srcDoc: Doc)
        (srcEl: Cst.Element)
        : seq<Doc * Cst.Element> =
        match Structure.tryFindSymbolForConcrete srcEl srcDoc.Structure with
        | None -> Seq.empty
        | Some sym ->
            let refs =
                match sym with
                | Sym.Tag tag -> findTagRefs includeDecl folder srcDoc.Id srcEl tag
                | Sym.Def def -> findDefRefs includeDecl folder srcDoc.Id (Some srcEl) def
                | Sym.Ref ref -> findRefRefs includeDecl folder srcDoc.Id ref

            refs |> Seq.sortBy (fun (d, e) -> d.Id, e.Range)
