module Marksman.Connections

open FSharpPlus.Operators

open Marksman.Misc
open Marksman.Ast
open Marksman.Names
open Marksman.Workspace
open Marksman.Index

let private filterDocsByName (folder: Folder) (name: InternName) : DocId[] =
    let byTitle: seq<DocId> =
        Folder.filterDocsBySlug (InternName.name name |> Slug.ofString) folder
        |> Seq.map Doc.id

    let byPath: seq<DocId> =
        InternName.tryAsPath name
        |> Option.map (fun path -> Folder.filterDocsByInternPath path folder |> Seq.map Doc.id)
        |> Option.defaultValue []

    Set.ofSeq (Seq.append byTitle byPath) |> Set.toArray

[<RequireQualifiedAccess>]
type Src =
    | WL of WikiLink
    | ML of MdLink
    | MR of MdRef

[<RequireQualifiedAccess>]
type Dest =
    | Doc
    | Head of Heading
    | LinkDef of MdLinkDef

module Src =
    let private tryFindDestDoc (folder: Folder) (srcDoc: DocId) (src: Src) =
        match src with
        | Src.WL { doc = None }
        | Src.ML { url = None }
        | Src.MR _ -> [| srcDoc |]
        | Src.WL { doc = Some doc } ->
            let name = InternName.mkUnchecked srcDoc doc
            filterDocsByName folder name
        | Src.ML { url = Some url } ->
            let exts = (Folder.configOrDefault folder).CoreMarkdownFileExtensions()

            match InternName.mkChecked exts srcDoc url with
            | Some name -> filterDocsByName folder name
            | None -> [||]

    let private tryFindDestInDoc
        (folder: Folder)
        (srcDoc: DocId)
        (src: Src)
        (destDoc: DocId)
        : seq<Dest> =
        let destDoc = Folder.findDocById destDoc folder
        let destIndex = Doc.index destDoc

        match src with
        | Src.WL { heading = None }
        | Src.ML { anchor = None } -> [ Dest.Doc ]
        | Src.WL { heading = Some heading }
        | Src.ML { anchor = Some heading } ->
            Index.filterHeadingBySlug (Slug.ofString heading) destIndex
            |> Seq.ofList
            |> Seq.map (fun x -> Heading.OfCst x.data |> Dest.Head)
        | Src.MR mdRef ->
            match
                Index.tryFindLinkDef mdRef.DestLabel destIndex
                |> Option.map (fun x -> MdLinkDef.OfCst x.data)
            with
            | Some linkDef -> [ Dest.LinkDef linkDef ]
            | None -> []

    let tryFindDest (folder: Folder) (srcDoc: DocId) (src: Src) : array<DocId * Dest> =
        let destDocs = tryFindDestDoc folder srcDoc src

        seq {
            for destDoc in destDocs do
                yield!
                    tryFindDestInDoc folder srcDoc src destDoc
                    |> Seq.map (fun x -> destDoc, x)
        }
        |> Array.ofSeq

type CGraph =
    { pointsTo: Map<DocId, Map<Src, Set<DocId * Dest>>>
      pointedBy: Map<DocId, Map<Dest, Set<DocId * Src>>>
      unresolved: Map<DocId, Set<Src>> }

module CGraph =
    let empty =
        { pointsTo = Map.empty
          pointedBy = Map.empty
          unresolved = Map.empty }

    let hasSrc docId src graph =
        Map.tryFind docId graph.pointsTo
        |> Option.map (Map.tryFind src)
        |> Option.isSome

    let hasDest docId dest graph =
        Map.tryFind docId graph.pointedBy
        |> Option.map (Map.tryFind dest)
        |> Option.isSome

    let addSrc (folder: Folder) (docId: DocId) (src: Src) (graph: CGraph) : CGraph =
        if hasSrc docId src graph then graph else failwith ""

    let addDest (folder: Folder) (docId: DocId) (dest: Dest) (graph: CGraph) : CGraph =
        if hasDest docId dest graph then graph else failwith ""

    let addDoc (folder: Folder) (docId: DocId) (graph: CGraph) : CGraph =
        addDest folder docId Dest.Doc graph

    let addElement (folder: Folder) (docId: DocId) (el: Element) (graph: CGraph) : CGraph =
        match el with
        | Element.H heading -> addDest folder docId (Dest.Head heading) graph
        | Element.MLD linkDef -> addDest folder docId (Dest.LinkDef linkDef) graph
        | Element.WL wikiLink -> addSrc folder docId (Src.WL wikiLink) graph
        | Element.ML mdLink -> addSrc folder docId (Src.ML mdLink) graph
        | Element.MR mdRef -> addSrc folder docId (Src.MR mdRef) graph
        | Element.T _ -> graph
