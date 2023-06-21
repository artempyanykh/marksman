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

    let private tryFindDestInDoc (folder: Folder) (src: Src) (destDoc: DocId) : seq<Dest> =
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
                yield! tryFindDestInDoc folder src destDoc |> Seq.map (fun x -> destDoc, x)
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

    let private hasSrc docId src graph =
        Map.tryFind docId graph.pointsTo
        |> Option.map (Map.tryFind src)
        |> Option.isSome

    let private hasDest docId dest graph =
        Map.tryFind docId graph.pointedBy
        |> Option.map (Map.tryFind dest)
        |> Option.isSome

    let private addUnresolved docId src graph =
        let insertFn setOpt = Option.defaultValue Set.empty setOpt |> Set.add src |> Some
        let unresolved = Map.change docId insertFn graph.unresolved
        { graph with unresolved = unresolved }


    let private addEdges (srcDoc: DocId, src: Src) (destDoc: DocId, dest: Dest) (graph: CGraph) =
        let addForwardEdge srcToDestOpt =
            let srcToDest = Option.defaultValue Map.empty srcToDestOpt

            let srcToDest =
                srcToDest
                |> Map.change src (fun destSetOpt ->
                    Option.defaultValue Set.empty destSetOpt
                    |> Set.add (destDoc, dest)
                    |> Some)

            Some srcToDest

        let pointsTo = graph.pointsTo |> Map.change srcDoc addForwardEdge

        let addBackwardEdge destToSrcOpt =
            let destToSrc = Option.defaultValue Map.empty destToSrcOpt

            let destToSrc =
                destToSrc
                |> Map.change dest (fun srcSetOpt ->
                    Option.defaultValue Set.empty srcSetOpt
                    |> Set.add (srcDoc, src)
                    |> Some)

            Some destToSrc

        let pointedBy = graph.pointedBy |> Map.change destDoc addBackwardEdge

        { graph with pointsTo = pointsTo; pointedBy = pointedBy }

    let private addSrcSkipUnresolved
        (folder: Folder)
        (srcDoc: DocId)
        (src: Src)
        (graph: CGraph)
        : bool * CGraph =
        if hasSrc srcDoc src graph then
            false, graph
        else
            let destinations = Src.tryFindDest folder srcDoc src

            if Array.isEmpty destinations then
                false, addUnresolved srcDoc src graph
            else
                let shouldResolve =
                    Array.exists (fun (doc, dest) -> hasDest doc dest graph |> not) destinations

                let connectSrc graph (destDoc, destInDoc) =
                    addEdges (srcDoc, src) (destDoc, destInDoc) graph

                let graph = Array.fold connectSrc graph destinations

                shouldResolve, graph

    let private addDestSkipUnresolved (docId: DocId) (dest: Dest) (graph: CGraph) : bool * CGraph =
        if hasDest docId dest graph then
            false, graph
        else
            let addDestFn mapOpt =
                Option.defaultValue Map.empty mapOpt |> Map.add dest Set.empty |> Some

            let newPointedBy = Map.change docId addDestFn graph.pointedBy
            true, { graph with pointedBy = newPointedBy }

    let private resolveUnresolved folder graph =
        // Now that we have a new dest, let's try to resolve previously unresolved links
        let unresolved = graph.unresolved
        let graph = { graph with unresolved = Map.empty }

        let resolveFn graph srcDoc srcSet =
            let resolveFn' graph src = addSrcSkipUnresolved folder srcDoc src graph |> snd
            Set.fold resolveFn' graph srcSet

        Map.fold resolveFn graph unresolved

    let private addDest (folder: Folder) (docId: DocId) (dest: Dest) (graph: CGraph) : CGraph =
        let shouldResolve, graph = addDestSkipUnresolved docId dest graph

        let graph =
            if shouldResolve then resolveUnresolved folder graph else graph

        graph

    let private addSrc (folder: Folder) (srcDoc: DocId) (src: Src) (graph: CGraph) : CGraph =
        let shouldResolve, graph = addSrcSkipUnresolved folder srcDoc src graph

        let graph =
            if shouldResolve then resolveUnresolved folder graph else graph

        graph

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

    let removeDoc (docId: DocId) (graph: CGraph) : CGraph = failwith "todo"

    let removeElement (docId: DocId) (el: Element) (graph: CGraph) : CGraph = failwith "todo"
