module Marksman.Conn

open Ionide.LanguageServerProtocol.Logging

open Marksman.Misc
open Marksman.MMap
open Marksman.Ast
open Marksman.Names
open Marksman.Graph

[<RequireQualifiedAccess>]
type Link =
    | WL of WikiLink
    | ML of MdLink
    | MR of MdRef
    | T of Tag

    member this.AsElement() =
        match this with
        | WL wl -> Element.WL wl
        | ML ml -> Element.ML ml
        | MR mr -> Element.MR mr
        | T t -> Element.T t

    member this.CompactFormat() = this.AsElement().CompactFormat()


[<RequireQualifiedAccess>]
type Def =
    | Doc
    | H of Heading
    | LD of MdLinkDef
    | T of Tag

    member this.CompactFormat() =
        match this with
        | Doc -> "Doc"
        | H h -> h.CompactFormat()
        | LD ld -> ld.CompactFormat()
        | T (Tag name) -> $"#{name}"

module Def =
    let isDoc =
        function
        | Def.Doc -> true
        | _ -> false

[<RequireQualifiedAccess>]
[<StructuredFormatDisplay("{CompactFormat}")>]
type Scope =
    | Doc of DocId
    | Tag

    override this.ToString() =
        match this with
        | Doc docId -> $"{docId}"
        | Tag -> "Tag"

    member this.CompactFormat = this.ToString()

[<RequireQualifiedAccess>]
type Sym =
    | Link of Link
    | Def of Def

    member this.CompactFormat() =
        match this with
        | Link link -> link.CompactFormat()
        | Def def -> def.CompactFormat()


type ScopedSym = Scope * Sym

module Sym =
    let asLink =
        function
        | Sym.Link link -> Some link
        | Sym.Def _ -> None

    let asDef =
        function
        | Sym.Def def -> Some def
        | Sym.Link _ -> None

    let ofElement (el: Ast.Element) : Sym =
        match el with
        | Ast.Element.H h -> Sym.Def(Def.H h)
        | Ast.Element.T t -> Sym.Def(Def.T t)
        | Ast.Element.ML ml -> Sym.Link(Link.ML ml)
        | Ast.Element.MR mr -> Sym.Link(Link.MR mr)
        | Ast.Element.WL wl -> Sym.Link(Link.WL wl)
        | Ast.Element.MLD ld -> Sym.Def(Def.LD ld)

    let scoped (scope: Scope) (sym: Sym) = scope, sym

    let scopedToDoc (docId: DocId) (sym: Sym) = Scope.Doc docId, sym

    let allScopedToDoc (docId: DocId) (syms: seq<Sym>) = syms |> Seq.map (scopedToDoc docId)


type Oracle =
    { resolveToScope: Scope -> Link -> Scope[]
      resolveInScope: Link -> Scope -> Def[] }

module ScopedSym =
    let asLink (scope, sym) = Sym.asLink sym |> Option.map (fun link -> scope, link)
    let asDef (scope, sym) = Sym.asDef sym |> Option.map (fun def -> scope, def)

type UnresolvedScope =
    | Global
    | InScope of Scope

[<RequireQualifiedAccess>]
type Unresolved =
    | Link of Scope * Link
    | Scope of UnresolvedScope

    member this.CompactFormat() =
        match this with
        | Link (scope, link) -> $"{link.CompactFormat()} @ {scope}"
        | Scope Global -> "Global"
        | Scope (InScope scope) -> $"{scope}"

type Conn =
    { links: MMap<Scope, Link>
      defs: MMap<Scope, Def>
      resolved: Graph<ScopedSym>
      unresolved: Graph<Unresolved>
      lastTouched: Set<ScopedSym> }

    member private this.ResolvedCompactFormat() =
        let byScope =
            this.resolved.edges
            |> MMap.toSetSeq
            |> Seq.groupBy (fun ((scope, _), _) -> scope)

        let lines =
            seq {
                for scope, edges in byScope do
                    yield $"{scope}:"

                    for (_, sym), dests in edges do
                        for destScope, destSym in dests do
                            yield
                                Indented(
                                    2,
                                    $"{sym.CompactFormat()} -> {destSym.CompactFormat()} @ {destScope}"
                                )
                                    .ToString()
            }

        concatLines lines

    member private this.UnresolvedCompactFormat() =
        let edges = this.unresolved.edges |> MMap.toSeq

        let lines =
            seq {
                for start, end_ in edges do
                    yield $"{start.CompactFormat()} -> {end_.CompactFormat()}".ToString()
            }

        concatLines lines

    member this.CompactFormat() =
        let lines =
            seq {
                yield "Links:"

                for scope, links in MMap.toSetSeq this.links do
                    yield $"  {scope}:"

                    for link in links do
                        yield Indented(4, link.CompactFormat()).ToString()

                yield "Defs:"

                for scope, defs in MMap.toSetSeq this.defs do
                    yield $"  {scope}:"

                    for def in defs do
                        yield Indented(4, def.CompactFormat()).ToString()

                yield "Resolved:"
                yield Indented(2, this.ResolvedCompactFormat()).ToString()
                yield "Unresolved:"
                yield Indented(2, this.UnresolvedCompactFormat()).ToString()
                yield "Last touched:"

                for scope, sym in this.lastTouched do
                    yield Indented(2, $"{sym.CompactFormat()} @ {scope}").ToString()
            }

        concatLines lines

module Conn =
    let private logger = LogProvider.getLoggerByName "Conn"

    let empty =
        { links = MMap.empty
          defs = MMap.empty
          resolved = Graph.empty
          unresolved = Graph.empty
          lastTouched = Set.empty }

    let updateAux
        (oracle: Oracle)
        ({ added = added; removed = removed }: Difference<ScopedSym>)
        (conn: Conn)
        : Conn =
        logger.trace (
            Log.setMessage "update: started"
            >> Log.addContext "#added" (Set.count added)
            >> Log.addContext "#removed" (Set.count removed)
        )

        let mutable { links = links
                      defs = defs
                      resolved = resolved
                      unresolved = unresolved } =
            conn

        let mutable lastTouched = Set.empty
        let mutable toResolveSet = Set.empty

        let addUnresolvedLinkToQueue =
            function
            | Unresolved.Link (scope, link) -> toResolveSet <- Set.add (scope, link) toResolveSet
            | _ -> ()

        // First we remove all links to avoid re-resolving links that are no longer part of the graph
        let removedLinks = removed |> Set.toSeq |> Seq.choose ScopedSym.asLink

        for scope, link in removedLinks do
            let scopedSym = (scope, Sym.Link link)
            links <- MMap.removeValue scope link links
            resolved <- Graph.removeVertex scopedSym resolved
            unresolved <- Graph.removeVertex (Unresolved.Link(scope, link)) unresolved
            // Add removed link to lastTouched because removing a broken link can affect the diagnostic
            lastTouched <- Set.add scopedSym lastTouched

        // Then we remove all defs. When we remove a def:
        // 1. some previously resolved links could become broken; we need to remember them and
        //    re-run the resolution,
        // 2. some links that were previously pointing to 2 defs (=~ ambiguous link diagnostic)
        //    may start pointing to a single def, and hence affect diagnostics.
        // This means that we need to collect everything that was pointing to the def, and re-run
        // link resolution using an oracle.
        let removedDefs = removed |> Set.toSeq |> Seq.choose ScopedSym.asDef

        for scope, def in removedDefs do
            let scopedSym = (scope, Sym.Def def)

            let cb =
                function
                | scope, Sym.Link link -> toResolveSet <- Set.add (scope, link) toResolveSet
                | _, Sym.Def _ -> ()

            defs <- MMap.removeValue scope def defs
            resolved <- Graph.removeVertexWithCallback cb scopedSym resolved

            // When the doc is removed we need to remove all unresolved links within this doc's scope
            match def with
            | Def.Doc ->
                unresolved <- Graph.removeVertex (Unresolved.Scope(InScope scope)) unresolved
            | _ -> ()

        let mutable docWasAdded = false

        for scope, sym as scopedSym in added do
            match sym with
            | Sym.Link link -> toResolveSet <- Set.add (scope, link) toResolveSet
            | Sym.Def def ->
                defs <- MMap.add scope def defs
                resolved <- Graph.addVertex scopedSym resolved

                if Def.isDoc def then
                    docWasAdded <- true

                // When we add a new def, things that were previously unresolved within the scope
                // could become resolvable
                unresolved <-
                    Graph.removeVertexWithCallback
                        addUnresolvedLinkToQueue
                        (Unresolved.Scope(InScope scope))
                        unresolved

        // Finally, if any new docs were added we need to re-resolve all previously unresolved links
        // within the global scope
        if docWasAdded then
            unresolved <-
                Graph.removeVertexWithCallback
                    addUnresolvedLinkToQueue
                    (Unresolved.Scope(Global))
                    unresolved

        // Now we run link resolution while updating both resolved and unresolved graphs
        for scope, link in toResolveSet do
            links <- MMap.add scope link links
            lastTouched <- Set.add (scope, Sym.Link link) lastTouched

            let targetScopes = oracle.resolveToScope scope link

            if Array.isEmpty targetScopes then
                unresolved <-
                    Graph.addEdge
                        (Unresolved.Link(scope, link))
                        (Unresolved.Scope(Global))
                        unresolved

            for targetScope in targetScopes do
                let targetDefs = oracle.resolveInScope link targetScope

                if Array.isEmpty targetDefs then
                    unresolved <-
                        Graph.addEdge
                            (Unresolved.Link(scope, link))
                            (Unresolved.Scope(InScope targetScope))
                            unresolved

                for targetDef in targetDefs do
                    resolved <-
                        Graph.addEdge
                            (scope, Sym.Link link)
                            (targetScope, Sym.Def targetDef)
                            resolved

        logger.trace (
            Log.setMessage "Finished updating conn"
            >> Log.addContext "#touched" (Set.count lastTouched)
        )

        { links = links
          defs = defs
          resolved = resolved
          unresolved = unresolved
          lastTouched = lastTouched }

    let update oracle diff conn =
        if Difference.isEmpty diff then
            logger.trace (Log.setMessage "update: skipping empty diff")
            conn
        else
            updateAux oracle diff conn

    let mk (oracle: Oracle) (symMap: MMap<DocId, Sym>) : Conn =
        // Backtrace can be helpful in tracking down when full rebuilds are requested
        // let trace = System.Diagnostics.StackTrace()
        logger.trace (
            Log.setMessage "mk: full rebuild started"
        // >> Log.addContext "backtrace" (trace.ToString())
        )

        let added =
            MMap.fold (fun acc docId sym -> Set.add (Scope.Doc docId, sym) acc) Set.empty symMap

        update oracle { added = added; removed = Set.empty } empty
