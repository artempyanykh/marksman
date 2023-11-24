module Marksman.Conn

open Ionide.LanguageServerProtocol.Logging

open Marksman.Misc
open Marksman.MMap
open Marksman.Names
open Marksman.Graph
open Marksman.Sym


type Oracle =
    { resolveToScope: Scope -> Ref -> Scope[]
      resolveInScope: Ref -> Scope -> Def[] }

module ScopedSym =
    let asRef (scope, sym) = Sym.asRef sym |> Option.map (fun link -> scope, link)
    let asDef (scope, sym) = Sym.asDef sym |> Option.map (fun def -> scope, def)
    let asTag (scope, sym) = Sym.asTag sym |> Option.map (fun tag -> scope, tag)

type UnresolvedScope =
    | FullyUnknown
    | InScope of Scope

[<RequireQualifiedAccess>]
type Unresolved =
    | Ref of Scope * Ref
    | Scope of UnresolvedScope

    member this.CompactFormat() =
        match this with
        | Ref (scope, ref) -> $"{ref.CompactFormat()} @ {scope}"
        | Scope FullyUnknown -> "FullyUnknown"
        | Scope (InScope scope) -> $"{scope}"

type Conn =
    { refs: MMap<Scope, Ref>
      defs: MMap<Scope, Def>
      tags: MMap<Scope, Tag>
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
                if this.refs |> MMap.isEmpty |> not then
                    yield "Refs:"

                    for scope, links in MMap.toSetSeq this.refs do
                        yield $"  {scope}:"

                        for link in links do
                            yield Indented(4, link.CompactFormat()).ToString()

                if this.defs |> MMap.isEmpty |> not then
                    yield "Defs:"

                    for scope, defs in MMap.toSetSeq this.defs do
                        yield $"  {scope}:"

                        for def in defs do
                            yield Indented(4, def.CompactFormat()).ToString()

                if this.tags |> MMap.isEmpty |> not then
                    yield "Tags:"

                    for _, tag in MMap.toSeq this.tags do
                        yield Indented(4, tag.CompactFormat()).ToString()

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
        { refs = MMap.empty
          defs = MMap.empty
          tags = MMap.empty
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

        let mutable { refs = refs
                      defs = defs
                      tags = tags
                      resolved = resolved
                      unresolved = unresolved } =
            conn

        let mutable lastTouched = Set.empty
        let mutable toResolveSet = Set.empty

        let addUnresolvedRefToQueue =
            function
            | Unresolved.Ref (scope, ref) -> toResolveSet <- Set.add (scope, ref) toResolveSet
            | Unresolved.Scope _ -> ()

        // First we remove all links to avoid re-resolving links that are no longer part of the graph
        let removedRefs = removed |> Set.toSeq |> Seq.choose ScopedSym.asRef

        for scope, ref in removedRefs do
            let scopedSym = (scope, Sym.Sym.Ref ref)
            // Add removed link to lastTouched because removing a broken link can affect the diagnostic
            lastTouched <- Set.add scopedSym lastTouched

            refs <- MMap.removeValue scope ref refs
            resolved <- Graph.removeVertex scopedSym resolved
            unresolved <- Graph.removeVertex (Unresolved.Ref(scope, ref)) unresolved

        let removedTags = removed |> Set.toSeq |> Seq.choose ScopedSym.asTag

        for scope, tag in removedTags do
            let scopedSym = (scope, Sym.Sym.Tag tag)
            lastTouched <- Set.add scopedSym lastTouched

            tags <- MMap.removeValue scope tag tags
            resolved <- Graph.removeVertex scopedSym resolved

        // Then we remove all defs. When we remove a def:
        // 1. some previously resolved links could become broken; we need to remember them and
        //    re-run the resolution,
        // 2. some links that were previously pointing to 2 defs (=~ ambiguous link diagnostic)
        //    may start pointing to a single def, and hence affect diagnostics.
        // This means that we need to collect everything that was pointing to the def, and re-run
        // link resolution using an oracle.
        let removedDefs = removed |> Set.toSeq |> Seq.choose ScopedSym.asDef

        for scope, def in removedDefs do
            let scopedSym = (scope, Sym.Sym.Def def)
            lastTouched <- Set.add scopedSym lastTouched

            let cb =
                function
                | scope, Sym.Ref link -> toResolveSet <- Set.add (scope, link) toResolveSet
                | _, Sym.Def _
                | _, Sym.Tag _ -> ()

            defs <- MMap.removeValue scope def defs
            resolved <- Graph.removeVertexWithCallback cb scopedSym resolved

            // When the doc is removed we need to remove all unresolved links within this doc's scope
            match def with
            | Def.Doc ->
                unresolved <- Graph.removeVertex (Unresolved.Scope(InScope scope)) unresolved
            | _ -> ()

        let mutable docWasAdded = false

        for scope, sym as scopedSym in added do
            lastTouched <- Set.add scopedSym lastTouched

            match sym with
            | Sym.Ref link -> toResolveSet <- Set.add (scope, link) toResolveSet
            | Sym.Def def ->
                defs <- MMap.add scope def defs
                resolved <- Graph.addVertex scopedSym resolved

                if Def.isDoc def then
                    docWasAdded <- true

                // When we add a new def, things that were previously unresolved within the scope
                // could become resolvable
                unresolved <-
                    Graph.removeVertexWithCallback
                        addUnresolvedRefToQueue
                        (Unresolved.Scope(InScope scope))
                        unresolved
            | Sym.Tag tag ->
                tags <- MMap.add scope tag tags
                // We can resolve tag right away into the Global scope
                resolved <- Graph.addEdge scopedSym (Scope.Global, Sym.Sym.Tag tag) resolved

        // Finally, if any new docs were added we need to re-resolve all previously unresolved links
        // within the global scope
        if docWasAdded then
            unresolved <-
                Graph.removeVertexWithCallback
                    addUnresolvedRefToQueue
                    (Unresolved.Scope(FullyUnknown))
                    unresolved

        // Now we run link resolution while updating both resolved and unresolved graphs
        for scope, link in toResolveSet do
            let srcSym = (scope, Sym.Sym.Ref link)
            lastTouched <- Set.add srcSym lastTouched
            refs <- MMap.add scope link refs

            let targetScopes = oracle.resolveToScope scope link

            if Array.isEmpty targetScopes then
                unresolved <-
                    Graph.addEdge
                        (Unresolved.Ref(scope, link))
                        (Unresolved.Scope(FullyUnknown))
                        unresolved

            for targetScope in targetScopes do
                let targetDefs = oracle.resolveInScope link targetScope

                if Array.isEmpty targetDefs then
                    unresolved <-
                        Graph.addEdge
                            (Unresolved.Ref(scope, link))
                            (Unresolved.Scope(InScope targetScope))
                            unresolved

                for targetDef in targetDefs do
                    let targetSym = (targetScope, Sym.Sym.Def targetDef)
                    lastTouched <- Set.add targetSym lastTouched
                    resolved <- Graph.addEdge srcSym targetSym resolved

        logger.trace (
            Log.setMessage "Finished updating conn"
            >> Log.addContext "#touched" (Set.count lastTouched)
        )

        { refs = refs
          defs = defs
          tags = tags
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
