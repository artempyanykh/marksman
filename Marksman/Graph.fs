module Marksman.Graph

open Marksman.MMap
open Marksman.Misc

/// Undirected graph
type Graph<'V> when 'V: comparison = { edges: MMap<'V, 'V> }

type GraphDifference<'V> when 'V: comparison =
    { edgeDifference: Difference<'V * 'V> }

    member this.IsEmpty() = Difference.isEmpty this.edgeDifference

    member this.CompactFormat() =
        let lines =
            seq {
                if not (Difference.isEmpty this.edgeDifference) then
                    yield "Edge difference:"
                    yield Indented(4, this.edgeDifference.CompactFormat()).ToString()
            }

        concatLines lines

module Graph =
    let empty = { edges = MMap.empty }

    let isEmpty g = MMap.isEmpty g.edges

    let hasVertex v g = MMap.containsKey v g.edges

    let degree v g = MMap.tryFind v g.edges |> Option.map Set.count

    let isDegree0OrAbsent v g = (degree v g |> Option.defaultValue 0) = 0

    let edges v g = g.edges |> MMap.tryFind v |> Option.defaultValue Set.empty

    let addEdge src dest g =
        let edges = g.edges |> MMap.add src dest |> MMap.add dest src
        { g with edges = edges }

    let removeEdge src dest g =
        let edges =
            g.edges |> MMap.removeValue src dest |> MMap.removeValue dest src

        { g with edges = edges }

    let removeVertexWithCallback cb v g =
        let edgesVs = MMap.tryFind v g.edges |> Option.defaultValue Set.empty

        Set.fold
            (fun g dest ->
                cb dest
                g |> removeEdge v dest |> removeEdge dest v)
            g
            edgesVs


    let removeVertex v g = removeVertexWithCallback ignore v g

    let difference g1 g2 =
        let edges1 = g1.edges |> MMap.toSeq |> Set.ofSeq
        let edges2 = g2.edges |> MMap.toSeq |> Set.ofSeq

        { edgeDifference = Difference.mk edges1 edges2 }
