module Marksman.Graph

open Marksman.MMap
open Marksman.Misc

/// Undirected graph
type Graph<'V> when 'V: comparison = { vertices: Set<'V>; edges: MMap<'V, 'V>; normalize: bool }

type GraphDifference<'V> when 'V: comparison =
    { vertexDifference: Difference<'V>
      edgeDifference: Difference<'V * 'V> }

    member this.IsEmpty() =
        Difference.isEmpty this.vertexDifference
        && Difference.isEmpty this.edgeDifference

    member this.CompactFormat() =
        let lines =
            seq {
                if not (Difference.isEmpty this.vertexDifference) then
                    yield "Vertex difference:"
                    yield Indented(4, this.vertexDifference.CompactFormat()).ToString()

                if not (Difference.isEmpty this.edgeDifference) then
                    yield "Edge difference:"
                    yield Indented(4, this.edgeDifference.CompactFormat()).ToString()
            }

        concatLines lines

module Graph =
    let empty normalize = { vertices = Set.empty; edges = MMap.empty; normalize = normalize }

    let hasVertex v g = Set.contains v g.vertices

    let degree v g = MMap.tryFind v g.edges |> Option.map Set.count

    let isDegree0OrAbsent v g = (degree v g |> Option.defaultValue 0) = 0


    let addVertex v g =
        if hasVertex v g then
            g
        else
            { g with vertices = Set.add v g.vertices }

    let addEdge src dest g =
        let g = g |> addVertex src |> addVertex dest
        let edges = g.edges |> MMap.add src dest |> MMap.add dest src
        { g with edges = edges }

    let private removeVertexUnsafe v g = { g with vertices = Set.remove v g.vertices }

    let removeEdge src dest g =
        let edges =
            g.edges |> MMap.removeValue src dest |> MMap.removeValue dest src

        let g =
            if g.normalize then
                let g = if isDegree0OrAbsent src g then removeVertexUnsafe src g else g
                if isDegree0OrAbsent dest g then removeVertexUnsafe dest g else g
            else
                g

        { g with edges = edges }

    let removeVertexWithCallback cb v g =
        let edgesVs = MMap.tryFind v g.edges |> Option.defaultValue Set.empty

        let g =
            Set.fold
                (fun g dest ->
                    cb dest
                    g |> removeEdge v dest |> removeEdge dest v)
                g
                edgesVs

        removeVertexUnsafe v g

    let removeVertex v g = removeVertexWithCallback ignore v g

    let difference g1 g2 =
        let edges1 = g1.edges |> MMap.toSeq |> Set.ofSeq
        let edges2 = g2.edges |> MMap.toSeq |> Set.ofSeq

        { vertexDifference = Difference.mk g1.vertices g2.vertices
          edgeDifference = Difference.mk edges1 edges2 }
