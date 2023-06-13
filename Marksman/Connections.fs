module Marksman.Connections

open Marksman.Ast
open Marksman.Names

[<RequireQualifiedAccess>]
type Src =
    | WL of WikiLink
    | ML of MdLink
    | MR of MdRef

[<RequireQualifiedAccess>]
type Dest =
    | Doc
    | Head of DocId * Heading
    | LinkDef of DocId * MdLinkDef

type CGraph =
    { pointsTo: Map<DocId, Map<Src, Set<Dest>>>
      pointedBy: Map<DocId, Map<Dest, Set<Src>>>
      unresolved: Map<DocId, Set<Src>> }

module CGraph =
    let empty =
        { pointsTo = Map.empty
          pointedBy = Map.empty
          unresolved = Map.empty }

    let addVertex v graph = graph

    let rmVertex v graph = graph

    let addEdge edge graph = graph

    let rmEdge edge graph = graph
