module Marksman.MMap

open Marksman.Misc

type MMap<'K, 'V> when 'K: comparison and 'V: comparison = MMap of Map<'K, Set<'V>>

module MMap =
    let add k v (MMap map) =
        let valueSet =
            Map.tryFind k map |> Option.defaultValue Set.empty |> Set.add v

        Map.add k valueSet map |> MMap

    let removeValue k v (MMap map) =
        Map.tryFind k map
        |> Option.map (fun valueSet ->
            let valueSet = Set.remove v valueSet

            if Set.isEmpty valueSet then
                Map.remove k map
            else
                Map.add k valueSet map)
        |> Option.defaultValue map
        |> MMap

    let isEmpty (MMap inner) = Map.isEmpty inner

    let empty = MMap(Map.empty)

    let tryFind k (MMap m) = Map.tryFind k m

    let addEmpty k (MMap m) = Map.add k Set.empty m |> MMap

    let foldSet f state (MMap m) = Map.fold f state m

    let fold f state (MMap m) =
        let f acc k vs = Set.fold (flip f k) acc vs
        Map.fold f state m

    let mapKeys f (MMap m) =
        let m = Map.fold (fun acc k vs -> Map.add (f k) vs acc) Map.empty m
        MMap m

    let iter f (MMap m) = m |> Map.iter (f >> Set.iter)

    let toSeq (MMap m) =
        seq {
            for KeyValue (k, vs) in m do
                for v in vs do
                    yield (k, v)
        }

    let toSetSeq (MMap m) = Map.toSeq m
