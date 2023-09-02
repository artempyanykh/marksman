module Marksman.MMap

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

    let fold f state (MMap m) = Map.fold f state m

type MMap2<'K1, 'K2, 'V> when 'K1: comparison and 'K2: comparison and 'V: comparison =
    | MMap2 of Map<'K1, MMap<'K2, 'V>>

module MMap2 =
    let add (outerKey, innerKey) v (MMap2 map) =
        let innerMap = Map.tryFind outerKey map |> Option.defaultValue MMap.empty

        let innerMap = MMap.add innerKey v innerMap
        Map.add outerKey innerMap map |> MMap2

    let addEmpty (outerKey, innerKey) (MMap2 map as mm2) =
        match Map.tryFind outerKey map with
        | None -> mm2
        | Some innerMap -> Map.add outerKey (MMap.addEmpty innerKey innerMap) map |> MMap2


    let removeValue (outerKey, innerKey) v (MMap2 map as mm2) =
        match Map.tryFind outerKey map with
        | None -> mm2
        | Some innerMap ->
            let innerMap = MMap.removeValue innerKey v innerMap

            let map =
                if MMap.isEmpty innerMap then
                    Map.remove outerKey map
                else
                    Map.add outerKey innerMap map

            MMap2(map)

    let empty = MMap2 Map.empty

    let isEmpty (MMap2 map) = Map.isEmpty map

    let tryFind (outerKey, innerKey) (MMap2 map) =
        Map.tryFind outerKey map |> Option.bind (MMap.tryFind innerKey)
