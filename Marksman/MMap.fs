module Marksman.MMap

open Marksman.Misc

type MMap<'K, 'V> when 'K: comparison and 'V: comparison = MMap of Map<'K, Set<'V>>

type MMapDifference<'K, 'V> when 'K: comparison and 'V: comparison = {
    removedKeys: Set<'K>
    addedKeys: Set<'K>
    changedKeys: array<'K * Difference<'V>>
} with

    member this.IsEmpty() =
        Set.isEmpty this.removedKeys
        && Set.isEmpty this.addedKeys
        && Array.isEmpty this.changedKeys

    member this.CompactFormat() =
        let lines =
            seq {
                if not (Set.isEmpty this.removedKeys) then
                    yield "Removed keys:"

                    for key in this.removedKeys do
                        yield Indented(4, key).ToString()

                if not (Set.isEmpty this.addedKeys) then
                    yield "Added keys:"

                    for key in this.addedKeys do
                        yield Indented(4, key).ToString()

                if not (Array.isEmpty this.changedKeys) then
                    yield "Changed keys:"

                    for key, diff in this.changedKeys do
                        yield Indented(4, key).ToString()
                        yield Indented(4, diff.CompactFormat()).ToString()
            }

        concatLines lines

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

    let ofSeq seq =
        Seq.groupBy fst seq
        |> Seq.map (fun (k, els) -> k, Seq.map snd els |> Set.ofSeq)
        |> Map.ofSeq
        |> MMap

    let tryFind k (MMap m) = Map.tryFind k m

    let find k (MMap m) = Map.find k m

    let containsKey k (MMap m) = Map.containsKey k m

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
            for KeyValue(k, vs) in m do
                for v in vs do
                    yield (k, v)
        }

    let toSetSeq (MMap m) = Map.toSeq m

    let difference (MMap m1) (MMap m2) =
        let ks1 = Map.keys m1 |> Set.ofSeq
        let ks2 = Map.keys m2 |> Set.ofSeq

        let same = Set.intersect ks1 ks2
        let removed = ks1 - same
        let added = ks2 - same

        let byKeyDiff =
            seq {
                for k in same do
                    let s1 = Map.find k m1
                    let s2 = Map.find k m2
                    let diff = Difference.mk s1 s2

                    if not (Difference.isEmpty diff) then
                        yield k, diff
            }
            |> Array.ofSeq

        { addedKeys = added; removedKeys = removed; changedKeys = byKeyDiff }
