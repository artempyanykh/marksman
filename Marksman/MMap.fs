module Marksman.MMap

open Marksman.Misc

type MMap<'K, 'V> when 'K: comparison and 'V: comparison =
    | MMap of Map<'K, Set<'V>>

    member private this.Inner = let (MMap map) = this in map

    member this.TryFind(k: 'K) = this.Inner.TryFind(k)

    member this.Add(k: 'K, v: 'V) : MMap<'K, 'V> =
        let valueSet =
            this.TryFind(k) |> Option.defaultValue Set.empty |> Set.add v

        this.Inner.Add(k, valueSet) |> MMap

    member this.RemoveValue(k: 'K, v: 'V) : MMap<'K, 'V> =
        this.TryFind(k)
        |> Option.map (fun valueSet ->
            let valueSet = valueSet.Remove(v)

            if valueSet.IsEmpty then
                this.Inner.Remove(k)
            else
                this.Inner.Add(k, valueSet))
        |> Option.defaultValue this.Inner
        |> MMap

    member this.IsEmpty = this.Inner.IsEmpty

    static member OfSeq(seq: seq<'K * 'V>) : MMap<'K, 'V> =
        Seq.groupBy fst seq
        |> Seq.map (fun (k, els) -> k, Seq.map snd els |> Set.ofSeq)
        |> Map.ofSeq
        |> MMap

    member this.Find(k: 'K) = Map.find k this.Inner

    member this.ContainsKey(k) = Map.containsKey k this.Inner

    member this.AddEmpty(k) = Map.add k Set.empty this.Inner |> MMap

    member this.FoldSet(f, state) = Map.fold f state this.Inner

    member this.Fold(f, state) =
        let f acc k vs = Set.fold (flip f k) acc vs
        Map.fold f state this.Inner

    member this.MapKeys(f) =
        let m =
            Map.fold (fun acc k vs -> Map.add (f k) vs acc) Map.empty this.Inner

        MMap m

    member this.Iter(f) = this.Inner |> Map.iter (f >> Set.iter)

    member this.ToSeq =
        seq {
            for KeyValue(k, vs) in this.Inner do
                for v in vs do
                    yield (k, v)
        }

    member this.ToSetSeq = Map.toSeq this.Inner

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
    let add k v (mm: MMap<'K, 'V>) = mm.Add(k, v)

    let removeValue k v (mm: MMap<'K, 'V>) = mm.RemoveValue(k, v)

    let isEmpty (mm: MMap<'K, 'V>) = mm.IsEmpty

    let empty = MMap(Map.empty)

    let ofSeq seq = MMap.OfSeq(seq)

    let tryFind k (mm: MMap<'K, 'V>) = mm.TryFind(k)

    let find k (mm: MMap<'K, 'V>) = mm.Find(k)

    let containsKey k (mm: MMap<'K, 'V>) = mm.ContainsKey(k)

    let addEmpty k (mm: MMap<'K, 'V>) = mm.AddEmpty(k)

    let foldSet f state (mm: MMap<'K, 'V>) = mm.FoldSet(f, state)

    let fold f state (mm: MMap<'K, 'V>) = mm.Fold(f, state)

    let mapKeys f (mm: MMap<'K, 'V>) = mm.MapKeys(f)

    let iter f (mm: MMap<'K, 'V>) = mm.Iter(f)

    let toSeq (mm: MMap<'K, 'V>) = mm.ToSeq

    let toSetSeq (mm: MMap<'K, 'V>) = mm.ToSetSeq

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
