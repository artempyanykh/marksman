module Marksman.PartitionedMapTests

open System
open Xunit

open Marksman.PartitionedMap

[<Fact>]
let differencesMatchOrdinaryMapsAcrossSnapshots () =
    let random = Random 42
    let mutable expected = Map.empty<int, int>
    let mutable actual = PartitionedMap.empty<int, int>
    let snapshots = ResizeArray<Map<int, int> * PartitionedMap<int, int>>()
    snapshots.Add((expected, actual))

    for step in 1..300 do
        let key = random.Next(200)

        if random.Next(3) = 0 then
            expected <- Map.remove key expected
            actual <- PartitionedMap.remove key actual
        else
            let value = random.Next(20)
            expected <- Map.add key value expected
            actual <- PartitionedMap.add key value actual

        if step % 50 = 0 then
            snapshots.Add((expected, actual))
            Assert.Equal<Map<int, int>>(expected, Map.ofSeq (PartitionedMap.toSeq actual))

    for oldMap, oldParts in snapshots do
        for newMap, newParts in snapshots do
            let keys =
                Set.union (Map.keys oldMap |> Set.ofSeq) (Map.keys newMap |> Set.ofSeq)

            let expectedChanges =
                keys
                |> Seq.choose (fun key ->
                    let before = Map.tryFind key oldMap
                    let after = Map.tryFind key newMap
                    if before = after then None else Some(key, before, after))
                |> Set.ofSeq

            let actualChanges = ResizeArray<_>()

            PartitionedMap.iterDifferences
                (fun key before after -> actualChanges.Add((key, before, after)))
                oldParts
                newParts

            Assert.Equal<Set<int * int option * int option>>(
                expectedChanges,
                Set.ofSeq actualChanges
            )

    let withValue = PartitionedMap.add 42 123 actual
    let unchanged = PartitionedMap.add 42 123 withValue
    Assert.True(obj.ReferenceEquals(withValue, unchanged))
