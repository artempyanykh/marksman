module Marksman.PartitionedMap

open Marksman.Misc

/// An immutable map split into stable partitions. Updating one key preserves
/// the identity of every other partition, so two snapshots can compare only
/// the partitions that differ. The outer array is copied on update; compared
/// with a Map-backed outer index, updates were similar but snapshot diffing
/// was faster in the benchmark.
type PartitionedMap<'Key, 'Value when 'Key: comparison> = private {
    partitions: Map<'Key, 'Value>[]
}

module PartitionedMap =
    // Updating copies 64 map roots; a changed partition then contains about
    // N/64 values if hashes are spread evenly. This beat 16 roots at 1,000
    // documents, but comparison still grows with each partition at larger N.
    [<Literal>]
    let private partitionCount = 64

    let empty<'Key, 'Value when 'Key: comparison> : PartitionedMap<'Key, 'Value> = {
        partitions = Array.init partitionCount (fun _ -> Map.empty)
    }

    let private partition key = hash key &&& (partitionCount - 1)

    let tryFind key map =
        let index = partition key
        Map.tryFind key map.partitions[index]

    let containsKey key map =
        let index = partition key
        Map.containsKey key map.partitions[index]

    let add key value map =
        let index = partition key
        let entries = map.partitions[index]

        if Map.tryFind key entries = Some value then
            map
        else
            let partitions = Array.copy map.partitions
            partitions[index] <- Map.add key value entries
            { partitions = partitions }

    let remove key map =
        let index = partition key

        if Map.containsKey key map.partitions[index] then
            let partitions = Array.copy map.partitions
            partitions[index] <- Map.remove key partitions[index]
            { partitions = partitions }
        else
            map

    let toSeq map =
        seq {
            for partition in map.partitions do
                yield! Map.toSeq partition
        }

    /// Visit keys whose values differ, including additions and removals.
    /// Unchanged partitions are skipped by identity; changed partitions are
    /// compared exactly, so unrelated hash collisions cannot change results.
    let iterDifferences visit before after =
        for index in 0 .. partitionCount - 1 do
            let oldPartition = before.partitions[index]
            let newPartition = after.partitions[index]

            if not (obj.ReferenceEquals(oldPartition, newPartition)) then
                SortedMerge.iter
                    (Map.toSeq oldPartition)
                    (Map.toSeq newPartition)
                    (fun key value -> visit key (Some value) None)
                    (fun key value -> visit key None (Some value))
                    (fun key oldValue newValue ->
                        if
                            not (obj.ReferenceEquals(oldValue, newValue))
                            && oldValue <> newValue
                        then
                            visit key (Some oldValue) (Some newValue))
