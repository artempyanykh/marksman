module Marksman.MMapTests

open Xunit

open Marksman.MMap

module DifferenceTests =
    [<Fact>]
    let keyIntersection () =
        let m1 = MMap.ofSeq [ "a", 1 ]
        let m2 = MMap.ofSeq [ "b", 2 ]
        let diff = MMap.difference m1 m2
        Assert.Contains("b", diff.addedKeys)
        Assert.Contains("a", diff.removedKeys)
        Assert.Empty(diff.changedKeys)
