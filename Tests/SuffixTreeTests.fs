module Marksman.SuffixTreeTests

open Xunit

module ImplTests =
    module SuffixTree = SuffixTree.Impl

    let tree =
        SuffixTree.ofSeq
            [ [ "a"; "b" ], 0
              [ "a"; "b"; "c" ], 1
              [ "a"; "b"; "d" ], 2
              [ "b" ], 3
              [ "c" ], 4 ]

    [<Fact>]
    let filterTest () =
        let actual = SuffixTree.filterMatchingValues [ "c" ] tree |> List.ofSeq
        Assert.Equal<int>([ 4; 1 ], actual)

        let actual = SuffixTree.filterMatchingValues [ "d" ] tree |> List.ofSeq
        Assert.Equal<int>([ 2 ], actual)

        let actual =
            SuffixTree.filterMatchingValues [ "b"; "c" ] tree |> List.ofSeq

        Assert.Equal<int>([ 1 ], actual)

        let actual = SuffixTree.filterMatchingValues [ "e" ] tree |> List.ofSeq
        Assert.Equal<int>([], actual)

        let actual =
            SuffixTree.filterMatchingValues [ "b"; "z" ] tree |> List.ofSeq

        Assert.Equal<int>([], actual)

    [<Fact>]
    let removeTest () =
        let actual =
            SuffixTree.remove [ "a"; "b"; "c" ] tree |> SuffixTree.collectValues

        Assert.Equal([ 3; 0; 4; 2 ], actual)

        let actual =
            SuffixTree.remove [ "a"; "b"; "z" ] tree |> SuffixTree.collectValues

        Assert.Equal([ 3; 0; 4; 1; 2 ], actual)

        let actual = SuffixTree.remove [ "c" ] tree |> SuffixTree.collectValues

        Assert.Equal([ 3; 0; 1; 2 ], actual)

        let actual =
            SuffixTree.remove [ "a"; "b" ] tree |> SuffixTree.collectValues

        Assert.Equal([ 3; 4; 1; 2 ], actual)
