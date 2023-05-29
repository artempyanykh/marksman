module Marksman.MiscTests

open Xunit

open Marksman.Misc

module StringExtensionsTests =
    [<Fact>]
    let isSubSequenceOf_1 () = Assert.True("f".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_2 () = Assert.True("fsharp".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_3 () = Assert.True("fr".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_4 () = Assert.True("".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_5 () = Assert.False("Md".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let slug_1 () =
        Assert.Equal(
            "header-with-words-and-some-punctuation",
            "# Header with words, and some; punctuation$%@?".Slug()
        )

    [<Fact>]
    let slug_2 () =
        Assert.Equal(
            "текст-на-другом-языке-как-тебе-такое-илон-маск",
            "## --Текст на другом языке. Как тебе такое, Илон Маск?!".Slug()
        )

    [<Fact>]
    let slug_3 () =
        Assert.Equal("wow-some-hyphens-in-the-middle", "#-wow-some-hyphens- in-the- -middle".Slug())

    [<Fact>]
    let slug_4 () = Assert.Equal("multi-hyphens", "multi-------hyphens".Slug())

    [<Fact>]
    let slug_5 () = Assert.Equal("", "".Slug())

    [<Fact>]
    let lines_1 () = Assert.Equal<string>([| "Line" |], "Line".Lines())

    [<Fact>]
    let lines_2 () =
        Assert.Equal<string>([| "Line 1"; "Line 2" |], "Line 1\nLine 2".Lines())

    [<Fact>]
    let lines_3 () =
        Assert.Equal<string>([| "Line 1"; "Line 2"; "Line 3" |], "Line 1\nLine 2\r\nLine 3".Lines())

    [<Fact>]
    let abspath_urlencode_1 () =
        Assert.Equal("/file.md", "file.md".AbsPathUrlEncode())
        Assert.Equal("/file.md".AbsPathUrlEncodedToRelPath(), "file.md")

    [<Fact>]
    let abspath_urlencode_2 () = Assert.Equal("/file.md", "/file.md".AbsPathUrlEncode())

    [<Fact>]
    let abspath_urlencode_3 () =
        Assert.Equal("/file%20with%20spaces.md", "file with spaces.md".AbsPathUrlEncode())
        Assert.Equal("/file%20with%20spaces.md".AbsPathUrlEncodedToRelPath(), "file with spaces.md")

    [<Fact>]
    let abspath_urlencode_4 () =
        Assert.Equal("/file%23name.md", "file#name.md".AbsPathUrlEncode())
        Assert.Equal("/file%23name.md".AbsPathUrlEncodedToRelPath(), "file#name.md")

    [<Fact>]
    let abspath_urlencode_5 () =
        Assert.Equal("/folder%20name/file%20name.md", "folder name/file name.md".AbsPathUrlEncode())

        Assert.Equal(
            "/folder%20name/file%20name.md".AbsPathUrlEncodedToRelPath(),
            "folder name/file name.md"
        )

    [<Fact>]
    let trimSuffix_1 () = Assert.Equal("foo", "foobar".TrimSuffix("bar"))

    [<Fact>]
    let trimSuffix_2 () = Assert.Equal("foobar", "foobar".TrimSuffix("baz"))

    [<Fact>]
    let encodeForWiki_1 () = Assert.Equal("blah blah", "blah blah".EncodeForWiki())

    [<Fact>]
    let encodeForWiki_2 () =
        let original = "blah #blah [] () |"
        let expected = "blah %23blah %5B%5D () %7C"
        let actual = original.EncodeForWiki()
        Assert.Equal(expected, actual)
        Assert.Equal(original, actual.UrlDecode())

module LinkLabelTest =
    [<Fact>]
    let caseSensitivity () = Assert.Equal(LinkLabel.ofString "hello", LinkLabel.ofString "HELLO")

    [<Fact>]
    let consecutiveWhitespace () =
        Assert.Equal(LinkLabel.ofString "H e ll o", LinkLabel.ofString "H  e    ll  o")

    [<Fact>]
    let surroundingWhitespace () =
        Assert.Equal(LinkLabel.ofString "abc", LinkLabel.ofString "  abc ")

module WatchGlobTest =
    [<Fact>]
    let test1 () =
        Assert.Equal("**/*.{md,markdown,mdx}", mkWatchGlob [| "md"; "markdown"; "mdx" |])

module SuffixTreeTests =
    let tree =
        SuffixTree.ofSeq [ [ "a"; "b"; "c" ], 1; [ "a"; "b"; "d" ], 2; [ "b" ], 3; [ "c" ], 4 ]

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
