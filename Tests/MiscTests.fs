module Marksman.MiscTests

open Xunit

open Misc

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
    let abspath_urlencode_2 () =
        Assert.Equal("/file.md", "/file.md".AbsPathUrlEncode())

    [<Fact>]
    let abspath_urlencode_3 () =
        Assert.Equal("/file%20with%20spaces.md", "file with spaces.md".AbsPathUrlEncode())
        Assert.Equal("/file%20with%20spaces.md".AbsPathUrlEncodedToRelPath(), "file with spaces.md")

    [<Fact>]
    let abspath_urlencode_4 () =
        Assert.Equal("/file%23name.md", "file#name.md".AbsPathUrlEncode())
        Assert.Equal("/file%23name.md".AbsPathUrlEncodedToRelPath(), "file#name.md")
    
module PathUriTests =
    [<Fact>]
    let testWinPathFromUri () =
        let uri = "file:///e%3A/notes"
        let puri = PathUri.fromString uri

        Assert.Equal("e:\\notes", puri.LocalPath)

    [<Fact>]
    let testWinPathFromPath () =
        let puri = PathUri.fromString "E:\\notes (precious)"

        Assert.Equal("e:\\notes (precious)", puri.LocalPath)

    [<Fact>]
    let testWinDocUriFromUri () =
        let uri = "file:///e%3A/notes"
        let puri = PathUri.fromString uri
        Assert.Equal(uri, puri.DocumentUri)

    [<Fact>]
    let testWinDocUriFromPath () =
        let path = "E:\\notes"
        let uri = "file:///e%3A/notes"
        let puri = PathUri.fromString path
        Assert.Equal(uri, puri.DocumentUri)
