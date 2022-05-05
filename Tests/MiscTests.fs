module Marksman.MiscTests

open Xunit

open Misc

module StringExtensionsTests =
    [<Fact>]
    let isSubSequenceOf_1 () =
        Assert.True("f".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_2 () =
        Assert.True("fsharp".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_3 () =
        Assert.True("fr".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_4 () =
        Assert.True("".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let isSubSequenceOf_5 () =
        Assert.False("Md".IsSubSequenceOf("fsharp"))

    [<Fact>]
    let slug_1 () =
        Assert.Equal(
            "header-with-words-and-some-punctuation",
            "# Header with words, and some; punctuation$%@?"
                .Slug()
        )

    [<Fact>]
    let slug_2 () =
        Assert.Equal(
            "текст-на-другом-языке-как-тебе-такое-илон-маск",
            "## --Текст на другом языке. Как тебе такое, Илон Маск?!"
                .Slug()
        )

    [<Fact>]
    let slug_3 () =
        Assert.Equal("wow-some-hyphens-in-the-middle", "#-wow-some-hyphens- in-the- -middle".Slug())

    [<Fact>]
    let slug_4 () =
        Assert.Equal("multi-hyphens", "multi-------hyphens".Slug())

    [<Fact>]
    let slug_5 () = Assert.Equal("", "".Slug())

    [<Fact>]
    let lines_1 () =
        Assert.Equal<string>([| "Line" |], "Line".Lines())
        
    [<Fact>]
    let lines_2 () =
        Assert.Equal<string>([| "Line 1"; "Line 2" |], "Line 1\nLine 2".Lines())
        
    [<Fact>]
    let lines_3 () =
        Assert.Equal<string>([| "Line 1"; "Line 2"; "Line 3" |], "Line 1\nLine 2\r\nLine 3".Lines())
