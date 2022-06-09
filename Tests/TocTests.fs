module Marksman.TocTests

open Marksman.Index
open Xunit

open Marksman.Misc
open Marksman.Parser
open Marksman.Helpers
open Marksman.Toc

module DetectToc =
    [<Fact>]
    let detectToc_1 () =
        let doc = makeFakeDocument "# T1\n# T2"

        let titles = TableOfContents.detect doc.text

        Assert.Equal(titles, None)

    [<Fact>]
    let detectToc_noMarker () =
        let doc =
            makeFakeDocumentLines [| "- [T1][#t1]"; " - [T2][#t2]"; ""; ""; "# T1"; "## T2" |]

        let titles = TableOfContents.detect doc.text

        Assert.Equal(titles, None)

    [<Fact>]
    let detectToc_withMarker () =
        let doc =
            makeFakeDocumentLines
                [| Toc.Marker
                   "- [T1][#t1]"
                   " - [T2][#t2]"
                   ""
                   ""
                   "# T1"
                   "## T2" |]

        let titles = (TableOfContents.detect doc.text).Value
        let tocText = doc.text.Substring titles

        let expectedTocText =
            let line num = doc.text.LineContent(num)

            line (0) + "\n" + line (1) + "\n" + line (2)

        Assert.Equal(tocText.TrimEnd(), expectedTocText.TrimEnd())
        Assert.Equal(5, titles.End.Line)
        Assert.Equal(0, titles.End.Character)

module CreateToc =
    [<Fact>]
    let createToc () =
        let doc = makeFakeDocumentLines [| "# T1"; "## T2" |]

        let titles = TableOfContents.mk doc.index |> Option.get

        let expected =
            { Entries =
                [| { Level = 1; Title = "T1"; Link = Slug.ofString "T1" }
                   { Level = 2; Title = "T2"; Link = Slug.ofString "T2" } |] }

        Assert.Equal(expected, titles)

module RenderToc =
    [<Fact>]
    let createToc () =
        let doc =
            makeFakeDocumentLines [| "# T1"; "## T2"; "### T3"; "## T4"; "### T5" |]

        let titles =
            TableOfContents.mk doc.index |> Option.get |> TableOfContents.render

        let expectedLines =
            [| Toc.Marker
               "- [T1](#t1)"
               " - [T2](#t2)"
               "  - [T3](#t3)"
               " - [T4](#t4)"
               "  - [T5](#t5)"
               "" // two new lines are important
               "" 
            |]

        let expected = String.concat "\n" expectedLines + "\n"

        Assert.Equal(expected, titles)
