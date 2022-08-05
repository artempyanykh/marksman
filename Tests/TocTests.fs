module Marksman.TocTests

open Xunit

open Marksman.Helpers
open Marksman.Toc

open type System.Environment

module DetectToc =
    [<Fact>]
    let detectToc_1 () =
        let doc = FakeDoc.Mk "# T1\n# T2"

        let titles = TableOfContents.detect doc.text

        Assert.Equal(titles, None)

    [<Fact>]
    let detectToc_noMarker () =
        let doc =
            FakeDoc.Mk [| "- [T1][#t1]"
                          " - [T2][#t2]"
                          ""
                          ""
                          "# T1"
                          "## T2" |]

        let titles = TableOfContents.detect doc.text

        Assert.Equal(titles, None)

    [<Fact>]
    let detectToc_withMarker () =
        let doc =
            FakeDoc.Mk [| StartMarker
                          "- [T1][#t1]"
                          " - [T2][#t2]"
                          EndMarker
                          "# T1"
                          "## T2" |]

        let toc = (TableOfContents.detect doc.text).Value
        let tocText = doc.text.Substring toc

        let expectedTocText =
            let line num = doc.text.LineContent(num)

            String.concat System.Environment.NewLine ([| 0..3 |] |> Array.map line)


        Assert.Equal(tocText.TrimEnd(), expectedTocText.TrimEnd())
        Assert.Equal(4, toc.End.Line)
        Assert.Equal(0, toc.End.Character)

module CreateToc =
    [<Fact>]
    let createToc () =
        let doc = FakeDoc.Mk [| "# T1"; "## T2" |]

        let titles = TableOfContents.mk doc.index |> Option.get

        let expected = { entries = [| Entry.Mk(1, "T1"); Entry.Mk(2, "T2") |] }

        Assert.Equal(expected, titles)

module RenderToc =
    [<Fact>]
    let createToc () =
        let doc = FakeDoc.Mk [| "# T1"; "## T2"; "### T3"; "## T4"; "### T5" |]

        let titles =
            TableOfContents.mk doc.index |> Option.get |> TableOfContents.render

        let expectedLines =
            [| StartMarker
               "- [T1](#t1)"
               " - [T2](#t2)"
               "  - [T3](#t3)"
               " - [T4](#t4)"
               "  - [T5](#t5)"
               EndMarker
               "" |]

        let expected = String.concat NewLine expectedLines

        Assert.Equal(expected, titles)
