module Marksman.TocTests

open Marksman.Index
open Xunit

open Marksman.Misc
open Marksman.Parser
open Marksman.Helpers
open Marksman.Toc

open type System.Environment

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
                [| Toc.StartMarker
                   "- [T1][#t1]"
                   " - [T2][#t2]"
                   Toc.EndMarker
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
        let doc = makeFakeDocumentLines [| "# T1"; "## T2" |]

        let titles = TableOfContents.mk doc.index |> Option.get

        let expected = { entries = [| Entry.Mk(1, "T1"); Entry.Mk(2, "T2") |] }

        Assert.Equal(expected, titles)

    [<Fact>]
    let createToc_yamlFrontMatter () =
        let doc =
            makeFakeDocumentLines
                [| "---"
                   """title: "First" """
                   """tags: ["1", "2"] """
                   "---"
                   ""
                   "# T1"
                   "## T2" |]

        let titles = TableOfContents.mk doc.index |> Option.get

        let expected = { entries = [| Entry.Mk(1, "T1"); Entry.Mk(2, "T2") |] }

        // Test that YAML front matter is not picked up as one of the headings
        // See https://spec.commonmark.org/0.30/#example-80 for why
        // it can be interpreted as a heading
        Assert.Equal(expected, titles)


module InsertToc =
    [<Fact>]
    let insert_documentBeginning () =
        let doc = makeFakeDocumentLines [| "## T1"; "## T2" |]

        let insertion = TableOfContents.insertionPoint doc

        Assert.Equal(insertion, Replacing Text.documentBeginning)

    [<Fact>]
    let insert_firstTitle () =
        let doc = makeFakeDocumentLines [| "# T1"; "## T2" |]

        let insertion = TableOfContents.insertionPoint doc
        let firstTitleRange = Array.head(doc.index.titles).range

        Assert.Equal(insertion, After firstTitleRange)

    [<Fact>]
    let insert_afterYaml () =
        let doc =
            makeFakeDocumentLines
                [| "---"
                   """title: "First" """
                   """tags: ["1", "2"] """
                   "---"
                   ""
                   "## T1"
                   "## T2" |]


        let insertion = TableOfContents.insertionPoint doc
        let yamlRange = Option.get(doc.index.yamlFrontMatter).range

        Assert.Equal(insertion, After yamlRange)

    [<Fact>]
    let insert_afterfirstTitle_withYaml () =
        let doc =
            makeFakeDocumentLines
                [| "---"
                   """title: "First" """
                   """tags: ["1", "2"] """
                   "---"
                   ""
                   "# T1"
                   "## T2" |]


        let insertion = TableOfContents.insertionPoint doc
        let firstTitleRange = Array.head(doc.index.titles).range

        Assert.Equal(insertion, After firstTitleRange)


module RenderToc =
    [<Fact>]
    let createToc () =
        let doc =
            makeFakeDocumentLines [| "# T1"; "## T2"; "### T3"; "## T4"; "### T5" |]

        let titles =
            TableOfContents.mk doc.index |> Option.get |> TableOfContents.render

        let expectedLines =
            [| Toc.StartMarker
               "- [T1](#t1)"
               "  - [T2](#t2)"
               "    - [T3](#t3)"
               "  - [T4](#t4)"
               "    - [T5](#t5)"
               Toc.EndMarker
               "" |]

        let expected = String.concat NewLine expectedLines

        Assert.Equal(expected, titles)

module DocumentEdit =
    [<Fact>]
    let insert_afterYaml () =
        let doc =
            makeFakeDocumentLines
                [| "---"
                   """title: "First" """
                   """tags: ["1", "2"] """
                   "---"
                   ""
                   "## T1"
                   "### T2"
                   "## T3"
                   "#### T4" |]

        let action = CodeActions.tableOfContents doc |> Option.get

        let modifiedText = applyDocumentAction doc action

        let expected =
            String.concat
                NewLine
                [| "---"
                   """title: "First" """
                   """tags: ["1", "2"] """
                   "---"
                   ""
                   Toc.StartMarker
                   "- [T1](#t1)"
                   "  - [T2](#t2)"
                   "- [T3](#t3)"
                   "    - [T4](#t4)"
                   Toc.EndMarker
                   ""
                   "## T1"
                   "### T2"
                   "## T3"
                   "#### T4" |]

        Assert.Equal(expected, modifiedText)

    [<Fact>]
    let insert_documentBeginning () =
        let doc =
            makeFakeDocumentLines [| "## T1"; "### T2"; "## T3"; "#### T4" |]

        let action = CodeActions.tableOfContents doc |> Option.get

        let modifiedText = applyDocumentAction doc action

        let expected =
            String.concat
                NewLine
                [| Toc.StartMarker
                   "- [T1](#t1)"
                   "  - [T2](#t2)"
                   "- [T3](#t3)"
                   "    - [T4](#t4)"
                   Toc.EndMarker
                   "## T1"
                   "### T2"
                   "## T3"
                   "#### T4" |]

        Assert.Equal(expected, modifiedText)
