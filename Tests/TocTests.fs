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
            FakeDoc.Mk [| "- [T1][#t1]"; " - [T2][#t2]"; ""; ""; "# T1"; "## T2" |]

        let titles = TableOfContents.detect doc.text

        Assert.Equal(titles, None)

    [<Fact>]
    let detectToc_withMarker () =
        let doc =
            FakeDoc.Mk
                [| StartMarker
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
        Assert.Equal(3, toc.End.Line)
        Assert.Equal(EndMarker.Length, toc.End.Character)

module CreateToc =
    [<Fact>]
    let createToc () =
        let doc = FakeDoc.Mk [| "# T1"; "## T2" |]

        let titles = TableOfContents.mk doc.index |> Option.get

        let expected = { entries = [| Entry.Mk(1, "T1"); Entry.Mk(2, "T2") |] }

        Assert.Equal(expected, titles)

    [<Fact>]
    let createToc_yamlFrontMatter () =
        let doc =
            FakeDoc.Mk
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
        let doc = FakeDoc.Mk [| "## T1"; "## T2" |]

        let insertion = TableOfContents.insertionPoint doc

        Assert.Equal(insertion, DocumentBeginning)

    [<Fact>]
    let insert_firstTitle () =
        let doc = FakeDoc.Mk [| "# T1"; "## T2" |]

        let insertion = TableOfContents.insertionPoint doc
        let firstTitleRange = Array.head(doc.index.titles).range

        Assert.Equal(insertion, After firstTitleRange)

    [<Fact>]
    let insert_afterYaml () =
        let doc =
            FakeDoc.Mk
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
    let insert_afterFirstTitle_withYaml () =
        let doc =
            FakeDoc.Mk
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
        let doc = FakeDoc.Mk [| "# T1"; "## T2"; "### T3"; "## T4"; "### T5" |]

        let titles =
            TableOfContents.mk doc.index |> Option.get |> TableOfContents.render

        let expectedLines =
            [| StartMarker
               "- [T1](#t1)"
               "  - [T2](#t2)"
               "    - [T3](#t3)"
               "  - [T4](#t4)"
               "    - [T5](#t5)"
               EndMarker |]

        let expected = String.concat NewLine expectedLines

        Assert.Equal(expected, titles)

module DocumentEdit =
    [<Fact>]
    let insert_afterYaml () =
        let text =
            stripMarginTrim
                $"
                |---
                |title: \"First\"
                |tags: [1, 2]
                |---
                |
                |## T1 
                |### T2
                |## T3
                |#### T4
                "

        let doc = FakeDoc.Mk text

        let action = CodeActions.tableOfContents doc |> Option.get

        let modifiedText = applyDocumentAction doc action

        let expected =
            stripMarginTrim
                $"
                |---
                |title: \"First\"
                |tags: [1, 2]
                |---
                |
                |{StartMarker}
                |- [T1](#t1)
                |  - [T2](#t2)
                |- [T3](#t3)
                |    - [T4](#t4)
                |{EndMarker}
                |
                |## T1 
                |### T2
                |## T3
                |#### T4
                "

        Assert.Equal(expected, modifiedText)

    [<Fact>]
    let insert_afterTopHeading () =
        let text =
            stripMarginTrim
                "
                |# T1 
                |### T2 
                |
                |## T3
                |
                |#### T4"

        let doc = FakeDoc.Mk text

        let action = CodeActions.tableOfContents doc |> Option.get

        let modifiedText = applyDocumentAction doc action

        let expected =
            stripMarginTrim
                $"
                |# T1 
                |
                |{StartMarker}
                |- [T1](#t1)
                |    - [T2](#t2)
                |  - [T3](#t3)
                |      - [T4](#t4)
                |{EndMarker}
                |
                |### T2 
                |
                |## T3
                |
                |#### T4"

        Assert.Equal(expected, modifiedText)

    [<Fact>]
    let insert_documentBeginning () =
        let text =
            stripMarginTrim
                "
                |## T1 
                |
                |hello 
                |### T2 
                |
                |## T3
                |
                |#### T4"

        let doc = FakeDoc.Mk text

        let action = CodeActions.tableOfContents doc |> Option.get

        let modifiedText = applyDocumentAction doc action

        let expected =
            stripMarginTrim
                $"
                |{StartMarker}
                |- [T1](#t1)
                |  - [T2](#t2)
                |- [T3](#t3)
                |    - [T4](#t4)
                |{EndMarker}
                |
                |## T1 
                |
                |hello 
                |### T2 
                |
                |## T3
                |
                |#### T4"

        Assert.Equal(expected, modifiedText)

    [<Fact>]
    let update_atBeginningOfFile () =
        let doc =
            FakeDoc.Mk(
                stripMarginTrim
                    $"
                    |{StartMarker}
                    |- [T1](#t1)
                    |{EndMarker}
                    |
                    |# T2"
            )

        let action = CodeActions.tableOfContents doc |> Option.get
        let modifiedText = applyDocumentAction doc action

        let expected =
            stripMarginTrim
                $"
                |{StartMarker}
                |- [T2](#t2)
                |{EndMarker}
                |
                |# T2"


        Assert.Equal(expected, modifiedText)

    [<Fact>]
    let upToDate_noUpdate () =
        let text =
            stripMarginTrim
                $"
                |{StartMarker}
                |- [T1](#t1)
                |{EndMarker}
                |
                |# T1"

        let doc = FakeDoc.Mk text

        let action = CodeActions.tableOfContents doc

        Assert.Equal(None, action)
