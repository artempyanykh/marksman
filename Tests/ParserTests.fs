module Marksman.ParserTests

open Snapper.Attributes
open Xunit

open Parser
open Snapper
open Misc

let checkSnapshot (document: array<Element>) =
    let lines =
        Array.map (fun x -> (Element.fmt x).Lines()) document
        |> Array.concat

    lines.ShouldMatchSnapshot()

let checkInlineSnapshot (document: array<Element>) snapshot =
    let lines =
        Array.map (fun x -> (Element.fmt x).Lines()) document
        |> Array.concat

    lines.ShouldMatchInlineSnapshot(snapshot)
    
let scrapeString content = parseText (Text.mkText content)

[<StoreSnapshotsPerClass>]
module SnapshotTests =

    [<Fact>]
    let parse_empty () =
        let text = ""
        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parse_title_single () =
        let text = "# Title text"
        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parse_title_multiple () =
        let text =
            "# Title 1\n# Title ... (2)\r\n# 3rd Title"

        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parse_title_with_child_paragraph () =
        let text =
            "# Title 1\nSome text\r\n# Title 2"

        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parse_nested_headings () =
        let text = "# H1 \n## H2.1\n## H2.2\n"
        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parser_link_shortcut_ignore_for_now () =
        let text = "[note]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_xref_note () =
        let text = "[[note]]"
        let document = scrapeString text
        checkInlineSnapshot document [ "R: [[note]]; (0,0)-(0,8)" ]

    [<Fact>]
    let parser_xref_note_heading () =
        //          01234567890123456
        let text = "[[note#heading]]"
        let document = scrapeString text
        checkInlineSnapshot document [ "R: [[note#heading]]; (0,0)-(0,16)" ]

    [<Fact>]
    let parser_xref_text_before () =
        //          0123456789012
        let text = "Before [[N]]"
        let document = scrapeString text
        checkInlineSnapshot document [ "R: [[N]]; (0,7)-(0,12)" ]

    [<Fact>]
    let parser_xref_text_after () =
        //          0123456789012345
        let text = "[[note]]! Other"
        let document = scrapeString text
        checkInlineSnapshot document [ "R: [[note]]; (0,0)-(0,8)" ]

    [<Fact>]
    let parser_xref_text_around () =
        //          0123456789012
        let text = "To [[note]]!"
        let document = scrapeString text
        checkInlineSnapshot document [ "R: [[note]]; (0,3)-(0,11)" ]

    [<Fact>]
    let parser_xref_2nd_line () =
        //                    1         2         3
        //          0123456789012345678901234567890
        let text = "# H1\nThis is [[note]] huh!\n"
        //          01234 0123456789012345678901
        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parser_completion_point_1 () =
        let text = "[["
        let document = scrapeString text
        checkInlineSnapshot document [ "CP: `[[`: (0,0)-(0,2)" ]

    [<Fact>]
    let parser_completion_point_2 () =
        //          0123456789
        let text = "[[partial other text"
        let document = scrapeString text
        checkInlineSnapshot document [ "CP: `[[partial`: (0,0)-(0,9)" ]

    [<Fact>]
    let parser_completion_point_3 () =
        //          0123456789
        let text = "[[not_cp] other text"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_completion_point_4 () =
        //          0123456789012
        let text = "P: [par_link other text"
        let document = scrapeString text
        checkInlineSnapshot document [ "CP: `[par_link`: (0,3)-(0,12)" ]

    [<Fact>]
    let complex_example_1 () =
        let text =
            //           1          2          3          4          5
            //1234 5 67890123 45678901234567 890123 45678901 23456789012345678901
            "# H1\n\n## H2.1\nP2.1 [[ref1]]\n[[cp1\n## H2.2\nP2.2 [:cp2 next"
        //   1       2        3              4      5        6

        let document = scrapeString text
        checkSnapshot document
    
module LinkParsing =
    [<Fact>]
    let parser_link_1() =
        let text = "[title](url)"
        let document = scrapeString text
        checkInlineSnapshot document ["L: [title](url)"]
        
    [<Fact>]
    let parser_link_2() =
        let text = "[]"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_3() =
        let text = "[][]"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_4() =
        let text = "[]()"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_5() =
        let text = "[la bel](url \"title\")"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_6() =
        let text = "[la bel](url title)"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_7() =
        let text = "[](url)"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_8() =
        let text = "[short cut]"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_9() =
        let text = "[short cut][]"
        let document = scrapeString text
        checkInlineSnapshot document []
        
    [<Fact>]
    let parser_link_10() =
        let text = "[label][ref]"
        let document = scrapeString text
        checkInlineSnapshot document []

module XDestTests =
    [<Fact>]
    let parse_pound () =
        let actual =
            RefDest.tryFromString "[[foo#bar]]"

        Assert.Equal(RefDest.Heading(Some "foo", "bar") |> Some, actual)

    [<Fact>]
    let parse_pound_pipe () =
        let actual =
            RefDest.tryFromString "[[foo#bar|baz]]"

        Assert.Equal(RefDest.Heading(Some "foo", "bar|baz") |> Some, actual)

    [<Fact>]
    let parse_pound_pound () =
        let actual =
            RefDest.tryFromString "[[foo#bar#baz]]"

        Assert.Equal(RefDest.Heading(Some "foo", "bar#baz") |> Some, actual)
