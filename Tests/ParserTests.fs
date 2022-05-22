module Marksman.ParserTests

open Snapper.Attributes
open Xunit

open Parser
open Snapper
open Misc

let checkSnapshot (document: array<Element>) =
    let lines = Array.map (fun x -> (Element.fmt x).Lines()) document |> Array.concat

    lines.ShouldMatchSnapshot()

let checkInlineSnapshot (document: array<Element>) snapshot =
    let lines = Array.map (fun x -> (Element.fmt x).Lines()) document |> Array.concat

    lines.ShouldMatchInlineSnapshot(snapshot)

let scrapeString content = parseText (Text.mkText content)

[<StoreSnapshotsPerClass>]
module HeadingTests =

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
        let text = "# Title 1\n# Title ... (2)\r\n# 3rd Title"

        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parse_title_with_child_paragraph () =
        let text = "# Title 1\nSome text\r\n# Title 2"

        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parse_nested_headings () =
        let text = "# H1 \n## H2.1\n## H2.2\n"
        let document = scrapeString text
        checkSnapshot document
        
[<StoreSnapshotsPerClass>]
module WikiLinkTests =

    [<Fact>]
    let parser_link_shortcut_ignore_for_now () =
        let text = "[note]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_xref_note () =
        let text = "[[note]]"
        let document = scrapeString text
        checkInlineSnapshot document [ "WL: [[note]]; (0,0)-(0,8)"; "  doc=note; (0,2)-(0,6)" ]

    [<Fact>]
    let parser_xref_note_heading () =
        //          01234567890123456
        let text = "[[note#heading]]"
        let document = scrapeString text

        checkInlineSnapshot
            document
            [ "WL: [[note#heading]]; (0,0)-(0,16)"
              "  doc=note; (0,2)-(0,6)"
              "  head=heading; (0,7)-(0,14)" ]

    [<Fact>]
    let parser_xref_text_before () =
        //          0123456789012
        let text = "Before [[N]]"
        let document = scrapeString text
        checkInlineSnapshot document [ "WL: [[N]]; (0,7)-(0,12)"; "  doc=N; (0,9)-(0,10)" ]

    [<Fact>]
    let parser_xref_text_after () =
        //          0123456789012345
        let text = "[[note]]! Other"
        let document = scrapeString text
        checkInlineSnapshot document [ "WL: [[note]]; (0,0)-(0,8)"; "  doc=note; (0,2)-(0,6)" ]

    [<Fact>]
    let parser_xref_text_around () =
        //          0123456789012
        let text = "To [[note]]!"
        let document = scrapeString text
        checkInlineSnapshot document [ "WL: [[note]]; (0,3)-(0,11)"; "  doc=note; (0,5)-(0,9)" ]

    [<Fact>]
    let parser_xref_2nd_line () =
        //                    1         2         3
        //          0123456789012345678901234567890
        let text = "# H1\nThis is [[note]] huh!\n"
        //          01234 0123456789012345678901
        let document = scrapeString text
        checkSnapshot document

    [<Fact>]
    let parser_wiki_empty_heading () =
        //          0123456
        let text = "[[T#]]"
        let doc = scrapeString text

        checkInlineSnapshot
            doc
            [ "WL: [[T#]]; (0,0)-(0,6)" //
              "  doc=T; (0,2)-(0,3)"
              "  head=; (0,4)-(0,4)" ]
    
    [<Fact>]
    let parse_wiki_escaped_hash () =
        //          01234567
        let text = "[[F\#]]"
        let doc = scrapeString text
        checkSnapshot doc
        
    [<Fact>]
    let parse_wiki_escaped_hash_and_heading () =
        //          0123456789012345
        let text = "[[F\##Section]]"
        let doc = scrapeString text
        checkSnapshot doc
        
    [<Fact>]
    let parse_wiki_escaped_hash_and_heading_with_hash () =
        //          0123456789012345
        let text = "[[F\##Section #3]]"
        let doc = scrapeString text
        checkSnapshot doc

    [<Fact>]
    let complex_example_1 () =
        let text =
            //           1          2          3          4          5
            //1234 5 67890123 45678901234567 890123 45678901 23456789012345678901
            "# H1\n\n## H2.1\nP2.1 [[ref1]]\n[[cp1\n## H2.2\nP2.2 [:cp2 next"
        //   0      12        3              4      5        6

        let document = scrapeString text
        checkSnapshot document

[<StoreSnapshotsPerClass>]
module MdLinkTest =
    [<Fact>]
    let parser_link_1 () =
        //          0123456789012
        let text = "[title](url)"
        let document = scrapeString text
        checkInlineSnapshot document [ "ML: [title](url); (0,0)-(0,12)" ]

    [<Fact>]
    let parser_link_2 () =
        let text = "[]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_link_3 () =
        let text = "[][]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_link_4 () =
        let text = "[]()"
        let document = scrapeString text
        checkInlineSnapshot document [ "ML: [](); (0,0)-(0,4)" ]

    [<Fact>]
    let parser_link_5 () =
        let text = "[la bel](url \"title\")"
        let document = scrapeString text
        checkInlineSnapshot document [ "ML: [la bel](url \"title\"); (0,0)-(0,21)" ]

    [<Fact>]
    let parser_link_6 () =
        let text = "[la bel](url title)"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_link_7 () =
        let text = "[](url)"
        let document = scrapeString text
        checkInlineSnapshot document [ "ML: [](url); (0,0)-(0,7)" ]

    [<Fact>]
    let parser_link_8 () =
        let text = "[short cut]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_link_9 () =
        let text = "[short cut][]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_link_10 () =
        let text = "[label][ref]"
        let document = scrapeString text
        checkInlineSnapshot document []
