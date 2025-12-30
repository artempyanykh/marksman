module Marksman.ParserTests

open Ionide.LanguageServerProtocol.Types

open Marksman.Names
open Snapper
open Snapper.Attributes
open Xunit

open Marksman.Cst
open Marksman.Structure
open Marksman.Parser
open Marksman.Misc
open Marksman.Helpers

let checkSnapshot (document: array<Element>) =
    let lines =
        Array.map (fun x -> (Element.fmt x).Lines()) document |> Array.concat

    lines.ShouldMatchSnapshot()

let checkInlineSnapshot = checkInlineSnapshot Element.fmt

let scrapeString content =
    parse Config.ParserSettings.Default (Text.mkText content)
    |> Structure.concreteElements

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
    let parser_xref_note () =
        //          01234567890123456
        let text = "[[note]]"
        let document = scrapeString text
        checkInlineSnapshot document [ "WL: [[note]]; (0,0)-(0,8)"; "  doc=note; (0,2)-(0,6)" ]

    [<Fact>]
    let parser_xref_note_heading () =
        //          01234567890123456
        let text = "[[note#heading]]"
        let document = scrapeString text

        checkInlineSnapshot document [
            "WL: [[note#heading]]; (0,0)-(0,16)"
            "  doc=note; (0,2)-(0,6)"
            "  head=heading; (0,7)-(0,14)"
        ]

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
    let parse_wiki_empty_heading () =
        //          0123456
        let text = "[[T#]]"
        let doc = scrapeString text

        checkInlineSnapshot doc [
            "WL: [[T#]]; (0,0)-(0,6)" //
            "  doc=T; (0,2)-(0,3)"
            "  head=; (0,4)-(0,4)"
        ]

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
    let parse_wiki_with_title () =
        //          0123456789012345
        let text = "[[T#head|title]]"
        let doc = scrapeString text
        checkSnapshot doc

    [<Fact>]
    let parse_wiki_empty_title () =
        //          0123456789012345
        let text = "[[T#head|]]"
        let doc = scrapeString text
        checkSnapshot doc

    [<Fact>]
    let parse_wiki_no_doc_and_title () =
        //          0123456789012345
        let text = "[[#head|title]]"
        let doc = scrapeString text
        checkSnapshot doc

    [<Fact>]
    let parse_wiki_no_doc_and_no_title () =
        //          0123456789012345
        let text = "[[|]]"
        let doc = scrapeString text
        checkSnapshot doc

    [<Fact>]
    let parse_wiki_all_empty () =
        //          0123456789012345
        let text = "[[]]"
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

        checkInlineSnapshot document [
            "ML: [title](url) @ (0,0)-(0,12)"
            "  IL: label=title @ (0,1)-(0,6); url=url @ (0,8)-(0,11); title=∅"
        ]

    [<Fact>]
    let parser_link_2 () =
        // Without any text inside this is not considered a link
        let text = "[]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_link_3 () =
        // Without any text inside this is not considered a link
        let text = "[][]"
        let document = scrapeString text
        checkInlineSnapshot document []

    [<Fact>]
    let parser_link_4 () =
        // This is considered a link even though the contents are empty
        let text = "[]()"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: []() @ (0,0)-(0,4)"
            "  IL: label= @ (0,0)-(0,0); url=∅; title=∅"
        ]

    [<Fact>]
    let parser_link_5 () =
        let text = "[la bel](url \"title\")"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [la bel](url \"title\") @ (0,0)-(0,21)"
            "  IL: label=la bel @ (0,1)-(0,7); url=url @ (0,9)-(0,12); title=title @ (0,13)-(0,20)"
        ]

    [<Fact>]
    let parser_link_6 () =
        let text = "[la bel](url title)" // without quotation of title only the shortcut parses
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [la bel] @ (0,0)-(0,8)" //
            "  RS: label=la bel @ (0,1)-(0,7)"
        ]

    [<Fact>]
    let parser_link_7 () =
        let text = "[](url)"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [](url) @ (0,0)-(0,7)"
            "  IL: label= @ (0,0)-(0,0); url=url @ (0,3)-(0,6); title=∅"
        ]

    [<Fact>]
    let parser_link_8 () =
        let text = "[short_cut]"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [short_cut] @ (0,0)-(0,11)"
            "  RS: label=short_cut @ (0,1)-(0,10)"
        ]

    [<Fact>]
    let parser_link_9 () =
        let text = "[short cut][]"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [short cut][] @ (0,0)-(0,13)"
            "  RC: label=short cut @ (0,1)-(0,10)"
        ]

    [<Fact>]
    let parser_link_10 () =
        let text = "[label][ref]"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [label][ref] @ (0,0)-(0,12)"
            "  RF: text=label @ (0,1)-(0,6); label=ref @ (0,8)-(0,11)"
        ]

    [<Fact>]
    let parser_link_11 () =
        let text = "[foo]: /foo 'foo title'"
        let document = scrapeString text

        checkInlineSnapshot document [
            "MLD: [foo]: /foo 'foo title' @ (0,0)-(0,23)"
            "  label=foo @ (0,1)-(0,4); url=/foo @ (0,7)-(0,11); title=foo title @ (0,12)-(0,23)"
        ]

    [<Fact>]
    let parser_link_12 () =
        let text = "[foo]: /foo"
        let document = scrapeString text

        checkInlineSnapshot document [
            "MLD: [foo]: /foo @ (0,0)-(0,11)"
            "  label=foo @ (0,1)-(0,4); url=/foo @ (0,7)-(0,11); title=∅"
        ]

    [<Fact>]
    let parser_link_13 () =
        let text = "[foo]: /bar\nHere comes [foo]."
        let document = scrapeString text

        checkInlineSnapshot document [
            "MLD: [foo]: /bar @ (0,0)-(0,11)"
            "  label=foo @ (0,1)-(0,4); url=/bar @ (0,7)-(0,11); title=∅"
            "ML: [foo] @ (1,11)-(1,16)"
            "  RS: label=foo @ (1,12)-(1,15)"
        ]

    [<Fact>]
    let parser_link_14 () =
        let text = "[label][ref]\n\n[ref]: https://some.url"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [label][ref] @ (0,0)-(0,12)"
            "  RF: text=label @ (0,1)-(0,6); label=ref @ (0,8)-(0,11)"
            "MLD: [ref]: https://some.url @ (2,0)-(2,23)"
            "  label=ref @ (2,1)-(2,4); url=https://some.url @ (2,7)-(2,23); title=∅"
        ]

module FootnoteTests =
    [<Fact(Skip = "Footnote parsing not implemented")>]
    let footnote_1 () =
        let text = "[^1]\n\n[^1]: Single line footnote"
        let document = scrapeString text

        checkInlineSnapshot document [
            "ML: [^1] @ (0,0)-(0,4)"
            "  RS: label=^1 @ (0,1)-(0,3)"
            "MLD: [^1]: Footnote @ (2,0)-(2,14)"
            "  label=^1 @ (2,1)-(2,3); url=Footnote @ (2,6)-(2,14); title=∅"
        ]

module TagsTests =
    [<Fact>]
    let tags_1 () =
        let text = "#tag"
        let cst = scrapeString text

        checkInlineSnapshot cst [ "T: name=tag; range=(0,1)-(0,4) @ (0,0)-(0,4)" ]

    [<Fact>]
    let tags_2 () =
        let text = "(#tag\n(#tag\n(#tag)\n[?](#tag)"
        let cst = scrapeString text

        checkInlineSnapshot cst [
            "T: name=tag; range=(0,2)-(0,5) @ (0,1)-(0,5)"
            "T: name=tag; range=(1,2)-(1,5) @ (1,1)-(1,5)"
            "T: name=tag; range=(2,2)-(2,5) @ (2,1)-(2,5)"
            "ML: [?](#tag) @ (3,0)-(3,9)"
            "  IL: label=? @ (3,1)-(3,2); url=#tag @ (3,4)-(3,8); title=∅"
        ]

    [<Fact>]
    let tags_3 () =
        //          012345678901
        let text = "#tag1,#tag2"
        let cst = scrapeString text

        checkInlineSnapshot cst [
            "T: name=tag1; range=(0,1)-(0,5) @ (0,0)-(0,5)"
            "T: name=tag2; range=(0,7)-(0,11) @ (0,6)-(0,11)"
        ]

    [<Fact>]
    let tags_nested () =
        let text = "#tag1/subtag1,#tag2/subtag2/subsubtag2"
        let cst = scrapeString text

        checkInlineSnapshot cst [
            "T: name=tag1/subtag1; range=(0,1)-(0,13) @ (0,0)-(0,13)"
            "T: name=tag2/subtag2/subsubtag2; range=(0,15)-(0,38) @ (0,14)-(0,38)"
        ]

module DocUrlTests =
    let mkUrlNode str =
        Node.mk str (Range.Mk(0, 0, 0, str.Length)) (UrlEncoded.mkUnchecked str)

    [<Fact>]
    let test1 () =
        let actual = mkUrlNode "/some.md" |> Url.ofUrlNode
        Assert.Equal("docUrl=/some.md @ (0,0)-(0,8)", actual.ToString())

    [<Fact>]
    let test2 () =
        let actual = mkUrlNode "/some.md#anchor" |> Url.ofUrlNode

        Assert.Equal(
            "docUrl=/some.md @ (0,0)-(0,8);anchor=anchor @ (0,9)-(0,15)",
            actual.ToString()
        )

    [<Fact>]
    let test3 () =
        //                       01234567
        let actual = mkUrlNode "#anchor" |> Url.ofUrlNode

        Assert.Equal("anchor=anchor @ (0,1)-(0,7)", actual.ToString())

module RegressionTests =
    [<Fact>]
    let no156 () =
        let content = "A\n\n-\n-"

        let actual = scrapeString content

        checkInlineSnapshot actual [
            "H2: range=(2,0)-(3,1); scope=(2,0)-(4,0)"
            "  text=`-"
            "-`"
            "  title=`-"
            "-` @ (2,0)-(3,1)"
        ]

    [<Fact>]
    let no235 () =
        let content =
            """
[                                                                               
'00000048', '00000681', '00000552', '00000206', '00000031', '00000303',         
'00001268', '00000540', '00000519', '00000821', '00000731', '00001089',         
'00000311', '00000784', '00000015', '00001052', '00000030', '00000352',         
'00000758', '00000113', '00000152', '00000099', '00000932', '00000071',         
'00000126', '00000450', '00000677', '00000722', '00000724', '00000182',         
'00000507', '00000001', '00000866', '00000147', '00000186', '00000711'          
]
"""

        let actual = scrapeString content

        checkInlineSnapshot actual [
            "ML: [                                                                               "
            "'00000048', '00000681', '00000552', '00000206', '00000031', '00000303',         "
            "'00001268', '00000540', '00000519', '00000821', '00000731', '00001089',         "
            "'00000311', '00000784', '00000015', '00001052', '00000030', '00000352',         "
            "'00000758', '00000113', '00000152', '00000099', '00000932', '00000071',         "
            "'00000126', '00000450', '00000677', '00000722', '00000724', '00000182',         "
            "'00000507', '00000001', '00000866', '00000147', '00000186', '00000711'          "
            "] @ (1,0)-(8,1)"
            "  RS: label='00000048', '00000681', '00000552', '00000206', '00000031', '00000303',         "
            "  '00001268', '00000540', '00000519', '00000821', '00000731', '00001089',         "
            "  '00000311', '00000784', '00000015', '00001052', '00000030', '00000352',         "
            "  '00000758', '00000113', '00000152', '00000099', '00000932', '00000071',         "
            "  '00000126', '00000450', '00000677', '00000722', '00000724', '00000182',         "
            "  '00000507', '00000001', '00000866', '00000147', '00000186', '00000711' @ (2,0)-(7,70)"
        ]

    [<Fact>]
    let no334 () =
        let content = "[][][]"
        let actual = scrapeString content
        checkInlineSnapshot actual []

module MathBlockTests =
    [<Fact>]
    let math_block_should_not_parse_wikilinks () =
        let content = """$$
\begin{verbatim}
[[nodiscard]]
\end{verbatim}
$$"""
        let actual = scrapeString content
        // Math block should not produce any wikilink elements
        checkInlineSnapshot actual []

    [<Fact>]
    let inline_math_should_not_parse_wikilinks () =
        let content = "Inline math: $[[x]]$ in text"
        let actual = scrapeString content
        // Inline math should not produce any wikilink elements
        checkInlineSnapshot actual []

    [<Fact>]
    let math_and_regular_wikilink () =
        let content = """$$
[[in-math]]
$$

Regular [[valid-link]]"""
        let actual = scrapeString content
        // Only the regular wikilink should be detected, not the one in math block
        checkInlineSnapshot actual [
            "WL: [[valid-link]]; (4,8)-(4,22)"
            "  doc=valid-link; (4,10)-(4,20)"
        ]
