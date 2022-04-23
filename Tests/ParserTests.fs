module Marksman.ParserTests

open Snapper.Attributes
open Xunit

open Parser
open Snapper
open Misc

[<StoreSnapshotsPerClass>]
module SnapshotTests =
    let checkSnapshot (document: array<Element>) =
        let lines = Array.map (fun x -> (Element.fmt x).Lines()) document |> Array.concat
        lines.ShouldMatchSnapshot()

    [<Fact>]
    let parse_empty () =
        let text = ""
        let document = scrapeDocument text
        checkSnapshot document

    [<Fact>]
    let parse_title_single () =
        let text = "# Title text"
        let document = scrapeDocument text
        checkSnapshot document

    [<Fact>]
    let parse_title_multiple () =
        let text = "# Title 1\n# Title ... (2)\r\n# 3rd Title"
        let document = scrapeDocument text
        checkSnapshot document

    [<Fact>]
    let parse_title_with_child_paragraph () =
        let text = "# Title 1\nSome text\r\n# Title 2"
        let document = scrapeDocument text
        checkSnapshot document

    [<Fact>]
    let parse_nested_headings () =
        let text = "# H1 \n## H2.1\n## H2.2\n"
        let document = scrapeDocument text
        checkSnapshot document
