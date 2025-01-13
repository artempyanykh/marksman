module Marksman.AstTests

open Marksman.Config
open Xunit
open Snapper

open Marksman.Ast
open Marksman.Structure

let parseString content = Parser.parse Config.ParserSettings.Default (Text.mkText content)

let checkInlineSnapshot =
    Helpers.checkInlineSnapshot (fun (el: Element) -> el.CompactFormat())

let struct1 =
    parseString
        """
# Doc 1
Some text, [collapsedRef][]. [[wiki-link]]

## Sub 1

This is [inline-link](url "title")

## Sub 2

This is a #tag

[collapsedRef]: DefURL
"""

let ast1 = struct1.Ast

[<Fact>]
let testAstShape () =
    checkInlineSnapshot ast1.elements [
        "# Doc 1 {doc-1}"
        "[collapsedRef][]"
        "[[wiki-link]]"
        "## Sub 1 {sub-1}"
        "[inline-link](url)"
        "## Sub 2 {sub-2}"
        "#tag"
        "[collapsedRef]: DefURL"
    ]

[<Fact>]
let testAstLookup () =
    let sub1 = ast1.elements[3]
    sub1.CompactFormat().ShouldMatchInlineSnapshot("## Sub 1 {sub-1}")

    let csub1 =
        (Structure.findConcreteForAbstract sub1 struct1) |> Array.ofSeq

    Helpers.checkInlineSnapshot Cst.Element.fmt csub1 [
        "H2: range=(4,0)-(4,8); scope=(4,0)-(8,0)"
        "  text=`## Sub 1`"
        "  title=`Sub 1` @ (4,3)-(4,8)"
    ]

    let shouldBeSub1 = Structure.findMatchingAbstract csub1[0] struct1
    Assert.Equal(sub1, shouldBeSub1)

    let madeUpAbstract = Element.MR(Collapsed "WAT")
    Assert.Equal(Structure.tryFindConcreteForAbstract madeUpAbstract struct1, None)

[<Fact>]
let testSymsWhenTitleFromHeadingIsOff () =
    let doc =
        """
# H1
Is this a title?
# H2
Is this another title?
## H2.2
# H3
And this?
"""

    let strukt =
        Parser.parse { ParserSettings.Default with titleFromHeading = false } (Text.mkText doc)

    Helpers.checkInlineSnapshot _.ToString() strukt.Symbols [
        "Doc"
        "H1 {h1}"
        "H1 {h2}"
        "H1 {h3}"
        "H2 {h22}"
    ]

[<Fact>]
let testSymsWhenRepeatedHeadingsGlfm () =
    let doc =
        """
# X^
# A
# A
# X
# Z
## A
"""

    let strukt =
        Parser.parse { ParserSettings.Default with titleFromHeading = false } (Text.mkText doc)

    Helpers.checkInlineSnapshot _.ToString() strukt.Symbols [
        "Doc"
        "H1 {a}"
        "H1 {a-1}"
        "H1 {x}"
        "H1 {x-1}"
        "H1 {z}"
        "H2 {a-2}"
    ]

[<Fact>]
let testSymsWhenRepeatedHeadingsNoGlfm () =
    let doc =
        """
# X^
# A
# A
# X
# Z
## A
"""

    let strukt =
        Parser.parse
            {
                ParserSettings.Default with
                    titleFromHeading = false
                    glfmHeadingIds = false
            }
            (Text.mkText doc)

    Helpers.checkInlineSnapshot _.ToString() strukt.Symbols [
        "Doc"
        "H1 {a}"
        "H1 {x}"
        "H1 {z}"
        "H2 {a}"
    ]

[<Fact>]
let testSymsWhenTitleFromHeadingIsOn () =
    let doc =
        """
# H1
Is this a title?
# H2
Is this another title?
## H2.2
# H3
And this?
"""

    let strukt =
        Parser.parse { ParserSettings.Default with titleFromHeading = true } (Text.mkText doc)

    Helpers.checkInlineSnapshot _.ToString() strukt.Symbols [
        "Doc"
        "T {h1}"
        "T {h2}"
        "T {h3}"
        "H2 {h22}"
    ]
