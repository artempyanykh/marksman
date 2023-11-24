module Marksman.AstTests

open Xunit
open Snapper

open Marksman.Ast
open Marksman.Structure

let parseString content = Parser.parse Config.defaultMarkdownExtensions (Text.mkText content)

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
    checkInlineSnapshot
        ast1.elements
        [ "# Doc 1 {doc-1}"
          "[collapsedRef][]"
          "[[wiki-link]]"
          "## Sub 1 {sub-1}"
          "[inline-link](url)"
          "## Sub 2 {sub-2}"
          "#tag"
          "[collapsedRef]: DefURL" ]

[<Fact>]
let testAstLookup () =
    let sub1 = ast1.elements[3]
    sub1.CompactFormat().ShouldMatchInlineSnapshot("## Sub 1 {sub-1}")

    let csub1 = (Structure.findMatchingConcrete sub1 struct1) |> List.ofArray

    Helpers.checkInlineSnapshot
        Cst.Element.fmt
        csub1
        [ "H2: range=(4,0)-(4,8); scope=(4,0)-(8,0)"
          "  text=`## Sub 1`"
          "  title=`Sub 1` @ (4,3)-(4,8)" ]

    let shouldBeSub1 = Structure.findMatchingAbstract csub1[0] struct1
    Assert.Equal(sub1, shouldBeSub1)

    let madeUpAbstract = Element.MR(Collapsed "WAT")
    Assert.Equal(Structure.tryFindMatchingConcrete madeUpAbstract struct1, None)
