module Marksman.AstTests

open Xunit
open Snapper

open Marksman.Ast

let parseString content = Parser.parseText (Text.mkText content)

let checkInlineSnapshot =
    Helpers.checkInlineSnapshot (fun (el: Element) -> el.CompactFormat())

let cst1 =
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

let ast1 = Ast.ofCst cst1

[<Fact>]
let testAstShape () =
    checkInlineSnapshot
        (Ast.elements ast1)
        [ "# Doc 1 {doc-1}"
          "[collapsedRef][]"
          "[[wiki-link]]"
          "## Sub 1 {sub-1}"
          "[inline-link](url title)"
          "## Sub 2 {sub-2}"
          "#tag"
          "[collapsedRef]: DefURL" ]

[<Fact>]
let testAstLookup () =
    let sub1 = (Ast.elements ast1)[3]
    sub1.CompactFormat().ShouldMatchInlineSnapshot("## Sub 1 {sub-1}")

    let csub1 = (Ast.tryFindMatchingConcrete sub1 ast1) |> Option.get

    Helpers.checkInlineSnapshot
        Cst.Element.fmt
        [ csub1 ]
        [ "H2: range=(4,0)-(4,8); scope=(4,0)-(8,0)"
          "  text=`## Sub 1`"
          "  title=`Sub 1` @ (4,3)-(4,8)" ]

    let shouldBeSub1 = Ast.tryFindMatchingAbstract csub1 ast1 |> Option.get
    Assert.Equal(sub1, shouldBeSub1)

    let madeUpAbstract = Element.MR(Collapsed "WAT")
    Assert.True(Ast.tryFindMatchingConcrete madeUpAbstract ast1 |> Option.isNone)
