module Marksman.CompTests

open Ionide.LanguageServerProtocol.Types
open Xunit

open Marksman.Comp
open Marksman.Misc

let mkTitleComp range = Comp.Comp.Title(range, true)

module CompOfText =
    [<Fact>]
    let empty () =
        let text = Text.mkText ""
        let comp = compOfText (Position.Mk(0, 0)) text
        Assert.Equal(None, comp)

    [<Fact>]
    let wikiEmptyEol () =
        let text = Text.mkText "[["
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let wikiEmptyNonEol () =
        let text = Text.mkText "[[ "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 0, 2))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let wikiSomeEol () =
        let text = Text.mkText "[[t"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let wikiSomeWs () =
        let text = Text.mkText "[[t "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 0, 3))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let wikiSomeAndTextAfter () =
        let text = Text.mkText "[[t other"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 0, 3))
        Assert.Equal(Some expected, comp)
