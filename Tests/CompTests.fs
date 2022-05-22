module Marksman.CompTests

open Ionide.LanguageServerProtocol.Types
open Xunit

open Marksman.Comp
open Marksman.Misc

let mkTitleComp range = Comp.Comp.Title(range, true)
let mkLinkRefComp range = Comp.Comp.LinkReference(range, true)

module WikiOfText =
    [<Fact>]
    let empty () =
        let text = Text.mkText ""
        let comp = compOfText (Position.Mk(0, 0)) text
        Assert.Equal(None, comp)

    [<Fact>]
    let emptyEol () =
        let text = Text.mkText "[["
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "[[ "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 0, 2))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someEol () =
        let text = Text.mkText "[[t"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someWs () =
        let text = Text.mkText "[[t "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 0, 3))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "[[t other"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = mkTitleComp (Range.Mk(0, 2, 0, 3))
        Assert.Equal(Some expected, comp)

module LinkOfText =
    [<Fact>]
    let empty () =
        let text = Text.mkText ""
        let comp = compOfText (Position.Mk(0, 0)) text
        Assert.Equal(None, comp)

    [<Fact>]
    let emptyEol () =
        let text = Text.mkText "["
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = mkLinkRefComp (Range.Mk(0, 1, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "[ "
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = mkLinkRefComp (Range.Mk(0, 1, 0, 1))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someEol () =
        let text = Text.mkText "[t"
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = mkLinkRefComp (Range.Mk(0, 1, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someWs () =
        let text = Text.mkText "[t "
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = mkLinkRefComp (Range.Mk(0, 1, 0, 2))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "[t other"
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = mkLinkRefComp (Range.Mk(0, 1, 0, 2))
        Assert.Equal(Some expected, comp)
