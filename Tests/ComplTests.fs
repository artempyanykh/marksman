module Marksman.ComplTests

open Ionide.LanguageServerProtocol.Types
open Xunit

open Marksman.Compl
open Marksman.Misc

let mkTitleComp range = Compl.Compl.WikiTitle(range, true)
let mkLinkRefComp range = Compl.Compl.LinkReference(range, true)
let mkLinkRefCompClosed range = Compl.Compl.LinkReference(range, false)

type Helpers =
    static member mkWikiHeadingComp(range: Range, ?destDoc: string, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.WikiHeading(destDoc, range, needsClosing)

    static member mkDocPathComp(range: Range, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.DocPath(range, needsClosing)

    static member mkDocAnchorComp(range: Range, ?dest: string, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.DocAnchor(dest, range, needsClosing)

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

    [<Fact>]
    let emptyHeading () =
        let text = Text.mkText "[[#"
        let comp = compOfText (Position.Mk(0, 3)) text
        let expected = Helpers.mkWikiHeadingComp (Range.Mk(0, 3, 1, 0))
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

    [<Fact>]
    let emptyBrackets () =
        let text = Text.mkText "[]"
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = mkLinkRefCompClosed (Range.Mk(0, 1, 0, 1))
        Assert.Equal(Some expected, comp)

module DocPathOfText =
    [<Fact>]
    let empty () =
        let text = Text.mkText ""
        let comp = compOfText (Position.Mk(0, 0)) text
        Assert.Equal(None, comp)

    [<Fact>]
    let emptyEol () =
        let text = Text.mkText "("
        let comp = compOfText (Position.Mk(0, 1)) text
        Assert.Equal(None, comp)
        
    [<Fact>]
    let emptyLinkEol () =
        let text = Text.mkText "]("
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "]( "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 2))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyNonEolFurther () =
        let text = Text.mkText "]( "
        let comp = compOfText (Position.Mk(0, 3)) text
        Assert.Equal(None, comp)

    [<Fact>]
    let someEol () =
        let text = Text.mkText "](t"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someWs () =
        let text = Text.mkText "](t "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "](t other"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyBrackets () =
        let text = Text.mkText "]()"
        let comp = compOfText (Position.Mk(0, 2)) text

        let expected =
            Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 2), needsClosing = false)

        Assert.Equal(Some expected, comp)

    [<Fact>]
    let bracketsAndOpenParen () =
        let text = Text.mkText "[]("
        let comp = compOfText (Position.Mk(0, 3)) text

        let expected = Helpers.mkDocPathComp (Range.Mk(0, 3, 1, 0))

        Assert.Equal(Some expected, comp)

module DocAnchorOfText =
    [<Fact>]
    let test1 () =
        let text = Text.mkText "](t# other"
        let comp = compOfText (Position.Mk(0, 2)) text

        let expected =
            Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3), needsClosing = false)

        Assert.Equal(Some expected, comp)

        let comp = compOfText (Position.Mk(0, 3)) text

        let expected =
            Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3), needsClosing = false)

        Assert.Equal(Some expected, comp)

        let comp = compOfText (Position.Mk(0, 4)) text
        let expected = Helpers.mkDocAnchorComp (Range.Mk(0, 4, 0, 4), dest = "t")
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let test2 () =
        let text = Text.mkText "](#a other"
        let comp = compOfText (Position.Mk(0, 3)) text
        let expected = Helpers.mkDocAnchorComp (Range.Mk(0, 3, 0, 4))
        Assert.Equal(Some expected, comp)

        let comp = compOfText (Position.Mk(0, 5)) text
        Assert.Equal(None, comp)

    [<Fact>]
    let test3 () =
        let text = Text.mkText "(#a )"
        let comp = compOfText (Position.Mk(0, 4)) text
        Assert.Equal(None, comp)

    [<Fact>]
    let test4 () =
        let text = Text.mkText "](#a)"
        let comp = compOfText (Position.Mk(0, 4)) text

        let expected =
            Helpers.mkDocAnchorComp (Range.Mk(0, 3, 0, 4), needsClosing = false)

        Assert.Equal(Some expected, comp)

    [<Fact>]
    let test5 () =
        //                      012345678
        let text = Text.mkText "](doc.md#"
        let comp = compOfText (Position.Mk(0, 9)) text

        let expected =
            Helpers.mkDocAnchorComp (Range.Mk(0, 9, 1, 0), dest = "doc.md")

        Assert.Equal(Some expected, comp)
