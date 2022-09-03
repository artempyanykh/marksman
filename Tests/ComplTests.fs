module Marksman.ComplTests

open Ionide.LanguageServerProtocol.Types
open Snapper.Attributes
open Xunit
open Snapper

open Marksman.Helpers
open Marksman.Compl
open Marksman.Misc
open Marksman.Workspace

type Helpers =
    static member mkTitleComp(range: Range, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.WikiTitle(range, needsClosing)

    static member mkWikiHeadingComp(range: Range, ?destDoc: string, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.WikiHeading(destDoc, range, needsClosing)

    static member mkDocPathComp(range: Range, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.DocPath(range, needsClosing)

    static member mkDocAnchorComp(range: Range, ?dest: string, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.DocAnchor(dest, range, needsClosing)

    static member mkLinkRefComp(range: Range, ?needsClosing: bool) =
        let needsClosing = defaultArg needsClosing true
        Compl.Compl.LinkReference(range, needsClosing)

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
        let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "[[ "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 0, 2))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someEol () =
        let text = Text.mkText "[[t"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someWs () =
        let text = Text.mkText "[[t "
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 0, 3))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "[[t other"
        let comp = compOfText (Position.Mk(0, 2)) text
        let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 0, 3))
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
        let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "[ "
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 1))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someEol () =
        let text = Text.mkText "[t"
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 1, 0))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someWs () =
        let text = Text.mkText "[t "
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 2))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "[t other"
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 2))
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let emptyBrackets () =
        let text = Text.mkText "[]"
        let comp = compOfText (Position.Mk(0, 1)) text
        let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 1), false)
        Assert.Equal(Some expected, comp)

    [<Fact>]
    let partialReference () =
        let text = Text.mkText "[l]["
        let comp = compOfText (Position.Mk(0, 4)) text
        let expected = Helpers.mkLinkRefComp (Range.Mk(0, 4, 1, 0))
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

let checkSnapshot (completions: array<CompletionItem>) =
    let fmtItem (ci: CompletionItem) =
        ci.TextEdit
        |> Option.map (fun te -> $"{te.Range.DebuggerDisplay}: {te.NewText}")
        |> Option.defaultValue "<no-edit>"

    let lines = Array.map fmtItem completions

    lines.ShouldMatchSnapshot()

[<StoreSnapshotsPerClass>]
module Candidates =
    let doc1 =
        FakeDoc.Mk(
            path = "doc1.md",
            contentLines =
                //  012345678901234567890
                [| "# H1" // 0
                   "[[#"
                   "# A"
                   "## H2" // 3
                   "#B"
                   "## H2"
                   "[](/doc%202.md#)" |] // 6
                //  012345678901234567890
        )

    let doc2 =
        FakeDoc.Mk(path = "doc 2.md", contentLines = [| "# H1"; "[[doc1#"; "## D2 H2" |])

    let folder = FakeFolder.Mk([ doc1; doc2 ])

    [<Fact>]
    let noDupsOnAchor_intraFile () =
        let cand = findCandidates (Position.Mk(1, 3)) doc1.path folder
        checkSnapshot cand

    [<Fact>]
    let noDupsOnAchor_crossFile () =
        let cand = findCandidates (Position.Mk(1, 3)) doc1.path folder
        checkSnapshot cand
        
    [<Fact>]
    let fileWithSpaces_anchor () =
        let cand = findCandidates (Position.Mk(6, 15)) doc1.path folder
        checkSnapshot cand
