module Marksman.ComplTests

open Ionide.LanguageServerProtocol.Types
open Snapper.Attributes
open Xunit
open Snapper

open Marksman.Helpers
open Marksman.Compl
open Marksman.Misc
open Marksman.Workspace

let tryParsePartialElement text line col = PartialElement.inText text (Position.Mk(line, col))
let parsePartialElement text line col = tryParsePartialElement text line col |> Option.get

[<StoreSnapshotsPerClass>]
[<UpdateSnapshots>]
module PartialElementWiki =
    let checkSnapshot els =
        let lines = Seq.map (fun x -> x.ToString().Lines()) els |> Array.concat
        lines.ShouldMatchSnapshot()

    [<Fact>]
    let empty () =
        let text = Text.mkText ""
        Assert.Equal(None, tryParsePartialElement text 0 0)

    [<Fact>]
    let emptyEof () =
        let text = Text.mkText "[["
        checkSnapshot [ (parsePartialElement text 0 2) ]

    [<Fact>]
    let emptyEol () =
        let text = Text.mkText "[[\n"
        checkSnapshot [ (parsePartialElement text 0 2) ]

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "[[ "
        checkSnapshot [ (parsePartialElement text 0 2) ]

    [<Fact>]
    let someEof () =
        let text = Text.mkText "[[t"
        checkSnapshot [ (parsePartialElement text 0 2) ]

    [<Fact>]
    let someEol () =
        let text = Text.mkText "[[to\n"
        checkSnapshot [ (parsePartialElement text 0 2) ]

    [<Fact>]
    let someWs () =
        let text = Text.mkText "[[t "
        checkSnapshot [ (parsePartialElement text 0 2) ]

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "[[t other"
        checkSnapshot [ (parsePartialElement text 0 2) ]

    [<Fact>]
    let emptyHeading () =
        let text = Text.mkText "[[#"
        checkSnapshot [ (parsePartialElement text 0 3) ]

    [<Fact>]
    let nonEmptyHeading () =
        let text = Text.mkText "[[#hea] "
        checkSnapshot [ (parsePartialElement text 0 3) ]

[<StoreSnapshotsPerClass>]
[<UpdateSnapshots>]
module PartialElementReference =
    let checkSnapshot els =
        let lines = Seq.map (fun x -> x.ToString().Lines()) els |> Array.concat
        lines.ShouldMatchSnapshot()

    [<Fact>]
    let empty () =
        let text = Text.mkText ""
        Assert.Equal(None, tryParsePartialElement text 0 0)

    [<Fact>]
    let emptyEof () =
        let text = Text.mkText "["
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let emptyEol () =
        let text = Text.mkText "[\n"
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "[ "
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let someEof () =
        let text = Text.mkText "[t"
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let someEol () =
        let text = Text.mkText "[t\n"
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let someWs () =
        let text = Text.mkText "[t "
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "[t other"
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let emptyBrackets () =
        let text = Text.mkText "[]"
        checkSnapshot [ (parsePartialElement text 0 1) ]

    [<Fact>]
    let partialReference () =
        let text = Text.mkText "[l]["
        checkSnapshot [ (parsePartialElement text 0 4) ]

[<StoreSnapshotsPerClass>]
module PartialElementInline =
    let checkSnapshot els =
        let lines = Seq.map (fun x -> x.ToString().Lines()) els |> Array.concat
        lines.ShouldMatchSnapshot()

    [<Fact>]
    let empty () =
        let text = Text.mkText ""
        Assert.Equal(None, tryParsePartialElement text 0 0)

    [<Fact>]
    let emptyEof () =
        let text = Text.mkText "("
        checkSnapshot [ parsePartialElement text 0 1 ]

    [<Fact>]
    let emptyEol () =
        let text = Text.mkText "(\n"
        checkSnapshot [ parsePartialElement text 0 1 ]

    [<Fact>]
    let emptyLinkEof () =
        let text = Text.mkText "]("
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let emptyLinkEol () =
        let text = Text.mkText "](\n"
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let emptyNonEol () =
        let text = Text.mkText "]( "
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let emptyNonEolFurther () =
        let text = Text.mkText "]( "
        Assert.Equal(None, tryParsePartialElement text 0 3)

    [<Fact>]
    let someEol () =
        let text = Text.mkText "](t"
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let someWs () =
        let text = Text.mkText "](t "
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let someAndTextAfter () =
        let text = Text.mkText "](t other"
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let someAndTextBeforeAfter () =
        //                      01234567890
        let text = Text.mkText "before](t other"
        checkSnapshot [ parsePartialElement text 0 8 ]

    [<Fact>]
    let emptyBrackets () =
        let text = Text.mkText "]()\n"
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let bracketsAndOpenParen () =
        let text = Text.mkText "[]("
        checkSnapshot [ parsePartialElement text 0 3 ]

    [<Fact>]
    let bracketsWithTextAndOpenParen () =
        let text = Text.mkText "[b]("
        checkSnapshot [ parsePartialElement text 0 4 ]

    [<Fact>]
    let anchor1 () =
        let text = Text.mkText "](t# other"

        checkSnapshot
            [ parsePartialElement text 0 2
              parsePartialElement text 0 3
              parsePartialElement text 0 4 ]

    [<Fact>]
    let anchor2 () =
        let text = Text.mkText "](#a other"
        checkSnapshot [ parsePartialElement text 0 3 ]

        Assert.Equal(None, tryParsePartialElement text 0 5)

    [<Fact>]
    let anchor3 () =
        let text = Text.mkText "(#a )"
        Assert.Equal(None, tryParsePartialElement text 0 4)

    [<Fact>]
    let anchor4 () =
        let text = Text.mkText "](#a)"
        checkSnapshot [ parsePartialElement text 0 4 ]

    [<Fact>]
    let anchor5 () =
        //                      012345678
        let text = Text.mkText "](doc.md#"
        checkSnapshot [ parsePartialElement text 0 9 ]

    [<Fact>]
    let anchor6 () =
        let text = Text.mkText "(# "
        checkSnapshot [ parsePartialElement text 0 2 ]

// module WikiOfText =
//     [<Fact>]
//     let empty () =
//         let text = Text.mkText ""
//         let comp = compOfText (Position.Mk(0, 0)) text
//         Assert.Equal(None, comp)
//
//     [<Fact>]
//     let emptyEol () =
//         let text = Text.mkText "[["
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 1, 0))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let emptyNonEol () =
//         let text = Text.mkText "[[ "
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 0, 2))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someEol () =
//         let text = Text.mkText "[[t"
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 1, 0))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someWs () =
//         let text = Text.mkText "[[t "
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 0, 3))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someAndTextAfter () =
//         let text = Text.mkText "[[t other"
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkTitleComp (Range.Mk(0, 2, 0, 3))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let emptyHeading () =
//         let text = Text.mkText "[[#"
//         let comp = compOfText (Position.Mk(0, 3)) text
//         let expected = Helpers.mkWikiHeadingComp (Range.Mk(0, 3, 1, 0))
//         Assert.Equal(Some expected, comp)
//
// module LinkOfText =
//     [<Fact>]
//     let empty () =
//         let text = Text.mkText ""
//         let comp = compOfText (Position.Mk(0, 0)) text
//         Assert.Equal(None, comp)
//
//     [<Fact>]
//     let emptyEol () =
//         let text = Text.mkText "["
//         let comp = compOfText (Position.Mk(0, 1)) text
//         let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 1, 0))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let emptyNonEol () =
//         let text = Text.mkText "[ "
//         let comp = compOfText (Position.Mk(0, 1)) text
//         let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 1))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someEol () =
//         let text = Text.mkText "[t"
//         let comp = compOfText (Position.Mk(0, 1)) text
//         let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 1, 0))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someWs () =
//         let text = Text.mkText "[t "
//         let comp = compOfText (Position.Mk(0, 1)) text
//         let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 2))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someAndTextAfter () =
//         let text = Text.mkText "[t other"
//         let comp = compOfText (Position.Mk(0, 1)) text
//         let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 2))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let emptyBrackets () =
//         let text = Text.mkText "[]"
//         let comp = compOfText (Position.Mk(0, 1)) text
//         let expected = Helpers.mkLinkRefComp (Range.Mk(0, 1, 0, 1), false)
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let partialReference () =
//         let text = Text.mkText "[l]["
//         let comp = compOfText (Position.Mk(0, 4)) text
//         let expected = Helpers.mkLinkRefComp (Range.Mk(0, 4, 1, 0))
//         Assert.Equal(Some expected, comp)
//
// module DocPathOfText =
//     [<Fact>]
//     let empty () =
//         let text = Text.mkText ""
//         let comp = compOfText (Position.Mk(0, 0)) text
//         Assert.Equal(None, comp)
//
//     [<Fact>]
//     let emptyEol () =
//         let text = Text.mkText "("
//         let comp = compOfText (Position.Mk(0, 1)) text
//         Assert.Equal(None, comp)
//
//     [<Fact>]
//     let emptyLinkEol () =
//         let text = Text.mkText "]("
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 1, 0))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let emptyNonEol () =
//         let text = Text.mkText "]( "
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 2))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let emptyNonEolFurther () =
//         let text = Text.mkText "]( "
//         let comp = compOfText (Position.Mk(0, 3)) text
//         Assert.Equal(None, comp)
//
//     [<Fact>]
//     let someEol () =
//         let text = Text.mkText "](t"
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 1, 0))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someWs () =
//         let text = Text.mkText "](t "
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let someAndTextAfter () =
//         let text = Text.mkText "](t other"
//         let comp = compOfText (Position.Mk(0, 2)) text
//         let expected = Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3))
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let emptyBrackets () =
//         let text = Text.mkText "]()"
//         let comp = compOfText (Position.Mk(0, 2)) text
//
//         let expected =
//             Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 2), needsClosing = false)
//
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let bracketsAndOpenParen () =
//         let text = Text.mkText "[]("
//         let comp = compOfText (Position.Mk(0, 3)) text
//
//         let expected = Helpers.mkDocPathComp (Range.Mk(0, 3, 1, 0))
//
//         Assert.Equal(Some expected, comp)
//
// module DocAnchorOfText =
//     [<Fact>]
//     let test1 () =
//         let text = Text.mkText "](t# other"
//         let comp = compOfText (Position.Mk(0, 2)) text
//
//         let expected =
//             Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3), needsClosing = false)
//
//         Assert.Equal(Some expected, comp)
//
//         let comp = compOfText (Position.Mk(0, 3)) text
//
//         let expected =
//             Helpers.mkDocPathComp (Range.Mk(0, 2, 0, 3), needsClosing = false)
//
//         Assert.Equal(Some expected, comp)
//
//         let comp = compOfText (Position.Mk(0, 4)) text
//         let expected = Helpers.mkDocAnchorComp (Range.Mk(0, 4, 0, 4), dest = "t")
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let test2 () =
//         let text = Text.mkText "](#a other"
//         let comp = compOfText (Position.Mk(0, 3)) text
//         let expected = Helpers.mkDocAnchorComp (Range.Mk(0, 3, 0, 4))
//         Assert.Equal(Some expected, comp)
//
//         let comp = compOfText (Position.Mk(0, 5)) text
//         Assert.Equal(None, comp)
//
//     [<Fact>]
//     let test3 () =
//         let text = Text.mkText "(#a )"
//         let comp = compOfText (Position.Mk(0, 4)) text
//         Assert.Equal(None, comp)
//
//     [<Fact>]
//     let test4 () =
//         let text = Text.mkText "](#a)"
//         let comp = compOfText (Position.Mk(0, 4)) text
//
//         let expected =
//             Helpers.mkDocAnchorComp (Range.Mk(0, 3, 0, 4), needsClosing = false)
//
//         Assert.Equal(Some expected, comp)
//
//     [<Fact>]
//     let test5 () =
//         //                      012345678
//         let text = Text.mkText "](doc.md#"
//         let comp = compOfText (Position.Mk(0, 9)) text
//
//         let expected =
//             Helpers.mkDocAnchorComp (Range.Mk(0, 9, 1, 0), dest = "doc.md")
//
//         Assert.Equal(Some expected, comp)


[<StoreSnapshotsPerClass>]
module Candidates =
    let checkSnapshot (completions: array<CompletionItem>) =
        let fmtItem (ci: CompletionItem) =
            ci.TextEdit
            |> Option.map (fun te -> $"{te.Range.DebuggerDisplay}: {te.NewText}")
            |> Option.defaultValue "<no-edit>"

        let lines = Array.map fmtItem completions

        lines.ShouldMatchSnapshot()

    let globalDoc1 =
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

    let globalDoc2 =
        FakeDoc.Mk(path = "doc 2.md", contentLines = [| "# H2"; "[[h1#"; "## D2 H2" |])

    let globalFolder = FakeFolder.Mk([ globalDoc1; globalDoc2 ])

    [<Fact>]
    let noDupsOnAchor_intraFile () =
        checkSnapshot (findCandidates globalFolder (Doc.path globalDoc1) (Position.Mk(1, 3)))

    [<Fact>]
    let noDupsOnAchor_crossFile () =
        checkSnapshot (findCandidates globalFolder (Doc.path globalDoc2) (Position.Mk(1, 5)))

    [<Fact>]
    let fileWithSpaces_anchor () =
        checkSnapshot (findCandidates globalFolder (Doc.path globalDoc1) (Position.Mk(6, 15)))

    [<Fact>]
    let docAndHeadingFuzzy () =
        //                                                                   012345
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[do#]]" |])

        let doc2 =
            FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2"; "## H2.1"; "## H2.2" |])

        let doc3 =
            FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3"; "## H3" |])

        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 5)))

    [<Fact>]
    let referenceEmptyBrackets () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[]"; ""; "[link-1]: url1"; "[link-2]: url2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 1)))

    [<Fact>]
    let referenceNonEmptyBrackets () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[l]"; ""; "[link-1]: url1"; "[link-2]: url2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 2)))

    [<Fact>]
    let inlineEmpty () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[]()" |])

        let doc2 = FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2" |])
        let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3" |])
        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 3)))

    [<Fact>]
    let partialWikiDoc () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[" |])

        let doc2 = FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2" |])
        let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3" |])
        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 2)))

    [<Fact>]
    let partialWikiHeading () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[[#"; "## H2.1"; "## H2.2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 3)))

    [<Fact>]
    let partialWikiDocHeading () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[d#" |])

        let doc2 =
            FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2"; "## H2" |])

        let doc3 =
            FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3"; "## H3" |])

        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 4)))


    [<Fact>]
    let partialWikiDocHeading_FilePathStem () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[d#" |])

        let doc2 =
            FakeDoc.Mk(path = "sub2/doc2.md", contentLines = [| "# Doc 2"; "## H2" |])

        let doc3 =
            FakeDoc.Mk(path = "sub3/this is doc 3.md", contentLines = [| "# Doc 3"; "## H3" |])

        let config =
            { Config.Config.Empty with complWikiStyle = Some Config.FilePathStem }

        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ], config = config)

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 4)))
        

    [<Fact>]
    let partialWikiDocHeading_FileStem () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[d#" |])

        let doc2 =
            FakeDoc.Mk(path = "sub2/doc2.md", contentLines = [| "# Doc 2"; "## H2" |])

        let doc3 =
            FakeDoc.Mk(path = "sub3/this is doc 3.md", contentLines = [| "# Doc 3"; "## H3" |])

        let config =
            { Config.Config.Empty with complWikiStyle = Some Config.FileStem }

        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ], config = config)

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 4)))

    [<Fact>]
    let partialReferenceEmpty () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "["; ""; "[link-1]: url1"; "[link-2]: url2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 1)))

    [<Fact>]
    [<UpdateSnapshots>]
    let partialInlineHeading () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[link](#"; "## H2.1"; "## H2.2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 8)))

    [<Fact>]
    let partialInlineDoc () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[](" |])

        let doc2 = FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2" |])
        let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3" |])
        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidates folder (Doc.path doc1) (Position.Mk(1, 3)))
