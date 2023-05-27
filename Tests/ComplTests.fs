module Marksman.ComplTests

open Ionide.LanguageServerProtocol.Types
open Snapper.Attributes
open Xunit
open Snapper

open Marksman.Helpers
open Marksman.Compl
open Marksman.Misc

let tryParsePartialElement text line col = PartialElement.inText text (Position.Mk(line, col))
let parsePartialElement text line col = tryParsePartialElement text line col |> Option.get

[<StoreSnapshotsPerClass>]
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

[<StoreSnapshotsPerClass>]
module PartialElementTag =
    let checkSnapshot els =
        let lines = Seq.map (fun x -> x.ToString().Lines()) els |> Array.concat
        lines.ShouldMatchSnapshot()

    [<Fact>]
    let opening1 () =
        let text = Text.mkText "#"
        checkSnapshot [ parsePartialElement text 0 1 ]

    [<Fact>]
    let opening2 () =
        let text = Text.mkText "# "
        checkSnapshot [ parsePartialElement text 0 1 ]

    [<Fact>]
    let opening3 () =
        let text = Text.mkText "## "
        checkSnapshot [ parsePartialElement text 0 2 ]

    [<Fact>]
    let opening4 () =
        // This one is arguably where we may want to NOT suggest any completion.
        // IOW we may require that a hash sign is preceded with a punctuation or
        // a whitespace char.
        //                      01234567
        let text = Text.mkText "hello# "
        checkSnapshot [ parsePartialElement text 0 6 ]


[<StoreSnapshotsPerClass>]
module Candidates =
    let checkSnapshot (completions: array<CompletionItem>) =
        let fmtItem (ci: CompletionItem) =
            let filterText = ci.FilterText |> Option.defaultValue "<no-filter>"

            ci.TextEdit
            |> Option.map (fun te -> $"{te.Range.DebuggerDisplay}: {te.NewText} / {filterText}")
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
        checkSnapshot (findCandidatesInDoc globalFolder globalDoc1 (Position.Mk(1, 3)))

    [<Fact>]
    let noDupsOnAchor_crossFile () =
        checkSnapshot (findCandidatesInDoc globalFolder globalDoc2 (Position.Mk(1, 5)))

    [<Fact>]
    let fileWithSpaces_anchor () =
        checkSnapshot (findCandidatesInDoc globalFolder globalDoc1 (Position.Mk(6, 15)))

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

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 5)))

    [<Fact>]
    let referenceEmptyBrackets () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[]"; ""; "[link-1]: url1"; "[link-2]: url2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 1)))

    [<Fact>]
    let referenceNonEmptyBrackets () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[l]"; ""; "[link-1]: url1"; "[link-2]: url2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 2)))

    [<Fact>]
    let inlineEmpty () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[]()" |])

        let doc2 = FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2" |])
        let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3" |])
        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 3)))

    [<Fact>]
    let partialWikiDoc () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[" |])

        let doc2 = FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2" |])
        let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3" |])
        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 2)))

    [<Fact>]
    let partialWikiHeading () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[[#"; "## H2.1"; "## H2.2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 3)))

    [<Fact>]
    let partialWikiDocHeading () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[d#" |])

        let doc2 =
            FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2"; "## H2" |])

        let doc3 =
            FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3"; "## H3" |])

        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 4)))


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

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 4)))


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

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 4)))

    [<Fact>]
    let partialWikiDoc_FileStem_ArbitraryPath () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[[sun" |])

        let doc2 =
            FakeDoc.Mk(path = "20221218.md", contentLines = [| "# Sun 18 Dec 2022" |])

        let folder = FakeFolder.Mk([ doc1; doc2 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 5)))

    [<Fact>]
    let partialReferenceEmpty () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "["; ""; "[link-1]: url1"; "[link-2]: url2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 1)))

    [<Fact>]
    let partialInlineHeading () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines = [| "# Doc 1"; "[link](#"; "## H2.1"; "## H2.2" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 8)))

    [<Fact>]
    let partialInlineDoc () =
        let doc1 =
            FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Doc 1"; "[](" |])

        let doc2 = FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Doc 2" |])
        let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Doc 3" |])
        let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])

        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 3)))

    [<StoreSnapshotsPerClass>]
    module WikiWithSpaces_TitleSlug =
        let doc1 = FakeDoc.Mk(path = "doc1.md", contentLines = [| "# A A B B" |])
        let doc2 = FakeDoc.Mk(path = "doc2.md", contentLines = [| "# A A C C" |])
        let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# A B B D" |])

        let doc4 =
            FakeDoc.Mk(path = "doc4.md", contentLines = [| "[[a]]"; "[[a-a]]"; "[[a-b]]" |])

        let folder = FakeFolder.Mk([ doc1; doc2; doc3; doc4 ])

        [<Fact>]
        let test1 () = checkSnapshot (findCandidatesInDoc folder doc4 (Position.Mk(0, 3)))

        [<Fact>]
        let test2 () = checkSnapshot (findCandidatesInDoc folder doc4 (Position.Mk(1, 5)))

        [<Fact>]
        let test3 () = checkSnapshot (findCandidatesInDoc folder doc4 (Position.Mk(2, 5)))

    [<StoreSnapshotsPerClass>]
    module WikiWithSpaces_FileStem =
        let doc1 = FakeDoc.Mk(path = "doc one.md", contentLines = [| "# Doc 1" |])
        let doc2 = FakeDoc.Mk(path = "doc two.md", contentLines = [| "# Doc 2" |])

        let doc3 =
            FakeDoc.Mk(path = "another doc.md", contentLines = [| "# Doc 3" |])

        let doc4 =
            FakeDoc.Mk(path = "doc4.md", contentLines = [| "[[do]]"; "[[doc o]]"; "[[ano]]" |])

        let folder =
            FakeFolder.Mk(
                [ doc1; doc2; doc3; doc4 ],
                config = { Config.Config.Empty with complWikiStyle = Some Config.FileStem }
            )

        [<Fact>]
        let test1 () = checkSnapshot (findCandidatesInDoc folder doc4 (Position.Mk(0, 4)))

        [<Fact>]
        let test2 () = checkSnapshot (findCandidatesInDoc folder doc4 (Position.Mk(1, 7)))

        [<Fact>]
        let test3 () = checkSnapshot (findCandidatesInDoc folder doc4 (Position.Mk(2, 5)))

    [<Fact>]
    let wiki_HeadingWithSpecialChars_NotEncoded () =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines =
                    [| "# Doc 1"
                       "## Foo / Bar"
                       "## Baz"
                       //1234
                       "[[#f"
                       "" |]
            )

        let folder = FakeFolder.Mk([ doc1 ])
        checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(3, 4)))

    [<Fact>]
    // # has a special meaning in URIs. Because of this Uri library doesn't handle
    // paths with # well and we can't provide subtitle completion.
    // Best workaround -- don't use paths with # sign.
    let wiki_FileWithSpecialChars_Subtitle_NoCompletionProvided () =
        let doc1 =
            FakeDoc.Mk(
                path = "blah#blah.md",
                contentLines = [| "# Blah Blah"; "## Subtitle 1"; "## Subtitle 2" |]
            )

        let doc2 =
            //                                              0123456789012345
            FakeDoc.Mk(path = "doc2.md", contentLines = [| "[[blah%23blah#]]" |])

        let config =
            { Config.Config.Empty with complWikiStyle = Some Config.FileStem }

        let folder = FakeFolder.Mk([ doc1; doc2 ], config)

        checkSnapshot (findCandidatesInDoc folder doc2 (Position.Mk(0, 14)))

    [<StoreSnapshotsPerClass>]
    module Tags =
        let doc1 =
            FakeDoc.Mk(
                path = "doc1.md",
                contentLines =
                    [| "We have #tag and #anotherTag"
                       //12345678901234567
                       "And an opening # "
                       //12345678901234567
                       "And partial #ta " |]
            )

        let doc2 =
            FakeDoc.Mk(path = "doc2.md", contentLines = [| "And #somethingElse #otherDocTag" |])

        let folder = FakeFolder.Mk([ doc1; doc2 ])

        [<Fact>]
        let tagOpening () = checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(1, 16)))

        [<Fact>]
        let tagWithName () = checkSnapshot (findCandidatesInDoc folder doc1 (Position.Mk(2, 15)))
