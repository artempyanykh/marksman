module Marksman.CodeActionTests

open type System.Environment

open Ionide.LanguageServerProtocol.Types
open Xunit

open Marksman.Helpers
open Marksman.Misc
open Marksman.Doc

module CreateMissingFileTests =
    [<Fact>]
    let shouldCreateWhenNoFileExists () =
        let doc1 = FakeDoc.Mk([| "# Doc 1"; "## Sub 1" |], path = "doc1.md")
        let doc2 = FakeDoc.Mk([| "[[doc3]]" |], path = "doc2.md")
        let folder = FakeFolder.Mk([ doc1; doc2 ])

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca =
            CodeActions.createMissingFile (Range.Mk(0, 3, 0, 3)) caCtx doc2 folder

        match ca with
        | Some { name = "Create `doc3.md`" } -> Assert.True(true)
        | _ -> Assert.True(false)

    [<Fact>]
    let shouldNotCreateWhenRefBrokenButFileExists () =
        let doc1 = FakeDoc.Mk([| "# Doc 1"; "## Sub 1" |], path = "doc1.md")
        let doc2 = FakeDoc.Mk([| "[[doc1#Sub 2]]" |], path = "doc2.md")
        let folder = FakeFolder.Mk([ doc1; doc2 ])

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca =
            CodeActions.createMissingFile (Range.Mk(0, 3, 0, 3)) caCtx doc2 folder

        Assert.Equal(None, ca)

module LinkToReferenceTests =
    let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

    [<Fact>]
    let shouldConvertWhenReferenceExists () =
        let doc =
            FakeDoc.Mk(
                [|
                    "[link][ref]"
                    "[inline](https://link/)"
                    ""
                    "[ref]: https://link/"
                |],
                path = "doc.md"
            )

        let ca = CodeActions.linkToReference (Range.Mk(1, 4, 1, 4)) caCtx doc

        let expected: CodeActions.MultiEditAction option =
            Some {
                name = "Replace link with reference `ref`"
                edits = [ Range.Mk(1, 0, 1, 23), "[inline][ref]" ]
            }

        Assert.Equal(expected, ca)

    [<Fact>]
    let shouldCreateWhenReferenceDoesNotExist () =
        let doc =
            FakeDoc.Mk(
                [|
                    "[link][ref]"
                    "[inline Link_Thing](https://link/)"
                    ""
                    "[ref]: https://other/"
                |],
                path = "doc.md"
            )

        let ca = CodeActions.linkToReference (Range.Mk(1, 4, 1, 4)) caCtx doc

        let expected: CodeActions.MultiEditAction option =
            Some {
                name = "Convert link to new reference `inline-link-thing`"
                edits = [
                    Range.Mk(1, 0, 1, 34), "[inline Link_Thing][inline-link-thing]"
                    Range.Mk(4, 0, 4, 0), $"{NewLine}[inline-link-thing]: https://link/"
                ]
            }

        Assert.Equal(expected, ca)

    [<Fact>]
    let shouldReturnNoneWhenCursorNotOnLink () =
        let doc = FakeDoc.Mk([| "some plain text"; "[inline](https://link/)" |], path = "doc.md")

        let ca = CodeActions.linkToReference (Range.Mk(0, 5, 0, 5)) caCtx doc

        Assert.Equal(None, ca)

    [<Fact>]
    let shouldReturnNoneWhenCursorOnReferenceLink () =
        let doc =
            FakeDoc.Mk(
                [| "[link][ref]"; ""; "[ref]: https://link/" |],
                path = "doc.md"
            )

        let ca = CodeActions.linkToReference (Range.Mk(0, 3, 0, 3)) caCtx doc

        Assert.Equal(None, ca)

    [<Fact>]
    let shouldReturnNoneWhenInlineLinkHasNoUrl () =
        let doc = FakeDoc.Mk([| "[text]()" |], path = "doc.md")

        let ca = CodeActions.linkToReference (Range.Mk(0, 3, 0, 3)) caCtx doc

        Assert.Equal(None, ca)

    [<Fact>]
    let shouldIncludeTitleInNewReference () =
        let doc =
            FakeDoc.Mk(
                [| "[My Link](https://example.com \"My Title\")" |],
                path = "doc.md"
            )

        let ca = CodeActions.linkToReference (Range.Mk(0, 3, 0, 3)) caCtx doc

        let expected: CodeActions.MultiEditAction option =
            Some {
                name = "Convert link to new reference `my-link`"
                edits = [
                    Range.Mk(0, 0, 0, 41), "[My Link][my-link]"
                    Range.Mk(1, 0, 1, 0),
                    $"{NewLine}[my-link]: https://example.com \"My Title\""
                ]
            }

        Assert.Equal(expected, ca)

    [<Fact>]
    let shouldConvertCorrectLinkWhenMultipleExist () =
        let doc =
            FakeDoc.Mk(
                [|
                    "[first](https://first.com)"
                    "[second](https://second.com)"
                |],
                path = "doc.md"
            )

        // Cursor on the second link
        let ca = CodeActions.linkToReference (Range.Mk(1, 5, 1, 5)) caCtx doc

        let expected: CodeActions.MultiEditAction option =
            Some {
                name = "Convert link to new reference `second`"
                edits = [
                    Range.Mk(1, 0, 1, 28), "[second][second]"
                    Range.Mk(2, 0, 2, 0), $"{NewLine}[second]: https://second.com"
                ]
            }

        Assert.Equal(expected, ca)

    [<Fact>]
    let shouldConvertSingleLinkDocument () =
        let doc = FakeDoc.Mk([| "[text](https://url.com)" |], path = "doc.md")

        let ca = CodeActions.linkToReference (Range.Mk(0, 3, 0, 3)) caCtx doc

        let expected: CodeActions.MultiEditAction option =
            Some {
                name = "Convert link to new reference `text`"
                edits = [
                    Range.Mk(0, 0, 0, 23), "[text][text]"
                    Range.Mk(1, 0, 1, 0), $"{NewLine}[text]: https://url.com"
                ]
            }

        Assert.Equal(expected, ca)
