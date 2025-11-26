module Marksman.CodeActionTests

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
    [<Fact>]
    let shouldConvertWhenReferenceExits () =
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

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca = CodeActions.linkToReference (Range.Mk(1, 4, 1, 4)) caCtx doc

        let expected =
            Some {
                Title = "Replace link with reference `ref`"
                Kind = Some CodeActionKind.RefactorRewrite
                Command = None
                Data = None
                Diagnostics = None
                Disabled = None
                IsPreferred = None
                Edit =
                    Some {
                        DocumentChanges = None
                        Changes =
                            Some
                                Map[Doc.uri doc,
                                    [|
                                        { Range = Range.Mk(1, 0, 1, 23); NewText = "[inline][ref]" }
                                    |]]
                    }
            }

        Assert.Equivalent(expected, ca)

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

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca = CodeActions.linkToReference (Range.Mk(1, 4, 1, 4)) caCtx doc

        let expected =
            Some {
                Title = "Convert link to new reference `inline-link-thing`"
                Kind = Some CodeActionKind.RefactorRewrite
                Command = None
                Data = None
                Diagnostics = None
                Disabled = None
                IsPreferred = None
                Edit =
                    Some {
                        DocumentChanges = None
                        Changes =
                            Some
                                Map[Doc.uri doc,
                                    [|
                                        {
                                            Range = Range.Mk(1, 0,
                                            1, 34)
                                            NewText = "[inline Link_Thing][inline-link-thing]"
                                        }
                                        {
                                            Range = Range.Mk(5, 0, 6, 34)
                                            NewText = "[inline-link-thing]: https://link/"
                                        }
                                    |]]
                    }
            }

        Assert.Equivalent(expected, ca)
