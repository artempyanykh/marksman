module Marksman.CodeActionTests

open Ionide.LanguageServerProtocol.Types
open Xunit

open Marksman.Helpers
open Marksman.Misc

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
