module Marksman.CodeActionTests

open Ionide.LanguageServerProtocol.Types
open Xunit

open Marksman.Helpers
open Marksman.Misc
open Marksman.Folder
open Marksman.Paths

module CreateMissingFileTests =
    [<Fact>]
    let shouldCreateWhenNoFileExists () =
        let doc1 = FakeDoc.Mk([| "# Doc 1"; "## Sub 1" |], path = "doc1.md")
        let doc2 = FakeDoc.Mk([| "[[doc3]]" |], path = "doc2.md")
        let folder = FakeFolder.Mk([ doc1; doc2 ])

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca =
            CodeActions.createMissingFile (Range.Mk(0, 3, 0, 3)) caCtx doc2 folder Seq.empty

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
            CodeActions.createMissingFile (Range.Mk(0, 3, 0, 3)) caCtx doc2 folder Seq.empty

        Assert.Equal(None, ca)

    [<Fact>]
    let noCreateFileActionWhenDocInExtraFolder () =
        // Primary folder: doc2 references [[extra-doc]] which only exists in extra folder
        let doc2 = FakeDoc.Mk([| "[[extra-doc]]" |], path = "doc2.md")
        let primaryFolder = FakeFolder.Mk([ doc2 ])

        // Extra folder contains extra-doc.md
        let extraRoot = dummyRootPath [ "extra" ]
        let extraRootUri = pathToUri extraRoot

        let extraDoc =
            FakeDoc.Mk("# Extra Doc", path = "extra-doc.md", root = "extra")

        let extraFolder =
            Folder.multiFile "extra" (UriWith.mkRoot extraRootUri) [ extraDoc ] None

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca =
            CodeActions.createMissingFile (Range.Mk(0, 3, 0, 3)) caCtx doc2 primaryFolder [
                extraFolder
            ]

        Assert.Equal(None, ca)
