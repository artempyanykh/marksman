module Marksman.RefactorTests

open System.IO
open Ionide.LanguageServerProtocol.Types
open Xunit
open Misc
open Marksman.Folder
open Marksman.Paths

let editsByFile =
    function
    | Refactor.Edit wsEdit ->
        match wsEdit.DocumentChanges with
        | Some docChanges ->
            docChanges
            |> Array.map (fun docChange ->
                let docEdit =
                    match docChange with
                    | TextDocumentEdit docEdit -> docEdit
                    | _ -> failwith $"Refactoring should always produce TextDocumentEdits"

                let doc = Path.GetFileName(docEdit.TextDocument.Uri)
                let ranges = docEdit.Edits |> Array.map (fun x -> x.Range, x.NewText)
                doc, ranges)
            |> Map.ofArray
        | _ ->
            match wsEdit.Changes with
            | Some docEditMap ->
                Map.toSeq docEditMap
                |> Seq.map (fun (doc, edits) ->
                    let ranges = edits |> Array.map (fun x -> x.Range, x.NewText)
                    (Path.GetFileName doc), ranges)
                |> Map.ofSeq
            | _ -> Map.empty
    | other -> failwith ($"Edit ranges are not defined for: {other}")

module RenameTests =
    module ReferenceLinks =
        let mkWorkspace () =
            let doc1 =
                Helpers.FakeDoc.Mk(
                    //  0         1         2
                    //  0123456789012345678901234567890
                    [|
                        "# Doc 1"
                        "Start [lbl1], then [lbl2]."
                        "Then [lbl1] again."
                        "And [broken] label."
                        ""
                        "[lbl1]: https://url1.com"
                        "[lbl2]: https://url2.com"
                    |],
                    path = "doc1.md"
                )

            let folder = Helpers.FakeFolder.Mk([ doc1 ])
            doc1, folder


        [<Fact>]
        let onRefLabel () =
            let pos = Position.Mk(2, 7)
            let doc1, folder = mkWorkspace ()
            let res = Refactor.rename true folder Seq.empty doc1 pos "newLbl"

            let expectedRanges =
                Map.ofSeq [
                    "doc1.md",
                    [|
                        Range.Mk(5, 1, 5, 5), "newLbl"
                        Range.Mk(2, 6, 2, 10), "newLbl"
                        Range.Mk(1, 7, 1, 11), "newLbl"
                    |]
                ]

            let actualRanges = editsByFile res

            Assert.Equal<Range * string>(
                Map.find "doc1.md" expectedRanges,
                Map.find "doc1.md" actualRanges
            )

        [<Fact>]
        let onDefLabel () =
            let pos = Position.Mk(5, 3)
            let doc1, folder = mkWorkspace ()
            let res = Refactor.rename true folder Seq.empty doc1 pos "newLbl"

            let expectedRanges =
                Map.ofSeq [
                    "doc1.md",
                    [|
                        Range.Mk(5, 1, 5, 5), "newLbl"
                        Range.Mk(2, 6, 2, 10), "newLbl"
                        Range.Mk(1, 7, 1, 11), "newLbl"
                    |]
                ]

            let actualRanges = editsByFile res

            Assert.Equal<Range * string>(
                Map.find "doc1.md" expectedRanges,
                Map.find "doc1.md" actualRanges
            )

    module HeadingLinks =
        let mkWorkspace () =
            let doc1 =
                Helpers.FakeDoc.Mk(
                    //   0         1         2
                    //   0123456789012345678901234567890
                    [|
                        "# Doc 1"
                        "## Doc 1.2"
                        "## Doc 1.3"
                        "See [[#doc-12]]"
                        "Also [](#doc-12)"
                    |],
                    path = "doc1.md"
                )

            let doc2 =
                Helpers.FakeDoc.Mk(
                    //   0         1         2
                    //   0123456789012345678901234567890
                    [|
                        "# Doc 2"
                        "[[doc-1]]"
                        "[[doc-1#doc-12]]"
                        "[](/doc1.md#doc-12)"
                        // filename wiki-link
                        "[[doc1]]"
                    |],
                    path = "doc2.md"
                )

            let folder = Helpers.FakeFolder.Mk([ doc1; doc2 ])
            doc1, doc2, folder


        [<Fact>]
        let onTitle () =
            let pos = Position.Mk(0, 3)
            let doc1, _doc2, folder = mkWorkspace ()
            let res = Refactor.rename true folder Seq.empty doc1 pos "New Title"

            let expectedRanges =
                Map.ofSeq [
                    "doc1.md", [| Range.Mk(0, 2, 0, 7), "New Title" |]
                    "doc2.md",
                    [|
                        Range.Mk(2, 2, 2, 7), "new-title"
                        Range.Mk(1, 2, 1, 7), "new-title"
                    |]
                ]

            let actualRanges = editsByFile res

            Assert.Equal<Range * string>(
                Map.find "doc1.md" expectedRanges,
                Map.find "doc1.md" actualRanges
            )

            Assert.Equal<Range * string>(
                Map.find "doc2.md" expectedRanges,
                Map.find "doc2.md" actualRanges
            )

        [<Fact>]
        let onSubtitle () =
            let pos = Position.Mk(1, 5)
            let doc1, _doc2, folder = mkWorkspace ()
            let res = Refactor.rename true folder Seq.empty doc1 pos "New Title"

            let expectedRanges =
                Map.ofSeq [
                    "doc1.md",
                    [|
                        Range.Mk(4, 9, 4, 15), "new-title"
                        Range.Mk(3, 7, 3, 13), "New Title"
                        Range.Mk(1, 3, 1, 10), "New Title"
                    |]
                    "doc2.md",
                    [|
                        Range.Mk(3, 12, 3, 18), "new-title"
                        Range.Mk(2, 8, 2, 14), "New Title"
                    |]
                ]

            let actualRanges = editsByFile res

            Assert.Equal<Range * string>(
                Map.find "doc1.md" expectedRanges,
                Map.find "doc1.md" actualRanges
            )

            Assert.Equal<Range * string>(
                Map.find "doc2.md" expectedRanges,
                Map.find "doc2.md" actualRanges
            )

        // Known limitation: subtitle (##) rename does not propagate cross-folder.
        // findDefRefs sets docNameForCrossRef = None for Header defs, so the
        // cross-folder search is never attempted. Only H1/Title renames propagate.
        [<Fact>]
        let onSubtitle_propagatesToExtraFolder () =
            let doc1, _doc2, folder = mkWorkspace ()

            let extraDoc =
                Helpers.FakeDoc.Mk(
                    "# Extra Doc\n[[doc-1#doc-12]]",
                    path = "extra.md",
                    root = "extra"
                )

            let extraRootUri = Helpers.pathToUri (Helpers.dummyRootPath [ "extra" ])

            let extraFolder =
                Folder.multiFile "extra" (UriWith.mkRoot extraRootUri) [ extraDoc ] None

            let res =
                Refactor.rename true folder [ extraFolder ] doc1 (Position.Mk(1, 5)) "New Title"

            let edits = editsByFile res

            Assert.True(Map.containsKey "doc1.md" edits, "Expected edits in primary-folder doc")
            // Cross-folder subtitle rename now propagates to extra-folder referencing docs
            Assert.True(
                Map.containsKey "extra.md" edits,
                "Subtitle rename should propagate cross-folder"
            )

module CrossFolderRenameTests =
    let private extraRootUri =
        Helpers.pathToUri (Helpers.dummyRootPath [ "extra" ])

    let private mkExtraDoc path content = Helpers.FakeDoc.Mk(content, path = path, root = "extra")

    let private mkExtraFolder docs =
        Folder.multiFile "extra" (UriWith.mkRoot extraRootUri) docs None

    // Rename H1 in the extra-folder doc; primary folder has a wiki-link to it.
    // referencingFolders = [primaryFolder] → edits must appear in both.
    [<Fact>]
    let renameTitleInExtraFolder_propagatesToPrimaryFolder () =
        let extraDoc =
            mkExtraDoc "brett-scorza.md" "# Brett Scorza\nSome content."

        let extraFolder = mkExtraFolder [ extraDoc ]

        let primaryDoc =
            Helpers.FakeDoc.Mk([| "# Primary"; "See [[brett-scorza]]." |], path = "primary.md")

        let primaryFolder = Helpers.FakeFolder.Mk([ primaryDoc ])

        let res =
            Refactor.rename
                true
                extraFolder
                [ primaryFolder ]
                extraDoc
                (Position.Mk(0, 3))
                "Brett Scorzetti"

        let edits = editsByFile res

        Assert.True(Map.containsKey "brett-scorza.md" edits, "Expected edits in extra-folder doc")

        Assert.True(
            Map.find "brett-scorza.md" edits
            |> Array.exists (fun (_, t) -> t = "Brett Scorzetti"),
            "Expected H1 title updated in extra-folder doc"
        )

        Assert.True(Map.containsKey "primary.md" edits, "Expected edits in primary-folder doc")

        Assert.True(
            Map.find "primary.md" edits
            |> Array.exists (fun (_, t) -> t = "brett-scorzetti"),
            "Expected link slug updated in primary-folder doc"
        )

    // Rename H1 in the primary folder; extra folder has a link back.
    // Extra folders are NOT referencingFolders — that direction is not supported.
    [<Fact>]
    let renameTitleInPrimaryFolder_doesNotTouchExtraFolder () =
        let primaryDoc =
            Helpers.FakeDoc.Mk([| "# Primary Doc"; "Some content." |], path = "primary.md")

        let primaryFolder = Helpers.FakeFolder.Mk([ primaryDoc ])

        let extraDoc =
            mkExtraDoc "extra-ref.md" "# Extra Ref\nSee [[primary-doc]]."

        let _extraFolder = mkExtraFolder [ extraDoc ]

        let res =
            Refactor.rename
                true
                primaryFolder
                Seq.empty
                primaryDoc
                (Position.Mk(0, 3))
                "New Primary Doc"

        let edits = editsByFile res

        Assert.True(Map.containsKey "primary.md" edits, "Expected edits in primary-folder doc")
        Assert.False(Map.containsKey "extra-ref.md" edits, "Extra-folder doc should not be touched")
