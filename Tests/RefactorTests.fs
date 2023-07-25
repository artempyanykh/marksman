module Marksman.RefactorTests

open System.IO
open Ionide.LanguageServerProtocol.Types
open Xunit
open Misc

let editRanges =
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
                let ranges = docEdit.Edits |> Array.map (fun x -> x.Range)
                doc, ranges)
            |> Map.ofArray
        | _ ->
            match wsEdit.Changes with
            | Some docEditMap ->
                Map.toSeq docEditMap
                |> Seq.map (fun (doc, edits) ->
                    let ranges = edits |> Array.map (fun x -> x.Range)
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
                    [| "# Doc 1"
                       "Start [lbl1], then [lbl2]."
                       "Then [lbl1] again."
                       "And [broken] label."
                       ""
                       "[lbl1]: https://url1.com"
                       "[lbl2]: https://url2.com" |],
                    path = "doc1.md"
                )

            let folder = Helpers.FakeFolder.Mk([ doc1 ])
            doc1, folder


        [<Fact>]
        let onRefLabel () =
            let pos = Position.Mk(2, 7)
            let doc1, folder = mkWorkspace ()
            let res = Refactor.rename true folder doc1 pos "newLbl"

            let expectedRanges =
                Map.ofSeq
                    [ "doc1.md",
                      [| Range.Mk(5, 1, 5, 5)
                         Range.Mk(2, 6, 2, 10)
                         Range.Mk(1, 7, 1, 11) |] ]

            let actualRanges = editRanges res

            Assert.Equal<Range>(Map.find "doc1.md" expectedRanges, Map.find "doc1.md" actualRanges)

        [<Fact>]
        let onDefLabel () =
            let pos = Position.Mk(5, 3)
            let doc1, folder = mkWorkspace ()
            let res = Refactor.rename true folder doc1 pos "newLbl"

            let expectedRanges =
                Map.ofSeq
                    [ "doc1.md",
                      [| Range.Mk(5, 1, 5, 5)
                         Range.Mk(2, 6, 2, 10)
                         Range.Mk(1, 7, 1, 11) |] ]

            let actualRanges = editRanges res

            Assert.Equal<Range>(Map.find "doc1.md" expectedRanges, Map.find "doc1.md" actualRanges)

    module HeadingLinks =
        let mkWorkspace () =
            let doc1 =
                Helpers.FakeDoc.Mk(
                    //  0         1         2
                    //  0123456789012345678901234567890
                    [| "# Doc 1"
                       "## Doc 1.2"
                       "## Doc 1.3"
                       "See [[#doc-12]]"
                       "Also [](#doc-12)" |],
                    path = "doc1.md"
                )

            let doc2 =
                Helpers.FakeDoc.Mk(
                    //  0         1         2
                    //  0123456789012345678901234567890
                    [| "# Doc 2"
                       "[[doc-1]]"
                       "[[doc-1#doc-12]]"
                       "[](/doc1.md#doc-12)"
                       // filename wiki-link
                       "[[doc1]]" |],
                    path = "doc2.md"
                )

            let folder = Helpers.FakeFolder.Mk([ doc1; doc2 ])
            doc1, doc2, folder


        [<Fact>]
        let onTitle () =
            let pos = Position.Mk(0, 3)
            let doc1, _doc2, folder = mkWorkspace ()
            let res = Refactor.rename true folder doc1 pos "New Title"

            let expectedRanges =
                Map.ofSeq
                    [ "doc1.md", [| Range.Mk(0, 2, 0, 7) |]
                      "doc2.md", [| Range.Mk(2, 2, 2, 7); Range.Mk(1, 2, 1, 7) |] ]

            let actualRanges = editRanges res

            Assert.Equal<Range>(Map.find "doc1.md" expectedRanges, Map.find "doc1.md" actualRanges)
            Assert.Equal<Range>(Map.find "doc2.md" expectedRanges, Map.find "doc2.md" actualRanges)

        [<Fact>]
        let onSubtitle () =
            let pos = Position.Mk(1, 5)
            let doc1, _doc2, folder = mkWorkspace ()
            let res = Refactor.rename true folder doc1 pos "New Title"

            let expectedRanges =
                Map.ofSeq
                    [ "doc1.md",
                      [| Range.Mk(4, 9, 4, 15)
                         Range.Mk(3, 7, 3, 13)
                         Range.Mk(1, 3, 1, 10) |]
                      "doc2.md", [| Range.Mk(3, 12, 3, 18); Range.Mk(2, 8, 2, 14) |] ]

            let actualRanges = editRanges res

            Assert.Equal<Range>(Map.find "doc1.md" expectedRanges, Map.find "doc1.md" actualRanges)
            Assert.Equal<Range>(Map.find "doc2.md" expectedRanges, Map.find "doc2.md" actualRanges)
