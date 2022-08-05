module Marksman.RefactorTests

open System.IO
open Ionide.LanguageServerProtocol.Types
open Xunit
open Misc

module RenameTests =
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

    let editRanges =
        function
        | Refactor.Edit wsEdit ->
            match wsEdit.DocumentChanges with
            | Some docEdits ->
                docEdits
                |> Array.map (fun docEdit ->
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

    [<Fact>]
    let onRefLabel () =
        let pos = Position.Mk(2, 7)
        let doc1, folder = mkWorkspace ()
        let res = Refactor.rename true folder doc1 pos "newLbl"

        let expectedRanges =
            Map.ofSeq
                [ "doc1.md",
                  [| Range.Mk(1, 7, 1, 11)
                     Range.Mk(2, 6, 2, 10)
                     Range.Mk(5, 1, 5, 5) |] ]

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
                  [| Range.Mk(1, 7, 1, 11)
                     Range.Mk(2, 6, 2, 10)
                     Range.Mk(5, 1, 5, 5) |] ]

        let actualRanges = editRanges res

        Assert.Equal<Range>(Map.find "doc1.md" expectedRanges, Map.find "doc1.md" actualRanges)
