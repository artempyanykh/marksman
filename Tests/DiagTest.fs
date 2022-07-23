module Marksman.DiagTest

open Marksman.Index
open Xunit

open Marksman.Workspace
open Marksman.Diag
open Marksman.Helpers

[<Fact>]
let documentIndex_1 () =
    let doc = FakeDoc.mk "# T1\n# T2"

    let titles =
        Index.titles doc.index |> Array.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)

[<Fact>]
let nonBreakingWhitespace () =
    let nbsp = "\u00a0"
    let doc = FakeDoc.mk $"# T1\n##{nbsp}T2"

    match (checkNonBreakingWhitespace doc) with
    | [ NonBreakableWhitespace range ] ->
        Assert.Equal(1, range.Start.Line)
        Assert.Equal(1, range.End.Line)

        Assert.Equal(2, range.Start.Character)
        Assert.Equal(3, range.End.Character)
    | _ -> failwith "Expected NonBreakingWhitespace diagnostic"
