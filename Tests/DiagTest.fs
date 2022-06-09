module Marksman.DiagTest

open Marksman.Index
open Xunit

open Marksman.Misc
open Marksman.Parser
open Marksman.Workspace
open Marksman.Diag
open Marksman.Helpers

[<Fact>]
let documentIndex_1 () =
    let doc = makeFakeDocument "# T1\n# T2"

    let titles =
        Index.titles doc.index |> Array.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)
