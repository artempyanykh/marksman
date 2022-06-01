module Marksman.DiagTest

open Marksman.Index
open Xunit

open Marksman.Misc
open Marksman.Parser
open Marksman.Workspace
open Marksman.Diag

let makeFakeDocument (content: string) : Doc =
    let text = Text.mkText content
    Doc.mk (PathUri.fromString "memory://fake.md") (PathUri.fromString "memory://") None text

[<Fact>]
let documentIndex_1 () =
    let doc = makeFakeDocument "# T1\n# T2"

    let titles =
        Index.titles doc.index |> Array.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)
