module Marksman.DiagTest

open Marksman.Index
open Xunit

open Marksman.Misc
open Marksman.Parser
open Marksman.Workspace
open Marksman.Diag

let makeFakeDocument (content: string) : Doc =
    let text = Text.mkText content
    let cst = parseText text

    { path = PathUri.fromString "memory://fake.md"
      rootPath = PathUri.fromString "memory://"
      version = None
      text = text
      cst = cst }


[<Fact>]
let documentIndex_1 () =
    let doc = makeFakeDocument "# T1\n# T2"

    let titles =
        Index.titles doc.Index |> Array.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)
