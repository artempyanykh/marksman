module Marksman.DiagTest

open Marksman.DB
open Xunit

open Marksman.Misc
open Marksman.Parser
open Marksman.Domain
open Marksman.Diag

let makeFakeDocument (content: string) : Doc =
    let text = Text.mkText content
    let elements = parseText text

    { path = PathUri.fromString "memory://fake.md"
      relPath = "fake.md"
      version = None
      text = text
      elements = elements }


[<Fact>]
let documentIndex_1 () =
    let doc = makeFakeDocument "# T1\n# T2"
    let db = DocDB.ofDoc doc

    let titles = DocDB.titles db |> List.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)
