module Marksman.DiagTest

open Xunit

open Marksman.Misc
open Marksman.Parser
open Marksman.Domain
open Marksman.Diag

let makeFakeDocument (content: string) : Document =
    let text = Text.mkText content
    let elements = Parser.parseText text

    { path = PathUri.fromString "memory://fake.md"
      name = "fake"
      version = None
      text = text
      elements = elements }


[<Fact>]
let documentIndex_1 () =
    let doc = makeFakeDocument "# T1\n# T2"
    let index = indexDocument doc

    let titles =
        index.titles |> List.map Heading.title

    Assert.Equal<string>([ "T1"; "T2" ], titles)
