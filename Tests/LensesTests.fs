module Marksman.LensesTests

open Xunit

open Marksman.Helpers

let d1 =
    FakeDoc.Mk(path = "d1.md", contentLines = [| "# Doc 1"; "## Sub"; "[[#Sub]]"; "## No ref" |])

let d2 =
    FakeDoc.Mk(path = "d2.md", contentLines = [| "# Doc 2"; "[[Doc 1#Sub]]" |])

let f = FakeFolder.Mk([ d1; d2 ])

[<Fact>]
let basicLenses () =
    let lenses =
        Lenses.forDoc f d1
        |> Array.map (fun lens -> $"{lens.Command.Value}, {lens.Range}")

    checkInlineSnapshot
        id
        lenses
        [ "{ Title = \"1 reference\""
          "  Command = \"marksman.findReferences\""
          "  Arguments = None }, (0,0)-(0,7)"
          "{ Title = \"2 references\""
          "  Command = \"marksman.findReferences\""
          "  Arguments = None }, (1,0)-(1,6)" ]
