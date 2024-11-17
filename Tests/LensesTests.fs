module Marksman.LensesTests

open Newtonsoft.Json.Linq
open Xunit

module LspServer = Ionide.LanguageServerProtocol.Server

open Marksman.State

open Marksman.Helpers

[<Fact>]
let basicHeaderLenses () =
    let d1 =
        FakeDoc.Mk(
            path = "d1.md",
            contentLines = [| "# Doc 1"; "## Sub"; "[[#Sub]]"; "## No ref" |]
        )

    let d2 =
        FakeDoc.Mk(path = "d2.md", contentLines = [| "# Doc 2"; "[[Doc 1#Sub]]" |])

    let f = FakeFolder.Mk([ d1; d2 ])

    let lenses =
        Lenses.forDoc ClientDescription.empty f d1
        |> Array.map (fun lens -> $"{lens.Command.Value}, {lens.Range}")

    checkInlineSnapshot id lenses [
        "{ Title = \"1 reference\""
        "  Command = \"marksman.findReferences\""
        "  Arguments = None }, (0,0)-(0,7)"
        "{ Title = \"2 references\""
        "  Command = \"marksman.findReferences\""
        "  Arguments = None }, (1,0)-(1,6)"
    ]

[<Fact>]
let basicHeaderLenses_withCommandArguments () =
    let d1 =
        FakeDoc.Mk(
            path = "d1.md",
            contentLines = [| "# Doc 1"; "## Sub"; "[[#Sub]]"; "## No ref" |]
        )

    let d2 =
        FakeDoc.Mk(path = "d2.md", contentLines = [| "# Doc 2"; "[[Doc 1#Sub]]" |])

    let f = FakeFolder.Mk([ d1; d2 ])

    let client = {
        ClientDescription.empty with
            caps = {
                ClientDescription.empty.caps with
                    Experimental = Some(JToken.Parse("""{"codeLensFindReferences": true}"""))
            }
    }

    let lensesData =
        Lenses.forDoc client f d1
        |> Array.map (fun lens ->
            let data =
                lens.Command.Value.Arguments.Value[0]
                |> LspServer.deserialize<Lenses.FindReferencesData>

            {| Title = lens.Command.Value.Title; Data = data |})

    Assert.Equal(2, lensesData.Length)
    Assert.Equal("1 reference", lensesData[0].Title)
    Assert.Equal(1, lensesData[0].Data.Locations.Length)
    Assert.Equal("2 references", lensesData[1].Title)
    Assert.Equal(2, lensesData[1].Data.Locations.Length)


[<Fact>]
let basicLinkDefLenses () =
    let d1 =
        FakeDoc.Mk(
            path = "d1.md",
            contentLines = [| "[foo]"; "[foo][]"; "[bar]"; ""; "[foo]: /url" |]
        )

    let f = FakeFolder.Mk([ d1 ])

    let lenses =
        Lenses.forDoc ClientDescription.empty f d1
        |> Array.map (fun lens -> $"{lens.Command.Value}, {lens.Range}")

    checkInlineSnapshot id lenses [
        "{ Title = \"2 references\""
        "  Command = \"marksman.findReferences\""
        "  Arguments = None }, (4,0)-(4,11)"
    ]
