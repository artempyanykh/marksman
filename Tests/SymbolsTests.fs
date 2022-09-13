module Marksman.SymbolsTests

open Xunit

open Ionide.LanguageServerProtocol.Types

open Marksman.Helpers

module DocSymbols =
    let fakeDoc =
        FakeDoc.Mk(
            [| "# E" //
               "## D"
               "### B"
               "## C"
               "# A" |]
        )

    [<Fact>]
    let order_noHierarchy () =
        let syms = Symbols.docSymbols false false fakeDoc

        let symNames =
            match syms with
            | First x -> x
            | _ -> failwith "Unexpected symbol type"
            |> Array.map (fun x -> x.Name)

        Assert.Equal<string>([| "H1: E"; "H2: D"; "H3: B"; "H2: C"; "H1: A" |], symNames)
