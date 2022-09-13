module Marksman.SymbolsTests

open Xunit

open Ionide.LanguageServerProtocol.Types

open Marksman.Helpers

module DocSymbols =
    let fakeDoc =
        FakeDoc.Mk(
            [| "# A" //
               "## B"
               "### C"
               "## D"
               "# E" |]
        )

    [<Fact>]
    let order_noHierarchy () =
        let syms = Symbols.docSymbols false false fakeDoc

        let symNames =
            match syms with
            | First x -> x
            | _ -> failwith "Unexpected symbol type"
            |> Array.map (fun x -> x.Name)

        Assert.Equal<string>([| "H1: A"; "H2: B"; "H3: C"; "H2: D"; "H1: E" |], symNames)
