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

    [<Fact>]
    let order_Hierarchy () =
        let syms = Symbols.docSymbols true false fakeDoc

        let syms =
            match syms with
            | Second x -> x
            | _ -> failwith "Unexpected symbol type"

        let names = ResizeArray()

        let rec collect (sym: DocumentSymbol) =
            names.Add(sym.Name)
            sym.Children |> Option.defaultValue [||] |> Array.iter collect

        syms |> Array.iter collect

        Assert.Equal<string>([| "E"; "D"; "B"; "C"; "A" |], names)
