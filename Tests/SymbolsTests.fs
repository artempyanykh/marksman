module Marksman.SymbolsTests

open Xunit

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Helpers
open Marksman.Workspace

module WorkspaceSymbol =
    let mkSymbolInfo name kind loc =
        { Name = name
          Kind = kind
          Location = loc
          Tags = None
          Deprecated = None
          ContainerName = None }

    let doc1 =
        FakeDoc.Mk(path = "doc1.md", contentLines = [| "# A"; "#tag1" |])

    let doc2 =
        FakeDoc.Mk(path = "doc2.md", contentLines = [| "# B"; "#tag1 #tag2" |])

    let folder = FakeFolder.Mk [ doc1; doc2 ]
    let workspace = Workspace.ofFolders None [ folder ]

    [<Fact>]
    let symbols_noQuery =
        let symbols = Symbols.workspaceSymbols "" workspace

        Assert.Equal(
            [ mkSymbolInfo
                  "H1: A"
                  SymbolKind.String
                  { Uri = doc1.Id.Uri; Range = Range.Mk(0, 0, 0, 3) }
              mkSymbolInfo
                  "Tag: tag1"
                  SymbolKind.String
                  { Uri = doc1.Id.Uri; Range = Range.Mk(1, 0, 1, 5) }
              mkSymbolInfo
                  "H1: B"
                  SymbolKind.String
                  { Uri = doc2.Id.Uri; Range = Range.Mk(0, 0, 0, 3) }
              mkSymbolInfo
                  "Tag: tag1"
                  SymbolKind.String
                  { Uri = doc2.Id.Uri; Range = Range.Mk(1, 0, 1, 5) }
              mkSymbolInfo
                  "Tag: tag2"
                  SymbolKind.String
                  { Uri = doc2.Id.Uri; Range = Range.Mk(1, 6, 1, 11) } ],
            symbols
        )

    [<Fact>]
    let symbols_withQuery () =
        let symbols = Symbols.workspaceSymbols "Tag:" workspace

        Assert.Equal(
            [ mkSymbolInfo
                  "Tag: tag1"
                  SymbolKind.String
                  { Uri = doc1.Id.Uri; Range = Range.Mk(1, 0, 1, 5) }
              mkSymbolInfo
                  "Tag: tag1"
                  SymbolKind.String
                  { Uri = doc2.Id.Uri; Range = Range.Mk(1, 0, 1, 5) }
              mkSymbolInfo
                  "Tag: tag2"
                  SymbolKind.String
                  { Uri = doc2.Id.Uri; Range = Range.Mk(1, 6, 1, 11) } ],
            symbols
        )

module DocSymbols =
    let fakeDoc =
        FakeDoc.Mk(
            [| "# E" //
               "## D"
               "#t1"
               "### B"
               "#t2"
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

        Assert.Equal<string>(
            [| "H1: E"
               "H2: D"
               "H3: B"
               "H2: C"
               "H1: A"
               "Tag: t1"
               "Tag: t2" |],
            symNames
        )

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

        Assert.Equal<string>([| "E"; "D"; "t1"; "B"; "t2"; "C"; "A" |], names)
