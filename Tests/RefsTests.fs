module Marksman.RefsTests

open Ionide.LanguageServerProtocol.Types
open System.IO
open Xunit

open Marksman.Misc
open Marksman.Helpers
open Marksman.Cst
open Marksman.Workspace
open Marksman.Refs

module DocRefTests =
    [<Fact>]
    let relPath_1 () =
        let folder = dummyRootPath [ "rootFolder" ]
        let docPath = dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]

        let actual =
            DocRef.tryResolveToRootPath folder docPath "../doc.md" |> Option.get

        Assert.Equal("doc.md", actual)

    [<Fact>]
    let relPath_2 () =
        let folder = dummyRootPath [ "rootFolder" ]
        let docPath = dummyRootPath [ "rootFolder"; "doc1.md" ]

        let actual =
            DocRef.tryResolveToRootPath folder docPath "./doc2.md" |> Option.get

        Assert.Equal("doc2.md", actual)

    [<Fact>]
    let relPath_non_exist () =
        let folder = dummyRootPath [ "rootFolder" ]
        let docPath = dummyRootPath [ "rootFolder"; "doc1.md" ]

        let actual = DocRef.tryResolveToRootPath folder docPath "../doc2.md"
        Assert.Equal(None, actual)

    [<Fact>]
    let rootPath () =
        let folder = dummyRootPath [ "rootFolder" ]
        let docPath = dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]

        let actual =
            DocRef.tryResolveToRootPath folder docPath "/doc.md" |> Option.get

        Assert.Equal("doc.md", actual)

    [<Fact>]
    let url_no_schema_FP () =
        let folder = dummyRootPath [ "rootFolder" ]
        let docPath = dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]

        let actual =
            DocRef.tryResolveToRootPath folder docPath "www.google.com"
            |> Option.get

        Assert.Equal("subfolder/www.google.com", actual)

    [<Fact>]
    let url_schema () =
        let folder = dummyRootPath [ "rootFolder" ]
        let docPath = dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]

        let actual =
            DocRef.tryResolveToRootPath folder docPath "http://www.google.com"

        Assert.Equal(None, actual)

let doc1 =
    FakeDoc.Mk(
        path = "doc1.md",
        contentLines =
            [| "# Doc 1" // 0
               "" // 1
               "## D1 H2.1" // 2
               "" // 3
               "[[doc-2#d2-h22]]" // 4
               "" // 5
               "## D1 H2.2" // 6
               "" // 7
               "[[#dup]]" // 8
               "" // 9
               "## Dup" // 10
               "Entry 1" // 11
               "" // 12
               "## Dup" // 13
               "Entry 2" |] // 14
    )

let doc2 =
    FakeDoc.Mk(
        path = "doc2.md",
        contentLines =
            [| "# Doc 2" // 0
               "" // 1
               "# D2 H2.1" // 2
               "" // 3
               "[d2-link-1]" // 4
               "" // 5
               "[[#d2-h22]]" // 6
               "" // 7
               "[d2-link-1]" // 8
               "" // 9
               "# D2 H2.2" // 10
               "" // 11
               "[[doc-1]]" // 12
               "[[doc-1#dup]]" // 13
               "[lbl1](/doc1.md)" // 14
               "" // 15
               "[d2-link-1]: some-url" |] // 16
    )

let folder = FakeFolder.Mk [ doc1; doc2 ]

let stripRefs (refs: seq<Doc * Element>) =
    refs
    |> Seq.map (fun (doc, el) ->
        Path.GetFileName(doc.path.DocumentUri), (Element.range el).DebuggerDisplay)
    |> Array.ofSeq

module RefsTests =
    [<Fact>]
    let refToLinkDef_atDef () =
        let def =
            Cst.elementAtPos (Position.Mk(16, 3)) doc2.cst
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs false folder doc2 def |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (4,0)-(4,11))"; "(doc2.md, (8,0)-(8,11))" ]

    [<Fact>]
    let refToLinkDef_atDef_withDecl () =
        let def =
            Cst.elementAtPos (Position.Mk(16, 3)) doc2.cst
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs true folder doc2 def |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (16,0)-(16,21))"
              "(doc2.md, (4,0)-(4,11))"
              "(doc2.md, (8,0)-(8,11))" ]

    [<Fact>]
    let refToLinkDef_atLink () =
        let def =
            Cst.elementAtPos (Position.Mk(8, 4)) doc2.cst
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs false folder doc2 def |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (4,0)-(4,11))"; "(doc2.md, (8,0)-(8,11))" ]

    [<Fact>]
    let refToLinkDef_atLink_withDecl () =
        let def =
            Cst.elementAtPos (Position.Mk(8, 4)) doc2.cst
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs true folder doc2 def |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (16,0)-(16,21))"
              "(doc2.md, (4,0)-(4,11))"
              "(doc2.md, (8,0)-(8,11))" ]

    [<Fact>]
    let refToDoc_atTitle () =
        let title =
            Cst.elementAtPos (Position.Mk(0, 2)) doc1.cst
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs false folder doc1 title |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (12,0)-(12,9))"
              "(doc2.md, (13,0)-(13,13))"
              "(doc2.md, (14,0)-(14,16))" ]

    [<Fact>]
    let refToDoc_atTitle_withDecl () =
        let title =
            Cst.elementAtPos (Position.Mk(0, 2)) doc1.cst
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs true folder doc1 title |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc1.md, (0,0)-(0,7))"
              "(doc2.md, (12,0)-(12,9))"
              "(doc2.md, (13,0)-(13,13))"
              "(doc2.md, (14,0)-(14,16))" ]

    [<Fact>]
    let refToDoc_atLink () =
        let wl =
            Cst.elementAtPos (Position.Mk(4, 4)) doc1.cst
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs false folder doc1 wl |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc1.md, (4,0)-(4,16))"; "(doc2.md, (6,0)-(6,11))" ]

    [<Fact>]
    let refToDoc_atLink_withDecl () =
        let wl =
            Cst.elementAtPos (Position.Mk(4, 4)) doc1.cst
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs true folder doc1 wl |> stripRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (10,0)-(10,9))"
              "(doc1.md, (4,0)-(4,16))"
              "(doc2.md, (6,0)-(6,11))" ]

// TODO: add tests for title refs
