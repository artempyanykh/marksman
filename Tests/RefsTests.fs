module Marksman.RefsTests

open Ionide.LanguageServerProtocol.Types
open System.IO
open Xunit

open Marksman.Misc
open Marksman.Helpers
open Marksman.Cst
open Marksman.Workspace
open Marksman.Refs

let doc1 =
    FakeDoc.mk (
        path = "doc1.md",
        content =
            "\
# Doc 1

## D1 H2.1

[[doc-2#d2-h22]]

## D1 H2.2
"
    )

let doc2 =
    FakeDoc.mk (
        path = "doc2.md",
        content =
            "\
# Doc 2

# D2 H2.1

[d2-link-1]

[[#d2-h22]]

[d2-link-1]

# D2 H2.2

[[doc-1]]
[lbl1](/doc1.md)

[d2-link-1]: some-url
"
    )

let folder = FakeFolder.mk [ doc1; doc2 ]

let stripRefs (refs: seq<Doc * Element>) =
    refs
    |> Seq.map (fun (doc, el) ->
        Path.GetFileName(doc.path.DocumentUri), (Element.range el).DebuggerDisplay)
    |> Array.ofSeq

[<Fact>]
let refToLinkDef_atDef () =
    let def = Cst.elementAtPos (Position.Mk(15, 3)) doc2.cst
    let def = def |> Option.defaultWith (fun _ -> failwith "No def")
    let refs = Ref.findElementRefs folder doc2 def
    let refs = stripRefs refs

    checkInlineSnapshot
        (fun x -> x.ToString())
        refs
        [ "(doc2.md, (4,0)-(4,11))"; "(doc2.md, (8,0)-(8,11))" ]

[<Fact>]
let refToLinkDef_atLink () =
    let def = Cst.elementAtPos (Position.Mk(8, 4)) doc2.cst
    let def = def |> Option.defaultWith (fun _ -> failwith "No def")
    let refs = Ref.findElementRefs folder doc2 def
    let refs = stripRefs refs

    checkInlineSnapshot
        (fun x -> x.ToString())
        refs
        [ "(doc2.md, (4,0)-(4,11))"; "(doc2.md, (8,0)-(8,11))" ]

[<Fact>]
let refToDoc_atTitle () =
    let title = Cst.elementAtPos (Position.Mk(0, 2)) doc1.cst
    let title = title |> Option.defaultWith (fun _ -> failwith "No title")
    let refs = Ref.findElementRefs folder doc1 title
    let refs = stripRefs refs

    checkInlineSnapshot
        (fun x -> x.ToString())
        refs
        [ "(doc2.md, (12,0)-(12,9))"; "(doc2.md, (13,0)-(13,16))" ]

[<Fact>]
let refToDoc_atLink () =
    let wl = Cst.elementAtPos (Position.Mk(4, 4)) doc1.cst
    let wl = wl |> Option.defaultWith (fun _ -> failwith "No title")
    let refs = Ref.findElementRefs folder doc1 wl
    let refs = stripRefs refs

    checkInlineSnapshot
        (fun x -> x.ToString())
        refs
        [ "(doc1.md, (4,0)-(4,16))"; "(doc2.md, (6,0)-(6,11))" ]
