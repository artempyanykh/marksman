module Marksman.Refs

open Marksman.Cst
open Marksman.Workspace
open Marksman.Index
open FSharpPlus.Operators

let resolveLink (folder: Folder) (doc: Doc) (link: Element) : option<Ref> =
    URef.ofElement link >>= fun uref -> Folder.resolveRef uref doc folder

let resolveLinks (folder: Folder) (doc: Doc) : Map<Element, Ref> =
    let links = Index.links doc.index

    links
    |> Seq.collect (fun link ->
        match resolveLink folder doc link with
        | Some ref -> [ link, ref ]
        | _ -> [])
    |> Map.ofSeq

let findReferencingElements (refMap: Map<Element, Ref>) (target: Ref) : seq<Element> =
    seq {
        for KeyValue (el, ref) in refMap do
            if ref = target then yield el else ()
    }

let findElementRefs (folder: Folder) (doc: Doc) (el: Element) : seq<Doc * Element> =
    let refToFind =
        match el with
        | MLD ld -> Ref.LinkDef(doc, ld) |> Some
        | H h when Heading.isTitle h.data -> Ref.Doc(doc) |> Some
        | H h -> Ref.Heading(doc, h) |> Some
        | other when Element.isLink other ->
            let docRefs = resolveLinks folder doc
            Map.tryFind other docRefs
        | _ -> None

    match refToFind with
    | None -> []
    | Some (Ref.LinkDef _ as ld) ->
        let docRefs = resolveLinks folder doc
        findReferencingElements docRefs ld |> Seq.map (fun el -> doc, el)
    | Some refToFind ->
        seq {
            for KeyValue (_, targetDoc) in folder.docs do
                let targetDocRefs = resolveLinks folder targetDoc

                let backRefs =
                    findReferencingElements targetDocRefs refToFind
                    |> Seq.map (fun el -> targetDoc, el)

                yield! backRefs
        }
