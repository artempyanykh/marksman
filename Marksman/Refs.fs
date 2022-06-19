module Marksman.Refs

open Marksman.Cst
open Marksman.Workspace
open Marksman.Index
open FSharpPlus.Operators

type ForwardRefs = Map<Element, Ref>

let resolveLink (folder: Folder) (doc: Doc) (link: Element) : option<Ref> =
    URef.ofElement link >>= fun uref -> Folder.resolveRef uref doc folder

let resolveLinks (folder: Folder) (doc: Doc) : ForwardRefs =
    let links = Index.links doc.index

    links
    |> Seq.collect (fun link ->
        match resolveLink folder doc link with
        | Some ref -> [ link, ref ]
        | _ -> [])
    |> Map.ofSeq

let findElementRefs (folder: Folder) (doc: Doc) (el: Element) : seq<Ref> =
    let docRefs = resolveLinks folder doc
    []
