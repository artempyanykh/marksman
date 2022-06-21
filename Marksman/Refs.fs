﻿module Marksman.Refs

open Ionide.LanguageServerProtocol.Types
open Marksman.Cst
open Marksman.Misc
open Marksman.Workspace
open Marksman.Index
open FSharpPlus.Operators

[<RequireQualifiedAccess>]
type DocRef =
    | Title of title: TextNode
    | Url of url: TextNode

module DocRef =
    let tryFindDoc (folder: Folder) (docRef: DocRef) : option<Doc> =
        match docRef with
        | DocRef.Title title -> Folder.tryFindDocBySlug (Slug.ofString title.text) folder
        | DocRef.Url url -> Folder.tryFindDocByUrl url.text folder

/// Unresolved reference.
[<RequireQualifiedAccess>]
type Uref =
    | Doc of DocRef
    | Heading of doc: option<DocRef> * heading: TextNode
    | LinkDef of TextNode

module Uref =
    let ofElement (el: Element) : option<Uref> =
        match el with
        | WL wl ->
            match wl.data.doc, wl.data.heading with
            | Some doc, Some heading -> Uref.Heading(Some(DocRef.Title doc), heading) |> Some
            | Some doc, None -> Uref.Doc(DocRef.Title doc) |> Some
            | None, Some heading -> Uref.Heading(None, heading) |> Some
            | None, None -> None
        | ML ml ->
            match ml.data with
            | MdLink.IL (_, Some url, _) ->
                let docUrl = DocUrl.ofUrlNode url

                match docUrl.url, docUrl.anchor with
                | Some url, Some anchor -> Uref.Heading(Some(DocRef.Url url), anchor) |> Some
                | Some url, None -> Uref.Doc(DocRef.Url url) |> Some
                | None, Some anchor -> Uref.Heading(None, anchor) |> Some
                | None, None -> None
            | MdLink.IL (_, None, _) -> None
            | MdLink.RS label
            | MdLink.RC label
            | MdLink.RF (_, label) -> Some(Uref.LinkDef label)
        | H _
        | MLD _ -> None

/// Resolved reference.
[<RequireQualifiedAccess>]
type Ref =
    | Doc of Doc
    | Heading of Doc * Node<Heading>
    | LinkDef of Doc * Node<MdLinkDef>

module Ref =
    let doc: Ref -> Doc =
        function
        | Ref.Doc doc -> doc
        | Ref.Heading (doc, _) -> doc
        | Ref.LinkDef (doc, _) -> doc

    let element: Ref -> Element option =
        function
        | Ref.Doc d -> Doc.title d |>> H
        | Ref.Heading (_, h) -> Some(H h)
        | Ref.LinkDef (_, ld) -> Some(MLD ld)

    let range: Ref -> Range =
        function
        | Ref.Doc doc ->
            Doc.title doc
            |> Option.map Node.range
            |> Option.defaultWith doc.text.FullRange
        | Ref.Heading (_, heading) -> heading.range
        | Ref.LinkDef (_, linkDef) -> linkDef.range

    let scope: Ref -> Range =
        function
        | Ref.Doc doc -> doc.text.FullRange()
        | Ref.Heading (_, heading) -> heading.data.scope
        | Ref.LinkDef (_, linkDef) -> linkDef.range

    let tryResolveUref (uref: Uref) (srcDoc: Doc) (folder: Folder) : option<Ref> =
        match uref with
        | Uref.LinkDef label ->
            let ld = srcDoc.index |> Index.tryFindLinkDef label.text
            ld |>> fun x -> Ref.LinkDef(srcDoc, x)
        | Uref.Doc docRef ->
            let doc = DocRef.tryFindDoc folder docRef
            doc |>> Ref.Doc
        | Uref.Heading (docRef, heading) ->
            let doc =
                docRef >>= DocRef.tryFindDoc folder |> Option.defaultValue srcDoc

            let heading =
                doc.index |> Index.tryFindHeadingBySlug (Slug.ofString heading.text)

            heading |>> fun h -> Ref.Heading(doc, h)

    let tryResolveLink (folder: Folder) (doc: Doc) (link: Element) : option<Ref> =
        Uref.ofElement link >>= fun uref -> tryResolveUref uref doc folder

    let resolveLinks (folder: Folder) (doc: Doc) : Map<Element, Ref> =
        let links = Index.links doc.index

        links
        |> Seq.collect (fun link ->
            match tryResolveLink folder doc link with
            | Some ref -> [ link, ref ]
            | _ -> [])
        |> Map.ofSeq

    let private findReferencingElements (refMap: Map<Element, Ref>) (target: Ref) : seq<Element> =
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