module Marksman.Refs

open System
open System.IO

open FSharpPlus.Operators
open Ionide.LanguageServerProtocol.Types

open Marksman.Cst
open Marksman.Index
open Marksman.Misc
open Marksman.Workspace

[<RequireQualifiedAccess>]
type DocRef =
    | Title of title: TextNode
    | Url of url: TextNode

module DocRef =
    let tryResolveToRootPath
        (folderPath: string)
        (srcDocPath: string)
        (url: string)
        : option<string> =
        if Uri.IsWellFormedUriString(url, UriKind.Absolute) then
            None
        else if url.StartsWith('/') then
            Some(url.TrimStart('/'))
        else
            try
                let srcDirComponents =
                    Path.GetDirectoryName(srcDocPath).Split(Path.DirectorySeparatorChar)

                // Make sure to retain the root component of the path
                if srcDocPath.StartsWith('/') && srcDirComponents[0] = "" then
                    srcDirComponents[0] <- "/"

                let urlComponents = url.Split('\\', '/')
                let allComponents = Array.append srcDirComponents urlComponents
                let targetPath = Path.Combine(allComponents)
                let targetPathNormalized = (PathUri.fromString targetPath).LocalPath
                let folderPathNormalized = (PathUri.fromString folderPath).LocalPath

                let rooted =
                    if targetPathNormalized.StartsWith(folderPathNormalized) then
                        let relPath =
                            Path.GetRelativePath(folderPathNormalized, targetPathNormalized)

                        let relPathNormalized =
                            String.concat "/" (relPath.Split(Path.DirectorySeparatorChar))

                        Some relPathNormalized
                    else
                        None

                rooted
            with
            | :? UriFormatException
            | :? InvalidOperationException -> None

    let tryFindDoc (folder: Folder) (srcDoc: Doc) (docRef: DocRef) : option<Doc> =
        match docRef with
        | DocRef.Title title -> Folder.tryFindDocBySlug (Slug.ofString title.text) folder
        | DocRef.Url url ->
            let url =
                tryResolveToRootPath srcDoc.rootPath.LocalPath srcDoc.path.LocalPath url.text

            url >>= flip Folder.tryFindDocByUrl folder

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
            let doc = DocRef.tryFindDoc folder srcDoc docRef
            doc |>> Ref.Doc
        | Uref.Heading (docRef, heading) ->
            let doc =
                docRef
                >>= DocRef.tryFindDoc folder srcDoc
                |> Option.defaultValue srcDoc

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
