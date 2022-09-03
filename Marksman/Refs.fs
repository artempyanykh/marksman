module Marksman.Refs

open System
open System.IO

open FSharpPlus.Data
open FSharpPlus.Operators
open FSharpPlus.GenericBuilders
open Ionide.LanguageServerProtocol.Types

open Marksman.Cst
open Marksman.Index
open Marksman.Misc
open Marksman.Workspace

[<RequireQualifiedAccess>]
type DocRef =
    | Title of title: string
    | Url of url: string

module DocRef =
    let ofUrl url : option<DocRef> =
        if Uri.IsWellFormedUriString(url, UriKind.Absolute) then
            None
        else
            try
                let ext = Path.GetExtension url
                let ext = ext.ToLowerInvariant()

                if ext = ".md" || ext = ".markdown" || ext = ".mdx" then
                    Some(DocRef.Url url)
                else
                    None
            with :? ArgumentException ->
                None

    let tryResolveToRootPath
        (folderPath: string)
        (srcDocPath: string)
        (url: string)
        : option<string> =
        if Uri.IsWellFormedUriString(url, UriKind.Absolute) then
            None
        else if url.StartsWith('/') then
            Some(url.AbsPathUrlEncodedToRelPath())
        else
            try
                let srcDirComponents =
                    Path.GetDirectoryName(srcDocPath).Split(Path.DirectorySeparatorChar)

                // Make sure to retain the root component of the path
                if srcDocPath.StartsWith('/') && srcDirComponents[0] = "" then
                    srcDirComponents[0] <- "/"

                let urlComponents = url.AbsPathUrlEncodedToRelPath().Split('\\', '/')
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

    let filterMatchingDocs (folder: Folder) (srcDoc: Doc) (docRef: DocRef) : seq<Doc> =
        match docRef with
        | DocRef.Title title -> Folder.filterDocsBySlug (Slug.ofString title) folder
        | DocRef.Url url ->
            let url =
                tryResolveToRootPath srcDoc.rootPath.LocalPath srcDoc.path.LocalPath url

            match url >>= fun url -> Folder.tryFindDocByUrl url folder with
            | Some doc -> [ doc ]
            | _ -> []

    let tryFindFirstDoc (folder: Folder) (srcDoc: Doc) (docRef: DocRef) : option<Doc> =
        filterMatchingDocs folder srcDoc docRef |> Seq.tryHead

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
            | Some doc, Some heading -> Uref.Heading(Some(DocRef.Title doc.text), heading) |> Some
            | Some doc, None -> Uref.Doc(DocRef.Title doc.text) |> Some
            | None, Some heading -> Uref.Heading(None, heading) |> Some
            | None, None -> None
        | ML ml ->
            match ml.data with
            | MdLink.IL (_, Some url, _) ->
                let docUrl = Url.ofUrlNode url

                match docUrl.url, docUrl.anchor with
                | Some url, Some anchor ->
                    DocRef.ofUrl url.text
                    |> Option.map (fun url -> Uref.Heading(Some(url), anchor))
                | Some url, None -> DocRef.ofUrl url.text |> Option.map Uref.Doc
                | None, Some anchor -> Uref.Heading(None, anchor) |> Some
                | None, None -> None
            | MdLink.IL (_, None, _) -> None
            | MdLink.RS label
            | MdLink.RC label
            | MdLink.RF (_, label) -> Some(Uref.LinkDef label)
        | H _
        | YML _
        | MLD _ -> None

    let hasExplicitDoc =
        function
        | Uref.Doc _ -> true
        | Uref.Heading(doc = Some _) -> true
        | Uref.Heading(doc = None) -> false
        | Uref.LinkDef _ -> false

/// Resolved reference.
[<RequireQualifiedAccess>]
type Dest =
    | Doc of Doc
    | Heading of Doc * Node<Heading>
    | LinkDef of Doc * Node<MdLinkDef>

module Dest =
    let doc: Dest -> Doc =
        function
        | Dest.Doc doc -> doc
        | Dest.Heading (doc, _) -> doc
        | Dest.LinkDef (doc, _) -> doc

    let element: Dest -> Element option =
        function
        | Dest.Doc d -> Doc.title d |>> H
        | Dest.Heading (_, h) -> Some(H h)
        | Dest.LinkDef (_, ld) -> Some(MLD ld)

    let range: Dest -> Range =
        function
        | Dest.Doc doc ->
            Doc.title doc
            |> Option.map Node.range
            |> Option.defaultWith doc.text.FullRange
        | Dest.Heading (_, heading) -> heading.range
        | Dest.LinkDef (_, linkDef) -> linkDef.range

    let scope: Dest -> Range =
        function
        | Dest.Doc doc -> doc.text.FullRange()
        | Dest.Heading (_, heading) -> heading.data.scope
        | Dest.LinkDef (_, linkDef) -> linkDef.range

    let uri (ref: Dest) : DocumentUri = doc ref |> Doc.uri

    let location (ref: Dest) : Location = { Uri = uri ref; Range = range ref }

    let tryResolveUref (uref: Uref) (srcDoc: Doc) (folder: Folder) : seq<Dest> =
        match uref with
        | Uref.LinkDef label ->
            let ld = srcDoc.index |> Index.tryFindLinkDef label.text

            match ld |>> fun x -> Dest.LinkDef(srcDoc, x) with
            | None -> Seq.empty
            | Some ld -> [ ld ]
        | Uref.Doc docRef ->
            let doc = DocRef.filterMatchingDocs folder srcDoc docRef
            doc |>> Dest.Doc
        | Uref.Heading (docRef, heading) ->
            let matchingDocs =
                docRef
                |> Option.map (DocRef.filterMatchingDocs folder srcDoc)
                |> Option.defaultValue [ srcDoc ]

            seq {
                for doc in matchingDocs do
                    let headings =
                        doc.index |> Index.filterHeadingBySlug (Slug.ofString heading.text)

                    for h in headings do
                        yield Dest.Heading(doc, h)
            }

    let tryResolveElement (folder: Folder) (doc: Doc) (element: Element) : seq<Dest> =
        match Uref.ofElement element with
        | Some uref -> tryResolveUref uref doc folder
        | None -> Seq.empty

    let resolveLinks (folder: Folder) (doc: Doc) : Map<Element, list<Dest>> =
        let links = Index.links doc.index

        links
        |> Seq.collect (fun link ->
            match tryResolveElement folder doc link |> Seq.toList with
            | refs -> [ link, refs ])
        |> Map.ofSeq

    let private findReferencingElements
        (refMap: Map<Element, list<Dest>>)
        (target: Dest)
        : seq<Element> =
        seq {
            for KeyValue (el, refs) in refMap do
                match target with
                | Dest.Doc targetDoc ->
                    let curUref = Uref.ofElement el

                    let explicitDoc =
                        curUref |> Option.map Uref.hasExplicitDoc |> Option.defaultValue false

                    let doesMatch ref = explicitDoc && doc ref = targetDoc

                    if List.exists doesMatch refs then
                        yield el
                | _ ->
                    if List.exists ((=) target) refs then
                        yield el
        }
        |> Seq.sortBy Element.rangeStart

    /// Finds elements referencing `el`.
    /// When `el` is a link, it's resolved to its destination first and then references to the
    /// destination are found.
    let findElementRefs
        (includeDecl: bool)
        (folder: Folder)
        (srcDoc: Doc)
        (el: Element)
        : seq<Doc * Element> =
        let declsToFind =
            match el with
            | MLD ld -> [ Dest.LinkDef(srcDoc, ld) ]
            | H h when Heading.isTitle h.data -> [ Dest.Doc(srcDoc) ]
            | H h -> [ Dest.Heading(srcDoc, h) ]
            | link when Element.isLink link ->
                let linkToDecl = resolveLinks folder srcDoc
                Map.tryFind link linkToDecl |> Option.defaultValue []
            | _ -> []

        let resolveDecl includeDecl declToFind =
            let targetDocs =
                match declToFind with
                | Dest.LinkDef _ -> [ srcDoc ]
                | Dest.Heading _
                | Dest.Doc _ -> folder.docs |> Map.values |> List.ofSeq

            let referencingEls =
                seq {
                    for targetDoc in targetDocs do
                        let targetDocRefs = resolveLinks folder targetDoc

                        let backRefs =
                            findReferencingElements targetDocRefs declToFind
                            |> Seq.map (fun el -> targetDoc, el)

                        yield! backRefs
                }

            let declEl =
                if includeDecl then
                    let decl =
                        monad' {
                            let! el = element declToFind
                            let doc = doc declToFind
                            doc, el
                        }

                    match decl with
                    | Some decl -> [ decl ]
                    | _ -> []
                else
                    []

            Seq.append declEl referencingEls


        declsToFind |> Seq.collect (resolveDecl includeDecl)
