module Marksman.Connections

open FSharpPlus.Operators

open Marksman.Misc
open Marksman.Ast
open Marksman.Names
open Marksman.Doc
open Marksman.Workspace
open Marksman.Index

let private filterDocsByName (folder: Folder) (name: InternName) : DocId[] =
    let byTitle: seq<DocId> =
        Folder.filterDocsBySlug (InternName.name name |> Slug.ofString) folder
        |> Seq.map Doc.id

    let byPath: seq<DocId> =
        InternName.tryAsPath name
        |> Option.map (fun path -> Folder.filterDocsByInternPath path folder |> Seq.map Doc.id)
        |> Option.defaultValue []

    Set.ofSeq (Seq.append byTitle byPath) |> Set.toArray

[<RequireQualifiedAccess>]
type Src =
    | WL of WikiLink
    | ML of MdLink
    | MR of MdRef

[<RequireQualifiedAccess>]
type Dest =
    | Doc
    | Head of Heading
    | LinkDef of MdLinkDef

module Src =
    let private findDestDocs (folder: Folder) (srcDoc: DocId) (src: Src) =
        match src with
        | Src.WL { doc = None }
        | Src.ML { url = None }
        | Src.MR _ -> [| srcDoc |]
        | Src.WL { doc = Some doc } ->
            let name = InternName.mkUnchecked srcDoc doc
            filterDocsByName folder name
        | Src.ML { url = Some url } ->
            let exts = (Folder.configOrDefault folder).CoreMarkdownFileExtensions()

            match InternName.mkChecked exts srcDoc url with
            | Some name -> filterDocsByName folder name
            | None -> [||]

    let private findDestInDoc (folder: Folder) (src: Src) (destDoc: DocId) : seq<Dest> =
        let destDoc = Folder.findDocById destDoc folder
        let destIndex = Doc.index destDoc

        match src with
        | Src.WL { heading = None }
        | Src.ML { anchor = None } -> [ Dest.Doc ]
        | Src.WL { heading = Some heading }
        | Src.ML { anchor = Some heading } ->
            Index.filterHeadingBySlug (Slug.ofString heading) destIndex
            |> Seq.ofList
            |> Seq.map (fun x -> Heading.OfCst x.data |> Dest.Head)
        | Src.MR mdRef ->
            match
                Index.tryFindLinkDef mdRef.DestLabel destIndex
                |> Option.map (fun x -> MdLinkDef.OfCst x.data)
            with
            | Some linkDef -> [ Dest.LinkDef linkDef ]
            | None -> []

    let findDest (folder: Folder) (srcDoc: DocId) (src: Src) : array<DocId * Dest> =
        let destDocs = findDestDocs folder srcDoc src

        seq {
            for destDoc in destDocs do
                yield! findDestInDoc folder src destDoc |> Seq.map (fun x -> destDoc, x)
        }
        |> Array.ofSeq
