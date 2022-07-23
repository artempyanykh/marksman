module Marksman.Diag

open Marksman.Workspace

module Lsp = Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Cst
open Marksman.Index

type Entry =
    | DupTitle of orig: Node<Heading> * dup: Node<Heading>
    | DupHeading of orig: Node<Heading> * dup: Node<Heading>
    | BrokenDocWL of Node<WikiLink>
    | BrokenHeadingWL of Node<WikiLink>
    | NonBreakableWhitespace of Lsp.Range

let code: Entry -> string =
    function
    | DupTitle _ -> "1"
    | DupHeading _ -> "2"
    | BrokenDocWL _ -> "3"
    | BrokenHeadingWL _ -> "4"
    | NonBreakableWhitespace _ -> "5"

let checkTitles (index: Index) : seq<Entry> =
    match Index.titles index |> List.ofArray with
    | [] -> []
    | [ _ ] -> []
    | first :: rest ->
        rest
        |> List.map (fun dup -> DupTitle(orig = first, dup = dup))
        |> Seq.ofList

let checkNonBreakingWhitespace (doc: Doc) =
    let nonBreakingWhitespace = "\u00a0"

    let headings = [ 1..7 ] |> List.map (fun n -> (String.replicate n "#"))

    [ 0 .. doc.text.lineMap.NumLines ]
    |> List.collect (fun x ->
        let line = doc.text.LineContent x

        let headingLike =
            List.tryFind (fun (h: string) -> line.StartsWith(h + nonBreakingWhitespace)) headings

        match headingLike with
        | None -> []
        | Some (heading) ->
            let whitespaceRange: Lsp.Range =
                { Start = { Line = x; Character = heading.Length }
                  End = { Line = x; Character = heading.Length + 1 } }

            [ NonBreakableWhitespace(whitespaceRange) ])


let checkHeadings (db: Index) : seq<Entry> =
    seq {
        for KeyValue (_, hs) in Index.headingsBySlug db do
            match hs with
            | [] -> ()
            | [ _ ] -> ()
            | first :: rest ->
                for dup in rest do
                    yield DupHeading(orig = first, dup = dup)
    }

let checkWikiLinks (doc: Doc) (folder: Folder) : seq<Entry> =
    let docSlug = Doc.slug doc

    seq {
        for wl in Index.wikiLinks doc.index do
            let destDocSlug =
                WikiLink.destDoc wl.data
                |> Option.map Slug.ofString
                |> Option.defaultValue docSlug

            let destDoc = Folder.tryFindDocBySlug destDocSlug folder

            match destDoc with
            | None -> yield BrokenDocWL(wl)
            | Some destDoc ->
                match WikiLink.destHeading wl.data with
                | None -> ()
                | Some destHeading ->
                    let destSlug = Slug.ofString destHeading

                    match Index.tryFindHeadingBySlug destSlug destDoc.index with
                    | None -> yield BrokenHeadingWL(wl)
                    | Some _ -> ()
    }

let checkFolder (folder: Folder) : seq<PathUri * list<Entry>> =
    seq {
        for doc in Folder.docs folder do
            let docDiag =
                seq {
                    yield! checkTitles doc.index
                    yield! checkHeadings doc.index
                    yield! checkWikiLinks doc folder
                    yield! checkNonBreakingWhitespace doc
                }
                |> List.ofSeq

            doc.path, docDiag
    }

let clippy msg =
    $"""
{msg}

/  \
|  |
@  @
|  |
|| |/
|| ||
|\_/|
\___/
"""



let diagToLsp (diag: Entry) : Lsp.Diagnostic =
    match diag with
    | DupTitle (_orig, dup) ->
        { Range = dup.range
          Severity = Some Lsp.DiagnosticSeverity.Error
          Code = Some(code diag)
          CodeDescription = None
          Source = "Marksman"
          Message = "Duplicate title"
          RelatedInformation = None
          Tags = None
          Data = None }
    | DupHeading (_orig, dup) ->
        { Range = dup.range
          Severity = Some Lsp.DiagnosticSeverity.Error
          Code = Some(code diag)
          CodeDescription = None
          Source = "Marksman"
          Message = "Heading with the same slug already exists"
          RelatedInformation = None
          Tags = None
          Data = None }
    | NonBreakableWhitespace (dup) ->
        { Range = dup
          Severity = Some Lsp.DiagnosticSeverity.Warning
          Code = Some(code diag)
          CodeDescription = None
          Source = "Marksman"
          Message =
            clippy
                "Non-breaking whitespace used instead of regular whitespace - this line won't be interpreted as a heading"
          RelatedInformation = None
          Tags = None
          Data = None }
    | BrokenDocWL wl ->
        { Range = wl.range
          Severity = Some Lsp.DiagnosticSeverity.Error
          Code = Some(code diag)
          CodeDescription = None
          Source = "Marksman"
          Message = $"Reference to non-existent document: {WikiLink.destDoc wl.data |> Option.get}"
          RelatedInformation = None
          Tags = None
          Data = None }
    | BrokenHeadingWL wl ->
        { Range = wl.range
          Severity = Some Lsp.DiagnosticSeverity.Error
          Code = Some(code diag)
          CodeDescription = None
          Source = "Marksman"
          Message =
            $"Reference to non-existent heading: {WikiLink.destHeading wl.data |> Option.get}"
          RelatedInformation = None
          Tags = None
          Data = None }

type FolderDiag = array<PathUri * array<Lsp.Diagnostic>>

module FolderDiag =
    let mk (folder: Folder) : FolderDiag =
        checkFolder folder
        |> Seq.map (fun (uri, diags) ->
            let lspDiags = List.map diagToLsp diags |> Array.ofList

            uri, lspDiags)
        |> Array.ofSeq

type WorkspaceDiag = Map<PathUri, FolderDiag>

module WorkspaceDiag =
    let mk (ws: Workspace) : WorkspaceDiag =
        Workspace.folders ws
        |> Seq.map (fun folder -> folder.root, FolderDiag.mk folder)
        |> Map.ofSeq
