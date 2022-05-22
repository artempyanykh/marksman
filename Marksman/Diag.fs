module Marksman.Diag

open System.Collections.Generic

module Lsp = Ionide.LanguageServerProtocol.Types

open Misc
open Parser
open Domain

type Entry =
    | DupTitle of orig: Node<Heading> * dup: Node<Heading>
    | DupHeading of orig: Node<Heading> * dup: Node<Heading>
    | BrokenDocWL of Node<WikiLink>
    | BrokenHeadingWL of Node<WikiLink>

let code: Entry -> string =
    function
    | DupTitle _ -> "1"
    | DupHeading _ -> "2"
    | BrokenDocWL _ -> "3"
    | BrokenHeadingWL _ -> "4"

type DocumentIndex =
    { path: PathUri
      titles: list<Node<Heading>>
      headings: Map<Slug, list<Node<Heading>>>
      wikiLinks: array<Node<WikiLink>> }

let indexDocument (doc: Doc) : DocumentIndex =
    let titles = ResizeArray()

    let headings = Dictionary<Slug, ResizeArray<Node<Heading>>>()

    let wikiLinks = ResizeArray()

    for el in Doc.elementsAll doc do
        match el with
        | H hn ->
            let slug = Heading.slug hn.data

            if not (headings.ContainsKey(slug)) then
                headings[slug] <- ResizeArray()

            headings.[slug].Add(hn)

            if Heading.isTitle hn.data then titles.Add(hn)
        | WL ref -> wikiLinks.Add(ref)
        | _ -> ()

    let titles = titles |> List.ofSeq

    let headings =
        seq {
            for KeyValue (slug, headings) in headings do
                yield slug, headings |> List.ofSeq
        }
        |> Map.ofSeq

    let wikiLinks = wikiLinks.ToArray()

    { path = doc.path
      titles = titles
      headings = headings
      wikiLinks = wikiLinks }

type FolderIndex = Map<Slug, DocumentIndex>

let indexFolder (folder: Folder) : FolderIndex =
    seq {
        for KeyValue (_, doc) in folder.documents do
            let docSummary = indexDocument doc
            yield Doc.name doc |> Slug.ofString, docSummary
    }
    |> Map.ofSeq

let checkTitles (summary: DocumentIndex) : seq<Entry> =
    match summary.titles with
    | [] -> []
    | [ _ ] -> []
    | first :: rest ->
        rest
        |> List.map (fun dup -> DupTitle(orig = first, dup = dup))
        |> Seq.ofList

let checkHeadings (index: DocumentIndex) : seq<Entry> =
    seq {
        for KeyValue (_, hs) in index.headings do
            match hs with
            | [] -> ()
            | [ _ ] -> ()
            | first :: rest ->
                for dup in rest do
                    yield DupHeading(orig = first, dup = dup)
    }

let checkWikiLinks (docSlug: Slug) (index: FolderIndex) : seq<Entry> =
    let docIndex = Map.find docSlug index

    seq {
        for wl in docIndex.wikiLinks do
            let destDocSlug =
                WikiLink.destDoc wl.data
                |> Option.map Slug.ofString
                |> Option.defaultValue docSlug

            let destDocIndex = Map.tryFind destDocSlug index

            match destDocIndex with
            | None -> yield BrokenDocWL(wl)
            | Some destDocIndex ->
                match WikiLink.destHeading wl.data with
                | None -> ()
                | Some destHeading ->
                    let destSlug = Slug.ofString destHeading

                    match Map.tryFind destSlug destDocIndex.headings with
                    | None -> yield BrokenHeadingWL(wl)
                    | Some _ -> ()
    }

let checkFolder (folder: Folder) : seq<PathUri * list<Entry>> =
    let folderIndex = indexFolder folder

    seq {
        for KeyValue (docSlug, docIndex) in folderIndex do
            let docDiag =
                seq {
                    yield! checkTitles docIndex
                    yield! checkHeadings docIndex
                    yield! checkWikiLinks docSlug folderIndex
                }
                |> List.ofSeq

            docIndex.path, docDiag
    }

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


let diagnosticForFolder (folder: Folder) : array<PathUri * array<Lsp.Diagnostic>> =
    checkFolder folder
    |> Seq.map (fun (uri, diags) ->
        let lspDiags = List.map diagToLsp diags |> Array.ofList

        uri, lspDiags)
    |> Array.ofSeq

let diagnosticForWorkspace
    (workspace: Workspace)
    : Map<PathUri, array<PathUri * array<Lsp.Diagnostic>>> =
    seq {
        for f in Workspace.folders workspace do
            yield f.root, diagnosticForFolder f
    }
    |> Map.ofSeq
