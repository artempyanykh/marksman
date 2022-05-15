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
      headings: Map<string, list<Node<Heading>>>
      wikiLinks: array<Node<WikiLink>> }

let indexDocument (doc: Document) : DocumentIndex =
    let titles = ResizeArray()

    let headings = Dictionary<string, ResizeArray<Node<Heading>>>()

    let refs = ResizeArray()

    for el in Document.elementsAll doc do
        match el with
        | H h ->
            let slug = (Heading.title h.data).Slug()

            if not (headings.ContainsKey(slug)) then
                headings[slug] <- ResizeArray()

            headings.[slug].Add(h)

            if Heading.isTitle h.data then titles.Add(h)
        | WL ref -> refs.Add(ref)
        | _ -> ()

    let titles = titles |> List.ofSeq

    let headings =
        seq {
            for KeyValue (slug, headings) in headings do
                yield slug, headings |> List.ofSeq
        }
        |> Map.ofSeq

    let refs = refs.ToArray()

    { path = doc.path; titles = titles; headings = headings; wikiLinks = refs }

type FolderIndex = Map<DocName, DocumentIndex>

let indexFolder (folder: Folder) : FolderIndex =
    seq {
        for KeyValue (_, doc) in folder.documents do
            let docSummary = indexDocument doc
            yield doc.name, docSummary
    }
    |> Map.ofSeq

let checkTitles (summary: DocumentIndex) : seq<Entry> =
    match summary.titles with
    | [] -> []
    | [ _ ] -> []
    | first :: rest -> rest |> List.map (fun dup -> DupTitle(orig = first, dup = dup)) |> Seq.ofList

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

let checkRefs (docName: DocName) (index: FolderIndex) : seq<Entry> =
    let docIndex = Map.find docName index

    seq {
        for wl in docIndex.wikiLinks do
            let destDocName = WikiLink.destDoc wl.data |> Option.defaultValue docName

            let destDocIndex = Map.tryFind destDocName index

            match destDocIndex with
            | None -> yield BrokenDocWL(wl)
            | Some destDocIndex ->
                match WikiLink.destHeading wl.data with
                | None -> ()
                | Some destHeading ->
                    let destSlug = destHeading.Slug()

                    match Map.tryFind destSlug destDocIndex.headings with
                    | None -> yield BrokenHeadingWL(wl)
                    | Some _ -> ()
    }

let checkFolder (folder: Folder) : seq<PathUri * list<Entry>> =
    let folderIndex = indexFolder folder

    seq {
        for KeyValue (docName, docIndex) in folderIndex do
            let docDiag =
                seq {
                    yield! checkTitles docIndex
                    yield! checkHeadings docIndex
                    yield! checkRefs docName folderIndex
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
          Message = $"Reference to non-existent heading: {WikiLink.destHeading wl.data |> Option.get}"
          RelatedInformation = None
          Tags = None
          Data = None }


let diagnosticForFolder (folder: Folder) : array<PathUri * array<Lsp.Diagnostic>> =
    checkFolder folder
    |> Seq.map (fun (uri, diags) ->
        let lspDiags = List.map diagToLsp diags |> Array.ofList

        uri, lspDiags)
    |> Array.ofSeq

let diagnosticForWorkspace (workspace: Workspace) : Map<PathUri, array<PathUri * array<Lsp.Diagnostic>>> =
    seq {
        for f in Workspace.folders workspace do
            yield f.root, diagnosticForFolder f
    }
    |> Map.ofSeq
