module Marksman.Diag

open System.Collections.Generic

module Lsp = Ionide.LanguageServerProtocol.Types

open Misc
open Parser
open Domain

type Entry =
    | DupTitle of orig: Heading * dup: Heading
    | DupHeading of orig: Heading * dup: Heading
    | BrokenDocRef of Ref
    | BrokenHeadingRef of Ref

let code: Entry -> string =
    function
    | DupTitle _ -> "1"
    | DupHeading _ -> "2"
    | BrokenDocRef _ -> "3"
    | BrokenHeadingRef _ -> "4"

type DocumentIndex =
    { path: PathUri
      titles: list<Heading>
      headings: Map<string, list<Heading>>
      refs: array<Ref> }

let indexDocument (doc: Document) : DocumentIndex =
    let titles = ResizeArray()

    let headings =
        Dictionary<string, ResizeArray<Heading>>()

    let refs = ResizeArray()

    for el in Document.elementsAll doc do
        match el with
        | H h ->
            let slug = (Heading.title h).Slug()

            if not (headings.ContainsKey(slug)) then
                headings[slug] <- ResizeArray()

            headings[ slug ].Add(h)

            if Heading.isTitle h then titles.Add(h)
        | R ref -> refs.Add(ref)
        | _ -> ()

    let titles = titles |> List.ofSeq

    let headings =
        seq {
            for KeyValue (slug, headings) in headings do
                yield slug, headings |> List.ofSeq
        }
        |> Map.ofSeq

    let refs = refs.ToArray()

    { path = doc.path
      titles = titles
      headings = headings
      refs = refs }

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

let checkRefs (docName: DocName) (index: FolderIndex) : seq<Entry> =
    let docIndex = Map.find docName index

    seq {
        for ref in docIndex.refs do
            let destDocName =
                Dest.destDoc ref.dest
                |> Option.defaultValue docName

            let destDocIndex =
                Map.tryFind destDocName index

            match destDocIndex with
            | None -> yield BrokenDocRef(ref)
            | Some destDocIndex ->
                match Dest.destHeading ref.dest with
                | None -> ()
                | Some destHeading ->
                    let destSlug = destHeading.Slug()

                    match Map.tryFind destSlug destDocIndex.headings with
                    | None -> yield BrokenHeadingRef(ref)
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
    | BrokenDocRef ref ->
        { Range = ref.range
          Severity = Some Lsp.DiagnosticSeverity.Error
          Code = Some(code diag)
          CodeDescription = None
          Source = "Marksman"
          Message = $"Reference to non-existent document: {Dest.destDoc ref.dest |> Option.get}"
          RelatedInformation = None
          Tags = None
          Data = None }
    | BrokenHeadingRef ref ->
        { Range = ref.range
          Severity = Some Lsp.DiagnosticSeverity.Error
          Code = Some(code diag)
          CodeDescription = None
          Source = "Marksman"
          Message = $"Reference to non-existent heading: {Dest.destHeading ref.dest |> Option.get}"
          RelatedInformation = None
          Tags = None
          Data = None }


let diagnosticForFolder (folder: Folder) : array<PathUri * array<Lsp.Diagnostic>> =
    checkFolder folder
    |> Seq.map (fun (uri, diags) ->
        let lspDiags =
            List.map diagToLsp diags |> Array.ofList

        uri, lspDiags)
    |> Array.ofSeq

let diagnosticForWorkspace (workspace: Workspace) : Map<PathUri, array<PathUri * array<Lsp.Diagnostic>>> =
    seq {
        for f in Workspace.folders workspace do
            yield f.root, diagnosticForFolder f
    }
    |> Map.ofSeq
