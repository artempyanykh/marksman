module Marksman.Diagnostics

module Lsp = Ionide.LanguageServerProtocol.Types

open Misc
open Parser
open Domain

type Diag =
    | DupTitle of orig: Heading * dup: Heading
    | DupHeading of orig: Heading * dup: Heading
    | BrokenDocRef of Ref
    | BrokenHeadingRef of Ref

let code: Diag -> string =
    function
    | DupTitle _ -> "1"
    | DupHeading _ -> "2"
    | BrokenDocRef _ -> "3"
    | BrokenHeadingRef _ -> "4"

type DocumentSummary =
    { titles: list<Heading>
      headings: Map<string, list<Heading>>
      refs: array<Ref> }

let summarizeDocument (doc: Document) : DocumentSummary =
    let mutable titles = []
    let mutable headings = Map.empty
    let refs = ResizeArray()

    for el in Document.elementsAll doc do
        match el with
        | H h ->
            let slug = (Heading.title h).Slug()

            let existing =
                Map.tryFind slug headings
                |> Option.defaultValue []

            headings <- Map.add slug (h :: existing) headings

            if Heading.isTitle h then
                titles <- h :: titles
        | R ref -> refs.Add(ref)
        | _ -> ()

    { titles = titles
      headings = headings
      refs = refs.ToArray() }

type FolderSummary = Map<DocName, DocumentSummary>

let summarizeFolder (folder: Folder) : FolderSummary =
    seq {
        for KeyValue (_, doc) in folder.documents do
            let docName = Folder.documentName doc folder
            let docSummary = summarizeDocument doc
            yield docName, docSummary
    }
    |> Map.ofSeq

let checkTitles (summary: DocumentSummary) : seq<Diag> =
    match summary.titles with
    | [] -> []
    | [ _ ] -> []
    | first :: rest ->
        rest
        |> List.map (fun dup -> DupTitle(orig = first, dup = dup))
        |> Seq.ofList

let checkHeadings (summary: DocumentSummary) : seq<Diag> =
    seq {
        for KeyValue (slug, hs) in summary.headings do
            match hs with
            | [] -> ()
            | [ _ ] -> ()
            | first :: rest ->
                for dup in rest do
                    yield DupHeading(orig = first, dup = dup)
    }

let checkRefs (folder: Folder) (summary: FolderSummary) : Map<PathUri, list<Diag>> =
    seq {
        for KeyValue (curDocName, curSummary) in summary do
            let curDoc =
                Folder.findDocumentByName curDocName folder

            let curDiag =
                seq {
                    for ref in curSummary.refs do
                        let destDocName =
                            Dest.destDoc ref.dest
                            |> Option.defaultValue curDocName

                        let destDoc =
                            Folder.tryFindDocumentByName destDocName folder

                        match destDoc with
                        | None -> yield BrokenDocRef(ref)
                        | Some destDoc ->
                            let destHeading = Dest.destHeading ref.dest

                            let destSummary =
                                Map.find destDocName summary

                            match destHeading with
                            | None -> ()
                            | Some destHeading ->
                                let destSlug = destHeading.Slug()

                                match Map.tryFind destSlug destSummary.headings with
                                | None -> yield BrokenHeadingRef(ref)
                                | Some _ -> ()
                }
                |> List.ofSeq

            yield curDoc.path, curDiag
    }
    |> Map.ofSeq

let checkFolder (folder: Folder) : seq<PathUri * list<Diag>> =
    let folderSummary = summarizeFolder folder

    seq {
        let folderRefDiag =
            checkRefs folder folderSummary

        for KeyValue (curDocName, curSummary) in folderSummary do
            let curDoc =
                Folder.findDocumentByName curDocName folder

            let curDiag =
                seq {
                    yield! checkTitles curSummary
                    yield! checkHeadings curSummary
                }
                |> List.ofSeq

            let totalDiag =
                match Map.tryFind curDoc.path folderRefDiag with
                | Some refDiag -> List.append curDiag refDiag
                | _ -> curDiag

            curDoc.path, totalDiag
    }

let diagToLsp (diag: Diag) : Lsp.Diagnostic =
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
