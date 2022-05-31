module Marksman.Diag

module Lsp = Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Parser
open Marksman.Workspace
open Marksman.DB

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

let checkTitles (db: DocDB) : seq<Entry> =
    match DocDB.titles db with
    | [] -> []
    | [ _ ] -> []
    | first :: rest ->
        rest
        |> List.map (fun dup -> DupTitle(orig = first, dup = dup))
        |> Seq.ofList

let checkHeadings (db: DocDB) : seq<Entry> =
    seq {
        for KeyValue (_, hs) in DocDB.headingsBySlug db do
            match hs with
            | [] -> ()
            | [ _ ] -> ()
            | first :: rest ->
                for dup in rest do
                    yield DupHeading(orig = first, dup = dup)
    }

let checkWikiLinks (docSlug: Slug) (fdb: FolderDB) : seq<Entry> =
    let docDB = FolderDB.findDocBySlug docSlug fdb

    seq {
        for wl in DocDB.wikiLinks docDB do
            let destDocSlug =
                WikiLink.destDoc wl.data
                |> Option.map Slug.ofString
                |> Option.defaultValue docSlug

            let destDocDB = FolderDB.tryFindDocBySlug destDocSlug fdb

            match destDocDB with
            | None -> yield BrokenDocWL(wl)
            | Some destDocDB ->
                match WikiLink.destHeading wl.data with
                | None -> ()
                | Some destHeading ->
                    let destSlug = Slug.ofString destHeading

                    match DocDB.tryFindHeadingBySlug destSlug destDocDB with
                    | None -> yield BrokenHeadingWL(wl)
                    | Some _ -> ()
    }

let checkFolder (folder: Folder) : seq<PathUri * list<Entry>> =
    let folderDB = FolderDB.ofFolder folder

    seq {
        for docSlug, docDB in FolderDB.docsBySlug folderDB do
            let docDiag =
                seq {
                    yield! checkTitles docDB
                    yield! checkHeadings docDB
                    yield! checkWikiLinks docSlug folderDB
                }
                |> List.ofSeq

            DocDB.path docDB, docDiag
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
