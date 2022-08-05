module Marksman.Refactor

open Ionide.LanguageServerProtocol.Types
open Marksman.Workspace
open Cst
open Misc
open Refs

type RenameResult =
    | Edit of WorkspaceEdit
    | Error of string
    | Skip

module RenameResult =
    let toLsp =
        function
        | Edit edit -> LspResult.success (Some edit)
        | Error msg -> LspResult.invalidParams msg
        | Skip -> LspResult.success None

let isValidLabel (name: string) =
    let badChars = [ "\n"; "["; "]"; "("; ")" ]
    badChars |> List.exists name.Contains |> not


let groupByFirst pairs =
    Seq.groupBy fst pairs
    |> Seq.map (fun (key, value) -> key, Set.ofSeq (Seq.map snd value) |> Set.toSeq)

let mkWorkspaceEdit (supportsDocumentEdit: bool) (edits: array<TextDocumentEdit>) : WorkspaceEdit =
    if supportsDocumentEdit then
        { Changes = None; DocumentChanges = Some edits }
    else
        { Changes = Some(WorkspaceEdit.DocumentChangesToChanges edits)
          DocumentChanges = None }

let renameMarkdownLabel (newLabel: string) (element: Element) : option<TextEdit> =
    match element with
    | ML link ->
        MdLink.referenceLabel link.data
        |> Option.map (fun label -> { Range = label.range; NewText = newLabel })
    | MLD { data = def } -> Some { Range = def.label.range; NewText = newLabel }
    | _ -> None


let renameMarkdownLabelsInDoc newLabel (doc: Doc, els) =
    let edits =
        Seq.collect (renameMarkdownLabel newLabel >> Option.toArray) els
        |> Array.ofSeq

    let lspDoc = { Uri = doc.path.DocumentUri; Version = doc.version }
    { TextDocument = lspDoc; Edits = edits }

let rename
    (supportsDocumentEdit: bool)
    (folder: Folder)
    (srcDoc: Doc)
    (pos: Position)
    (newName: string)
    : RenameResult =
    match Cst.elementAtPos pos srcDoc.cst with
    | None -> Skip
    | Some (ML link as el) ->
        match MdLink.referenceLabel link.data with
        | None -> Skip
        | Some label ->
            if not (isValidLabel newName) then
                Error $"Not a valid label name: {newName}"
            else if label.range.ContainsInclusive pos then
                let refs = Ref.findElementRefs true folder srcDoc el
                let byDoc = refs |> groupByFirst

                let docEdits =
                    byDoc |> Seq.map (renameMarkdownLabelsInDoc newName) |> Array.ofSeq

                let workspaceEdit = mkWorkspaceEdit supportsDocumentEdit docEdits
                Edit workspaceEdit
            else
                Skip
    | Some (MLD { data = def } as el) ->
        if not (isValidLabel newName) then
            Error $"Not a valid label name: {newName}"
        else if def.label.range.ContainsInclusive pos then
            let refs = Ref.findElementRefs true folder srcDoc el
            let byDoc = refs |> groupByFirst

            let docEdits =
                byDoc |> Seq.map (renameMarkdownLabelsInDoc newName) |> Array.ofSeq

            let workspaceEdit = mkWorkspaceEdit supportsDocumentEdit docEdits
            Edit workspaceEdit
        else
            Skip
    | _ -> Skip

let renameRange (srcDoc: Doc) (pos: Position) : option<Range> =
    match Cst.elementAtPos pos srcDoc.cst with
    | None -> None
    | Some (ML link) ->
        match MdLink.referenceLabel link.data with
        | None -> None
        | Some label -> if label.range.ContainsInclusive pos then Some label.range else None
    | Some (MLD { data = def }) ->
        if def.label.range.ContainsInclusive pos then
            Some def.label.range
        else
            None
    | _ -> None
