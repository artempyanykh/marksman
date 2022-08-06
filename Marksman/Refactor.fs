module Marksman.Refactor

open System
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

let isValidTitle (name: string) =
    let badChars = [ "\n"; "#" ]
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

let renameHeadingLink (heading: Heading) (newSlug: Slug) (element: Element) : option<TextEdit> =
    match element with
    | WL { data = wl } ->
        let toEdit = if Heading.isTitle heading then wl.doc else wl.heading

        toEdit
        |> Option.map (fun node -> { Range = node.range; NewText = Slug.toString newSlug })
    | ML { data = MdLink.IL (_, url, _) } ->
        let docUrl = url |> Option.map DocUrl.ofUrlNode

        let toEdit =
            if not (Heading.isTitle heading) then
                docUrl |> Option.bind DocUrl.anchor
            else
                None

        toEdit
        |> Option.map (fun node -> { Range = node.range; NewText = Slug.toString newSlug })
    | _ -> None

let renameHeadingLinksInDoc heading newTitle (doc: Doc, els) =
    let newSlug = Slug.ofString newTitle

    let edits =
        Seq.collect (renameHeadingLink heading newSlug >> Option.toArray) els
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
    | Some (H { data = heading } as el) ->
        if not (isValidLabel newName) then
            Error $"Not a valid title: {newName}"
        else if heading.title.range.ContainsInclusive pos then
            let headingEdit = { Range = heading.title.range; NewText = newName }

            let refs = Ref.findElementRefs false folder srcDoc el
            let byDoc = refs |> groupByFirst

            let docEdits =
                byDoc
                |> Seq.map (fun (doc, els) ->
                    let linkEdits = renameHeadingLinksInDoc heading newName (doc, els)

                    if doc = srcDoc then
                        { linkEdits with Edits = Array.append [| headingEdit |] linkEdits.Edits }
                    else
                        linkEdits)
                |> Array.ofSeq

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
    | Some (H { data = heading }) ->
        if heading.title.range.ContainsInclusive pos then
            Some heading.title.range
        else
            None
    | _ -> None
