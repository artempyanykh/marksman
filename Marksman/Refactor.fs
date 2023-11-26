module Marksman.Refactor

open Ionide.LanguageServerProtocol.Types

open Marksman.Config
open Marksman.Misc
open Marksman.Cst
open Marksman.Names
open Marksman.Doc
open Marksman.Refs
open Marksman.Folder
open Marksman.Structure
open Marksman.Syms

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

let groupByFirst2 (triples: seq<'a * 'b * 'c>) : seq<'a * seq<'b * 'c>> =
    let grouped = Seq.groupBy (fun (k, _, _) -> k) triples
    let takeUnique23 triples = triples |> Seq.map (fun (_, b, c) -> b, c) |> Seq.distinct

    grouped |> Seq.map (fun (k, v) -> k, takeUnique23 v)

let mkWorkspaceEdit
    (supportsDocumentEdit: bool)
    (docEdits: array<TextDocumentEdit>)
    : WorkspaceEdit =

    // A little harmless mutation never hurt nobody
    // Sort edits: those that affect the end of the document go first.
    for docEdit in docEdits do
        docEdit.Edits |> Array.sortInPlaceWith (fun x y -> -(compare x y))

    let docChanges = docEdits |> Array.map DocumentChange.TextDocumentEdit

    if supportsDocumentEdit then
        { Changes = None; DocumentChanges = Some docChanges }
    else
        { Changes = Some(WorkspaceEdit.DocumentChangesToChanges docChanges)
          DocumentChanges = None }

let renameMarkdownLabel (newLabel: string) (element: Element) : option<TextEdit> =
    match element with
    | ML link ->
        MdLink.referenceLabel link.data
        |> Option.map (fun label -> { Range = label.range; NewText = newLabel })
    | MLD { data = def } -> Some { Range = (MdLinkDef.label def).range; NewText = newLabel }
    | _ -> None


let renameMarkdownLabelsInDoc newLabel (doc: Doc, els) =
    let edits =
        Seq.collect (renameMarkdownLabel newLabel >> Option.toArray) els
        |> Array.ofSeq

    let lspDoc = { Uri = (Doc.uri doc); Version = Doc.version doc }
    { TextDocument = lspDoc; Edits = edits }

let renameHeadingLink
    (complStyle: ComplWikiStyle)
    (srcDoc: Doc)
    (srcHeading: Element)
    (newTitle: string)
    (targetDoc: Doc)
    (targetEl: Element)
    : option<TextEdit> =
    let shouldRename =
        let _, srcId =
            srcDoc.Structure
            |> Structure.tryFindSymbolForConcrete srcHeading
            |> Option.bind Sym.asDef
            |> Option.bind Def.asHeader
            |> Option.defaultWith (fun () ->
                failwith $"Internal error: heading without a symbol: {srcHeading}")

        match targetDoc.Structure |> Structure.tryFindSymbolForConcrete targetEl with
        | Some (Sym.Ref (CrossRef (CrossDoc docName))) ->
            let linkKind = FileLinkKind.detect complStyle targetDoc.Id docName srcDoc
            linkKind = FileLinkKind.Title && Slug.equalStrings srcId docName
        | Some (Sym.Ref (CrossRef (CrossSection (docName, sectionName)))) ->
            if Element.isTitle srcHeading then
                let linkKind = FileLinkKind.detect complStyle targetDoc.Id docName srcDoc
                linkKind = FileLinkKind.Title && Slug.equalStrings srcId docName
            else
                Slug.ofString srcId = sectionName
        | Some (Sym.Ref (IntraRef (IntraSection sectionName))) -> Slug.ofString srcId = sectionName
        | _ -> false

    if shouldRename then
        match targetEl with
        | WL { data = wl } ->
            let toEdit = if Element.isTitle srcHeading then wl.doc else wl.heading

            toEdit
            |> Option.map (fun node ->
                { Range = node.range; NewText = WikiEncoded.encodeAsString newTitle })
        | ML { data = MdLink.IL (_, url, _) } ->
            let docUrl = url |> Option.map Url.ofUrlNode

            let toEdit =
                if not (Element.isTitle srcHeading) then
                    docUrl |> Option.bind Url.anchor
                else
                    None

            toEdit
            |> Option.map (fun node -> { Range = node.range; NewText = Slug.str newTitle })
        | _ -> None
    else
        None

let renameHeadingLinksInDoc
    (complStyle: ComplWikiStyle)
    (srcDoc: Doc)
    (srcHeading: Element)
    (newTitle: string)
    (targetDoc: Doc)
    (targetEls: seq<Element>)
    =
    let edits =
        Seq.collect
            (renameHeadingLink complStyle srcDoc srcHeading newTitle targetDoc
             >> Option.toArray)
            targetEls
        |> Array.ofSeq

    let lspDoc = { Uri = Doc.uri targetDoc; Version = Doc.version targetDoc }
    { TextDocument = lspDoc; Edits = edits }

let combineDocumentEdits (e1s: array<TextDocumentEdit>) (e2s: array<TextDocumentEdit>) =
    let deconstruct (x: TextDocumentEdit) = x.TextDocument, x.Edits

    let byDoc1 = e1s |> Seq.map deconstruct |> Map.ofSeq
    let byDoc2 = e2s |> Seq.map deconstruct |> Map.ofSeq

    let larger, smaller =
        if byDoc1.Count > byDoc1.Count then byDoc1, byDoc2 else byDoc2, byDoc1

    let mutable larger = larger

    for KeyValue (doc, additionalEdits) in smaller do
        let existing = larger |> Map.tryFind doc |> Option.defaultValue [||]
        let combined = Array.append existing additionalEdits
        larger <- Map.add doc combined larger

    larger
    |> Map.toSeq
    |> Seq.map (fun (doc, edits) -> { TextDocument = doc; Edits = edits })
    |> Array.ofSeq

let rename
    (supportsDocumentEdit: bool)
    (folder: Folder)
    (srcDoc: Doc)
    (pos: Position)
    (newName: string)
    : RenameResult =
    match Cst.elementAtPos pos (Doc.cst srcDoc) with
    | None -> Skip
    | Some (ML link as el) ->
        // Reference Links
        match MdLink.referenceLabel link.data with
        | None -> Skip
        | Some label ->
            if not (isValidLabel newName) then
                Error $"Not a valid label name: {newName}"
            else if label.range.ContainsInclusive pos then
                let refs = Dest.findElementRefs true folder srcDoc el
                // With reference link labels, there's no ambiguity about the destination, so we
                // can skip inspecting element's destination for the purposes of renaming.
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
        else if (MdLinkDef.label def).range.ContainsInclusive pos then
            let refs = Dest.findElementRefs true folder srcDoc el
            // Similar to the reference links above
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
            let headingEdit =
                let lspDoc = { Uri = Doc.uri srcDoc; Version = Doc.version srcDoc }
                let edit = { Range = heading.title.range; NewText = newName }
                { TextDocument = lspDoc; Edits = [| edit |] }

            let refs = Dest.findElementRefs false folder srcDoc el
            let byDoc = refs |> groupByFirst

            let complStyle = (Folder.configOrDefault folder).ComplWikiStyle()

            let linkEdits =
                byDoc
                |> Seq.map (fun (targetDoc, targetEls) ->
                    renameHeadingLinksInDoc complStyle srcDoc el newName targetDoc targetEls)
                |> Array.ofSeq

            let docEdits = combineDocumentEdits linkEdits [| headingEdit |]

            let workspaceEdit = mkWorkspaceEdit supportsDocumentEdit docEdits

            Edit workspaceEdit
        else
            Skip
    | _ -> Skip

let renameRange (srcDoc: Doc) (pos: Position) : option<Range> =
    match Cst.elementAtPos pos (Doc.cst srcDoc) with
    | None -> None
    | Some (ML link) ->
        match MdLink.referenceLabel link.data with
        | None -> None
        | Some label -> if label.range.ContainsInclusive pos then Some label.range else None
    | Some (MLD { data = def }) ->
        if (MdLinkDef.label def).range.ContainsInclusive pos then
            Some (MdLinkDef.label def).range
        else
            None
    | Some (H { data = heading }) ->
        if heading.title.range.ContainsInclusive pos then
            Some heading.title.range
        else
            None
    | _ -> None
