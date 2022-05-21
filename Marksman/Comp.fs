module Marksman.Comp

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types
open Marksman.Parser
open Marksman.Domain
open Marksman.Misc

let private logger = LogProvider.getLoggerByName "Comp"

[<RequireQualifiedAccess>]
type Comp =
    | Title of range: Range
    | Heading of destDoc: option<string> * range: Range

    override this.ToString() =
        match this with
        | Comp.Title range -> $"Title: range={range.DebuggerDisplay}"
        | Comp.Heading (destDoc, range) -> $"Heading: dest={destDoc}; range={range.DebuggerDisplay}"

let compOfElement (pos: Position) (el: Element) : option<Comp> =
    let elementRange = Element.range el

    if not (elementRange.ContainsInclusive(pos)) then
        None
    else
        match el with
        | WL wl ->
            match wl.data.doc, wl.data.heading with
            | None, None ->
                let range = Range.Mk(elementRange.Start.NextChar(2), elementRange.End.PrevChar(2))
                Some(Comp.Title range)
            | Some doc, None ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Comp.Title doc.range)
                else
                    None
            | Some doc, Some hd ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Comp.Title doc.range)
                else if hd.range.ContainsInclusive(pos) then
                    let destDoc = doc.text
                    Some(Comp.Heading(Some destDoc, hd.range))
                else
                    None
            | None, Some hd ->
                if hd.range.ContainsInclusive(pos) then
                    Some(Comp.Heading(None, hd.range))
                else
                    None
        | _ -> None

let findCompAtPoint (pos: Position) (doc: Document) : option<Comp> =
    let comp =
        Document.elementsAll doc
        |> Seq.map (compOfElement pos)
        |> Seq.tryFind Option.isSome
        |> Option.flatten

    logger.trace (Log.setMessage "Processed completion points" >> Log.addContext "comp" comp)
    comp


let findCandidatesInDoc (comp: Comp) (srcDoc: Document) (folder: Folder) : array<CompletionItem> =
    match comp with
    | Comp.Title range ->
        let input = srcDoc.text.Substring(range)

        let titleWhenMatch (doc: Document) =
            let titleText =
                Document.title doc |> Option.map (fun t -> Heading.title t.data) |> Option.defaultValue doc.name

            if input.IsSubSequenceOf(titleText) then [ titleText ] else []

        let matchingTitles = folder.documents |> Map.values |> Seq.collect titleWhenMatch

        let toCompletionItem title =
            let textEdit = { Range = range; NewText = title }

            { CompletionItem.Create(title) with
                Detail = None
                TextEdit = Some textEdit
                FilterText = Some title }

        Seq.map toCompletionItem matchingTitles |> Array.ofSeq
    | Comp.Heading (destDocTag, range) ->
        let destDoc =
            match destDocTag with
            | None -> Some srcDoc
            | Some destDocTag ->
                let matchTag (doc: Document) =
                    let tag =
                        Document.title doc |> Option.map (fun h -> h.data.title.text) |> Option.defaultValue doc.name

                    tag = destDocTag

                folder.documents |> Map.values |> Seq.tryFind matchTag

        match destDoc with
        | None -> [||]
        | Some destDoc ->
            let input = srcDoc.text.Substring(range)

            let matchingHeadings =
                Document.headings destDoc
                |> Seq.map (fun n -> Heading.title n.data)
                |> Seq.filter input.IsSubSequenceOf

            let toCompletionItem hd =
                let textEdit = { Range = range; NewText = hd }

                { CompletionItem.Create(hd) with
                    Detail = None
                    TextEdit = Some textEdit
                    FilterText = Some hd }

            matchingHeadings |> Seq.map toCompletionItem |> Array.ofSeq

let findCandidates (pos: Position) (docUri: PathUri) (folder: Folder) : array<CompletionItem> =
    let doc = Folder.tryFindDocument docUri folder

    match doc with
    | None -> [||]
    | Some doc ->
        match findCompAtPoint pos doc with
        | None -> [||]
        | Some comp -> findCandidatesInDoc comp doc folder
