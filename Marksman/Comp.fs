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
                let range =
                    Range.Mk(elementRange.Start.NextChar(2), elementRange.End.PrevChar(2))

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
                    Some(Comp.Heading(Some(destDoc), hd.range))
                else
                    None
            | None, Some hd ->
                if hd.range.ContainsInclusive(pos) then
                    Some(Comp.Heading(None, hd.range))
                else
                    None
        | _ -> None

let findCompAtPoint (pos: Position) (doc: Document) : option<Comp> =
    Document.elementsAll doc
    |> Seq.map (compOfElement pos)
    |> Seq.tryFind Option.isSome
    |> Option.flatten

let findCandidatesInDoc (comp: Comp) (srcDoc: Document) (folder: Folder) : array<CompletionItem> =
    match comp with
    | Comp.Title range ->
        let input = srcDoc.text.Substring(range)
        let inputSlug = Slug.ofString input

        let titleWhenMatch (doc: Document) =
            match Document.title doc with
            | Some { data = heading } ->
                let title = (Heading.name heading)
                let titleSlug = Slug.ofString title

                if Slug.isSubSequence inputSlug titleSlug then [ doc, title ] else []
            | None -> []

        let matchingTitles =
            folder.documents |> Map.values |> Seq.collect titleWhenMatch

        let toCompletionItem (doc: Document, title: string) =
            let textEdit = { Range = range; NewText = Slug.str title }

            { CompletionItem.Create(title) with
                Detail = Some doc.relPath
                TextEdit = Some textEdit
                FilterText = Some title }

        Seq.map toCompletionItem matchingTitles |> Array.ofSeq
    | Comp.Heading (destDocTitle, range) ->
        let destDoc =
            match destDocTitle with
            | None -> Some srcDoc
            | Some destDocTitle ->
                let destDocSlug = Slug.ofString destDocTitle

                let matchTitle (doc: Document) =
                    let docSlug = Slug.ofString (Document.name doc)
                    docSlug = destDocSlug

                folder.documents |> Map.values |> Seq.tryFind matchTitle

        match destDoc with
        | None -> [||]
        | Some destDoc ->
            let input = srcDoc.text.Substring(range)
            let inputSlug = Slug.ofString input

            let matchingHeadings =
                Document.headings destDoc
                |> Seq.filter (fun { data = h } -> h.level <> 1)
                |> Seq.map (fun { data = h } -> Heading.name h)
                |> Seq.filter (fun h -> (Slug.ofString h) |> Slug.isSubSequence inputSlug)

            let toCompletionItem hd =
                let textEdit = { Range = range; NewText = Slug.str hd }

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
        | None ->
            logger.trace (Log.setMessage "No completion point found")

            [||]
        | Some comp ->
            logger.trace (Log.setMessage "Found completion point" >> Log.addContext "comp" comp)

            findCandidatesInDoc comp doc folder
