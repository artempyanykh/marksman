module Marksman.Comp

open System

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types

open Marksman.Parser
open Marksman.Domain
open Marksman.Misc
open Marksman.Text

let private logger = LogProvider.getLoggerByName "Comp"

[<RequireQualifiedAccess>]
type Comp =
    | Title of range: Range * needsClosing: bool
    | Heading of destDoc: option<string> * range: Range
    | LinkReference of range: Range * needsClosing: bool

    override this.ToString() =
        match this with
        | Comp.Title (range, needsClosing) ->
            $"Title: range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Comp.Heading (destDoc, range) -> $"Heading: dest={destDoc}; range={range.DebuggerDisplay}"
        | Comp.LinkReference (range, needsClosing) ->
            $"LinkReference: range={range.DebuggerDisplay}; needsClosing={needsClosing}"

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

                Some(Comp.Title(range, false))
            | Some doc, None ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Comp.Title(doc.range, false))
                else
                    None
            | Some doc, Some hd ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Comp.Title(doc.range, false))
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

let matchBracketParaElement (pos: Position) (line: Line) : option<Comp> =
    let lineRange = Line.range line

    let scanCursor =
        match line |> Line.toCursorAt pos with
        | Some cursor -> Cursor.backward cursor
        | None -> if Line.endsAt pos line then Line.endCursor line else None

    let findOpenOrWs =
        Cursor.tryFindCharMatching Cursor.backward (fun c -> Char.IsWhiteSpace(c) || c = '[')

    let paraElStart = scanCursor |> Option.bind findOpenOrWs

    let findEndWs =
        Cursor.tryFindCharMatching Cursor.forward Char.IsWhiteSpace

    let rangeEnd =
        scanCursor
        |> Option.bind findEndWs
        |> Option.map Cursor.pos
        |> Option.defaultValue lineRange.End

    match paraElStart with
    | Some start ->
        match Cursor.backwardChar2 start with
        | Some ('[', '[') ->
            let rangeStart = (Cursor.pos start).NextChar(1)
            Some(Comp.Title(Range.Mk(rangeStart, rangeEnd), true))
        | _ ->
            if Cursor.char start = '[' then
                let rangeStart = (Cursor.pos start).NextChar(1)
                Some(Comp.LinkReference(Range.Mk(rangeStart, rangeEnd), true))
            else
                None
    | _ -> None

let compOfText (pos: Position) (text: Text) : option<Comp> =
    Line.ofPos text pos |> Option.bind (matchBracketParaElement pos)

let findCompAtPoint (pos: Position) (doc: Document) : option<Comp> =
    let elComp =
        Document.elementsAll doc
        |> Seq.map (compOfElement pos)
        |> Seq.tryFind Option.isSome
        |> Option.flatten

    match elComp with
    | Some _ -> elComp
    | None -> compOfText pos doc.text

let findCandidatesInDoc (comp: Comp) (srcDoc: Document) (folder: Folder) : array<CompletionItem> =
    match comp with
    | Comp.Title (range, needsClosing) ->
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
            let newText =
                if needsClosing then Slug.str title + "]]" else Slug.str title

            let textEdit = { Range = range; NewText = newText }

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
    | Comp.LinkReference (range, needsClosing) ->
        // TODO: add link def completion
        let input = srcDoc.text.Substring(range)
        [||]

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
