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

    override this.ToString() =
        match this with
        | Comp.Title (range, needsClosing) ->
            $"Title: range={range.DebuggerDisplay}; needsClosing={needsClosing}"
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

let compOfText (pos: Position) (text: Text) : option<Comp> =
    let line = { text = text; line = pos.Line }
    let lineRange = Line.range line

    let cursor =
        match Line.toCursorAt pos line with
        | Some cursor -> Cursor.backward cursor
        | None -> if Line.endsAt pos line then Line.endCursor line else None


    match cursor with
    | None -> None
    | Some cursor ->
        let start =
            Cursor.tryFindCharMatching
                Cursor.backward
                (fun c -> Char.IsWhiteSpace(c) || c = '[')
                cursor

        let end_ =
            Cursor.tryFindCharMatching Cursor.forward Char.IsWhiteSpace cursor

        match start with
        | Some start ->
            match Cursor.backwardChar2 start with
            | Some ('[', '[') ->
                let rangeStart = (Cursor.pos start).NextChar(1)

                let rangeEnd =
                    end_ |> Option.map Cursor.pos |> Option.defaultValue lineRange.End

                Some(Comp.Title(Range.Mk(rangeStart, rangeEnd), true))
            | _ -> None
        | None -> None

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
