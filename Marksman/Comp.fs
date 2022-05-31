module Marksman.Comp

open System

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types

open FSharpPlus.GenericBuilders

open Marksman.Parser
open Marksman.Workspace
open Marksman.Misc
open Marksman.Text

let private logger = LogProvider.getLoggerByName "Comp"

[<RequireQualifiedAccess>]
type Comp =
    | DocPath of range: Range * needsClosing: bool
    | DocAnchor of destDoc: option<string> * range: Range * needsClosing: bool
    | WikiTitle of range: Range * needsClosing: bool
    | WikiHeading of destDoc: option<string> * range: Range
    | LinkReference of range: Range * needsClosing: bool

    override this.ToString() =
        match this with
        | Comp.DocPath (range, needsClosing) ->
            $"DocPath: range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Comp.DocAnchor (destDoc, range, needsClosing) ->
            $"DocAnchor: dest={destDoc}; range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Comp.WikiTitle (range, needsClosing) ->
            $"WikiTitle: range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Comp.WikiHeading (destDoc, range) ->
            $"WikiHeading: dest={destDoc}; range={range.DebuggerDisplay}"
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

                Some(Comp.WikiTitle(range, false))
            | Some doc, None ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Comp.WikiTitle(doc.range, false))
                else
                    None
            | Some doc, Some hd ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Comp.WikiTitle(doc.range, false))
                else if hd.range.ContainsInclusive(pos) then
                    let destDoc = doc.text
                    Some(Comp.WikiHeading(Some(destDoc), hd.range))
                else
                    None
            | None, Some hd ->
                if hd.range.ContainsInclusive(pos) then
                    Some(Comp.WikiHeading(None, hd.range))
                else
                    None
        | ML link ->
            match link.data with
            | MdLink.RF (_, label)
            | MdLink.RS label
            | MdLink.RC label -> Some(Comp.LinkReference(label.range, false))
            | MdLink.IL (_, Some url, _) ->
                let docUrl = DocUrl.ofUrlNode url

                match docUrl.url, docUrl.anchor with
                | Some url, _ when url.range.ContainsInclusive pos ->
                    Some(Comp.DocPath(url.range, false))
                | docUrl, Some anchor when anchor.range.ContainsInclusive pos ->
                    let destDoc = docUrl |> Option.map Node.text
                    Some(Comp.DocAnchor(destDoc, anchor.range, false))
                | _ -> None
            | _ -> None
        | _ -> None

let matchBracketParaElement (pos: Position) (line: Line) : option<Comp> =
    let lineRange = Line.range line

    let scanCursor =
        match line |> Line.toCursorAt pos with
        | Some cursor -> Cursor.backward cursor
        | None -> if Line.endsAt pos line then Line.endCursor line else None

    let findOpenOrWs =
        Cursor.tryFindCharMatching Cursor.backward (fun c ->
            Char.IsWhiteSpace(c) || c = '[' || c = '(')

    let paraElStart = scanCursor |> Option.bind findOpenOrWs

    let findCloseOrWs =
        Cursor.tryFindCharMatching Cursor.forward (fun c -> Char.IsWhiteSpace(c) || c = ']')

    let paraElEnd = scanCursor |> Option.bind findCloseOrWs

    let rangeEnd =
        paraElEnd
        |> Option.map Cursor.pos
        |> Option.defaultValue lineRange.End

    match paraElStart with
    | Some start ->
        match Cursor.backwardChar2 start with
        | Some ('[', '[') ->
            let rangeStart = (Cursor.pos start).NextChar(1)
            Some(Comp.WikiTitle(Range.Mk(rangeStart, rangeEnd), true))
        | _ ->
            if Cursor.char start = '[' then
                let rangeStart = (Cursor.pos start).NextChar(1)

                let needsClosing =
                    paraElEnd |> Option.exists (fun c -> Cursor.char c = ']') |> not

                Some(Comp.LinkReference(Range.Mk(rangeStart, rangeEnd), needsClosing))
            else
                None
    | _ -> None

let matchParenParaElement (pos: Position) (line: Line) : option<Comp> =
    let lineRange = Line.range line

    let scanCursor =
        match line |> Line.toCursorAt pos with
        | Some cursor -> Cursor.backward cursor
        | None -> if Line.endsAt pos line then Line.endCursor line else None

    let findOpenOrWs =
        Cursor.tryFindCharMatching Cursor.backward (fun c -> Char.IsWhiteSpace(c) || c = '(')

    let paraElStart = scanCursor |> Option.bind findOpenOrWs

    let findCloseOrWs =
        Cursor.tryFindCharMatching Cursor.forward (fun c -> Char.IsWhiteSpace(c) || c = ')')

    let paraElEnd = scanCursor |> Option.bind findCloseOrWs

    let rangeEnd =
        paraElEnd
        |> Option.map Cursor.pos
        |> Option.defaultValue lineRange.End

    match paraElStart with
    | Some start ->
        if Cursor.char start = '(' then
            let rangeStart = (Cursor.pos start).NextChar(1)
            let range = Range.Mk(rangeStart, rangeEnd)
            let input = line.text.Substring(range)
            let docUrl = DocUrl.ofUrlNode (Node.mkText input range)

            let needsClosing =
                paraElEnd |> Option.exists (fun c -> Cursor.char c = ')') |> not

            match docUrl.url, docUrl.anchor with
            | Some url, anchor when url.range.ContainsInclusive pos ->
                Some(Comp.DocPath(url.range, Option.isNone anchor && needsClosing))
            | docUrl, Some anchor when anchor.range.ContainsInclusive pos ->
                let destDoc = docUrl |> Option.map Node.text
                Some(Comp.DocAnchor(destDoc, anchor.range, needsClosing))
            | _ -> None

        else
            None
    | _ -> None

let compOfText (pos: Position) (text: Text) : option<Comp> =
    monad {
        let! line = Line.ofPos text pos

        let! comp =
            matchBracketParaElement pos line
            |> Option.orElseWith (fun () -> matchParenParaElement pos line)

        comp
    }

let findCompAtPoint (pos: Position) (doc: Doc) : option<Comp> =
    let elComp =
        Doc.elementsAll doc
        |> Seq.map (compOfElement pos)
        |> Seq.tryFind Option.isSome
        |> Option.flatten

    match elComp with
    | Some _ -> elComp
    | None -> compOfText pos doc.text

let findCandidatesInDoc (comp: Comp) (srcDoc: Doc) (folder: Folder) : array<CompletionItem> =
    match comp with
    | Comp.WikiTitle (range, needsClosing) ->
        let input = srcDoc.text.Substring(range)
        let inputSlug = Slug.ofString input

        let titleWhenMatch (doc: Doc) =
            match Doc.title doc with
            | Some { data = heading } ->
                let title = (Heading.name heading)
                let titleSlug = Slug.ofString title

                if Slug.isSubSequence inputSlug titleSlug then [ doc, title ] else []
            | None -> []

        let matchingTitles =
            folder.docs |> Map.values |> Seq.collect titleWhenMatch

        let toCompletionItem (doc: Doc, title: string) =
            let newText =
                if needsClosing then Slug.str title + "]]" else Slug.str title

            let textEdit = { Range = range; NewText = newText }

            { CompletionItem.Create(title) with
                Detail = Some doc.relPath
                TextEdit = Some textEdit
                FilterText = Some title }

        Seq.map toCompletionItem matchingTitles |> Array.ofSeq
    | Comp.WikiHeading (destDocTitle, range) ->
        let destDoc =
            match destDocTitle with
            | None -> Some srcDoc
            | Some destDocTitle ->
                let destDocSlug = Slug.ofString destDocTitle

                let matchTitle (doc: Doc) =
                    let docSlug = Slug.ofString (Doc.name doc)
                    docSlug = destDocSlug

                folder.docs |> Map.values |> Seq.tryFind matchTitle

        match destDoc with
        | None -> [||]
        | Some destDoc ->
            let input = srcDoc.text.Substring(range)
            let inputSlug = Slug.ofString input

            let matchingHeadings =
                Doc.headings destDoc
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
        let input = srcDoc.text.Substring(range)

        let matchingDefs =
            Doc.linkDefs srcDoc
            |> Seq.filter (fun { data = def } -> input.IsSubSequenceOf(def.label.text))
            |> Seq.map Node.data

        let toCompletionItem def =
            let newText = def.label.text

            let newText = if needsClosing then newText + "]" else newText

            let textEdit = { Range = range; NewText = newText }

            { CompletionItem.Create(def.label.text) with
                Detail = def.title |> Option.map Node.text
                Documentation = def.url |> Node.text |> Documentation.String |> Some
                TextEdit = Some textEdit
                FilterText = Some def.label.text }

        matchingDefs |> Seq.map toCompletionItem |> Array.ofSeq
    | Comp.DocPath (range, needsClosing) ->
        let input = srcDoc.text.Substring(range)

        let isMatching doc =
            input.IsSubSequenceOf(doc.relPath)
            || input.IsSubSequenceOf(doc.relPath.AbsPathUrlEncode())

        let matchingDocs = folder.docs |> Map.values |> Seq.filter isMatching

        let toCompletionItem doc =
            let newText = doc.relPath.AbsPathUrlEncode()
            let newText = if needsClosing then newText + ")" else newText
            let textEdit = { Range = range; NewText = newText }

            { CompletionItem.Create(doc.relPath) with
                Detail = Some(Doc.name doc)
                TextEdit = Some textEdit
                FilterText = Some doc.relPath }

        matchingDocs |> Seq.map toCompletionItem |> Array.ofSeq
    | Comp.DocAnchor (destDocUrl, range, needsClosing) ->
        let destDoc =
            match destDocUrl with
            | Some url ->
                let isMatching doc = doc.relPath.AbsPathUrlEncode() = url
                folder.docs |> Map.values |> Seq.tryFind isMatching
            | None -> Some srcDoc

        match destDoc with
        | Some destDoc ->
            let input = srcDoc.text.Substring(range)
            let inputSlug = Slug.ofString input

            let matchingHeadings =
                Doc.headings destDoc
                |> Seq.filter (fun { data = h } -> h.level <> 1)
                |> Seq.map (fun { data = h } -> Heading.name h)
                |> Seq.filter (fun h -> (Slug.ofString h) |> Slug.isSubSequence inputSlug)

            let toCompletionItem hd =
                let newText = Slug.str hd
                let newText = if needsClosing then newText + ")" else newText
                let textEdit = { Range = range; NewText = newText }

                { CompletionItem.Create(hd) with
                    Detail = None
                    TextEdit = Some textEdit
                    FilterText = Some hd }

            matchingHeadings |> Seq.map toCompletionItem |> Array.ofSeq
        | None -> [||]

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
