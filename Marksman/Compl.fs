module Marksman.Compl

open System

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types

open FSharpPlus.GenericBuilders

open Marksman.Cst
open Marksman.Index
open Marksman.Refs
open Marksman.Workspace
open Marksman.Misc
open Marksman.Text

let private logger = LogProvider.getLoggerByName "Compl"

[<RequireQualifiedAccess>]
type Compl =
    | DocPath of range: Range * needsClosing: bool
    | DocAnchor of destDoc: option<string> * range: Range * needsClosing: bool
    | WikiTitle of range: Range * needsClosing: bool
    | WikiHeading of destDoc: option<string> * range: Range * needsClosing: bool
    | LinkReference of range: Range * needsClosing: bool

    override this.ToString() =
        match this with
        | Compl.DocPath (range, needsClosing) ->
            $"DocPath: range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Compl.DocAnchor (destDoc, range, needsClosing) ->
            $"DocAnchor: dest={destDoc}; range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Compl.WikiTitle (range, needsClosing) ->
            $"WikiTitle: range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Compl.WikiHeading (destDoc, range, needsClosing) ->
            $"WikiHeading: dest={destDoc}; range={range.DebuggerDisplay}; needsClosing={needsClosing}"
        | Compl.LinkReference (range, needsClosing) ->
            $"LinkReference: range={range.DebuggerDisplay}; needsClosing={needsClosing}"

let compOfElement (pos: Position) (el: Element) : option<Compl> =
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

                Some(Compl.WikiTitle(range, false))
            | Some doc, None ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Compl.WikiTitle(doc.range, false))
                else
                    None
            | Some doc, Some hd ->
                if doc.range.ContainsInclusive(pos) then
                    Some(Compl.WikiTitle(doc.range, false))
                else if hd.range.ContainsInclusive(pos) then
                    let destDoc = doc.text
                    Some(Compl.WikiHeading(Some(destDoc), hd.range, false))
                else
                    None
            | None, Some hd ->
                if hd.range.ContainsInclusive(pos) then
                    Some(Compl.WikiHeading(None, hd.range, false))
                else
                    None
        | ML link ->
            match link.data with
            | MdLink.RF (_, label)
            | MdLink.RS label
            | MdLink.RC label -> Some(Compl.LinkReference(label.range, false))
            | MdLink.IL (_, Some url, _) ->
                let docUrl = DocUrl.ofUrlNode url

                match docUrl.url, docUrl.anchor with
                | Some url, _ when url.range.ContainsInclusive pos ->
                    Some(Compl.DocPath(url.range, false))
                | docUrl, Some anchor when anchor.range.ContainsInclusive pos ->
                    let destDoc = docUrl |> Option.map Node.text
                    Some(Compl.DocAnchor(destDoc, anchor.range, false))
                | _ -> None
            | _ -> None
        | _ -> None

let matchBracketParaElement (pos: Position) (line: Line) : option<Compl> =
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
        if Cursor.backwardChar2 start = Some('[', '[') then
            if Cursor.forwardChar start = Some '#' then
                let rangeStart = (Cursor.pos start).NextChar(2)
                Some(Compl.WikiHeading(None, Range.Mk(rangeStart, rangeEnd), true))
            else
                let rangeStart = (Cursor.pos start).NextChar(1)
                Some(Compl.WikiTitle(Range.Mk(rangeStart, rangeEnd), true))
        else if Cursor.char start = '[' then
            let rangeStart = (Cursor.pos start).NextChar(1)

            let needsClosing =
                paraElEnd |> Option.exists (fun c -> Cursor.char c = ']') |> not

            Some(Compl.LinkReference(Range.Mk(rangeStart, rangeEnd), needsClosing))
        else
            None
    | _ -> None

let matchParenParaElement (pos: Position) (line: Line) : option<Compl> =
    let lineRange = Line.range line

    let scanCursor =
        match line |> Line.toCursorAt pos with
        | Some cursor -> Cursor.backward cursor
        | None -> if Line.endsAt pos line then Line.endCursor line else None

    let findOpenOrWs =
        Cursor.tryFindCharMatching Cursor.backward (fun c ->
            Char.IsWhiteSpace(c) || c = '(' || c = '[')

    let paraElStart = scanCursor |> Option.bind findOpenOrWs

    let findCloseOrWs =
        Cursor.tryFindCharMatching Cursor.forward (fun c ->
            Char.IsWhiteSpace(c) || c = ')' || c = ']')

    let paraElEnd = scanCursor |> Option.bind findCloseOrWs

    let rangeEnd =
        paraElEnd
        |> Option.map Cursor.pos
        |> Option.defaultValue lineRange.End

    match paraElStart with
    | Some start ->
        if Cursor.backwardChar2 start = Some(']', '(') then
            let rangeStart = (Cursor.pos start).NextChar(1)
            let range = Range.Mk(rangeStart, rangeEnd)
            let input = line.text.Substring(range)
            let docUrl = DocUrl.ofUrlNode (Node.mkText input range)

            let needsClosing =
                paraElEnd |> Option.exists (fun c -> Cursor.char c = ')') |> not

            match docUrl.url, docUrl.anchor with
            | Some url, anchor when url.range.ContainsInclusive pos ->
                Some(Compl.DocPath(url.range, Option.isNone anchor && needsClosing))
            | docUrl, Some anchor when anchor.range.ContainsInclusive pos ->
                let destDoc = docUrl |> Option.map Node.text
                Some(Compl.DocAnchor(destDoc, anchor.range, needsClosing))
            | _ -> None

        else
            None
    | _ -> None

let compOfText (pos: Position) (text: Text) : option<Compl> =
    monad {
        let! line = Line.ofPos text pos

        let! comp =
            matchBracketParaElement pos line
            |> Option.orElseWith (fun () -> matchParenParaElement pos line)

        comp
    }

let findCompAtPoint (pos: Position) (doc: Doc) : option<Compl> =
    let elComp =
        doc
        |> Doc.index
        |> Index.links
        |> Seq.map (compOfElement pos)
        |> Seq.tryFind Option.isSome
        |> Option.flatten

    match elComp with
    | Some _ -> elComp
    | None -> compOfText pos doc.text

let findCandidatesInDoc (comp: Compl) (srcDoc: Doc) (folder: Folder) : array<CompletionItem> =
    match comp with
    | Compl.WikiTitle (range, needsClosing) ->
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
                Detail = Some doc.RelPath
                TextEdit = Some textEdit
                FilterText = Some title }

        Seq.map toCompletionItem matchingTitles |> Array.ofSeq
    | Compl.WikiHeading (destDocTitle, range, needsClosing) ->
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
                let newText = Slug.str hd
                let newText = if needsClosing then newText + "]]" else newText
                let textEdit = { Range = range; NewText = newText }

                { CompletionItem.Create(hd) with
                    Detail = None
                    TextEdit = Some textEdit
                    FilterText = Some hd }

            matchingHeadings |> Seq.map toCompletionItem |> Array.ofSeq
    | Compl.LinkReference (range, needsClosing) ->
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
    | Compl.DocPath (range, needsClosing) ->
        let input = srcDoc.text.Substring(range)

        let isMatching (doc: Doc) =
            input.IsSubSequenceOf(doc.RelPath)
            || input.IsSubSequenceOf(doc.RelPath.AbsPathUrlEncode())

        let matchingDocs = folder.docs |> Map.values |> Seq.filter isMatching

        let toCompletionItem (doc: Doc) =
            let newText = doc.RelPath.AbsPathUrlEncode()
            let newText = if needsClosing then newText + ")" else newText
            let textEdit = { Range = range; NewText = newText }

            { CompletionItem.Create(doc.RelPath) with
                Detail = Some(Doc.name doc)
                TextEdit = Some textEdit
                FilterText = Some doc.RelPath }

        matchingDocs |> Seq.map toCompletionItem |> Array.ofSeq
    | Compl.DocAnchor (destDocUrl, range, needsClosing) ->
        let destDoc =
            match destDocUrl with
            | Some url ->
                let docRef = DocRef.Url url
                DocRef.tryFindDoc folder srcDoc docRef
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
    let doc = Folder.tryFindDocByPath docUri folder

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
