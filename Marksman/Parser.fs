module Marksman.Parser

open System
open Ionide.LanguageServerProtocol.Types

open Text
open Misc

type DocName = string

[<RequireQualifiedAccess>]
type RefDest =
    | Doc of DocName
    | Heading of doc: option<DocName> * heading: string

module RefDest =
    let fmt dest =
        let inner =
            match dest with
            | RefDest.Doc name -> name
            | RefDest.Heading (doc, heading) -> $"{doc |> Option.defaultValue String.Empty}#{heading}"

        $"[[{inner}]]"

    let destDoc =
        function
        | RefDest.Doc name -> Some name
        | RefDest.Heading (docOpt, _) -> docOpt

    let destHeading =
        function
        | RefDest.Doc _ -> None
        | RefDest.Heading (_, heading) -> Some heading

    let tryFromString (text: string) : option<RefDest> =
        let isValid =
            (text.StartsWith "[[" && text.EndsWith "]]")

        if not isValid then
            None
        else
            let targetLength = text.Length - 2 - 2 // -[[ -]]

            assert (targetLength >= 0)
            let inner = text.Substring(2, targetLength) // drop [[ and ]]
            let parts = inner.Split('#', 2)

            if parts.Length = 1 then
                RefDest.Doc inner |> Some
            else
                let doc = parts[0]
                let heading = parts[1]

                if String.IsNullOrWhiteSpace doc then
                    RefDest.Heading(None, heading) |> Some
                else
                    RefDest.Heading(Some doc, heading) |> Some

type Ref =
    { text: string
      dest: RefDest
      range: Range }

module Ref =
    let fmt x =
        $"R: {RefDest.fmt x.dest}; {x.range.DebuggerDisplay}"

type CompletionPoint = { text: string; range: Range }

module CompletionPoint =
    let fmt cp =
        $"CP: `{cp.text}`: {cp.range.DebuggerDisplay}"

    let isRef cp = cp.text.StartsWith("[[")

    let destNote cp : option<DocName> =
        if isRef cp then
            cp.text.Substring(2) |> Some
        else
            None

type StringLoc = { content: string; range: Range }

module StringLoc =
    let mk (content: string) (range: Range) = { content = content; range = range }
    let content (x: StringLoc) = x.content
    let range (x: StringLoc) = x.range

[<RequireQualifiedAccess>]
type LinkDescr =
    // inline
    | IL of label: StringLoc * url: option<StringLoc> * title: option<StringLoc>
    // reference full
    | RF of text: StringLoc * label: StringLoc
    // reference collapsed
    | RC of label: StringLoc
    // reference shortcut
    | RS of label: StringLoc

module LinkDescr =
    let fmt: LinkDescr -> string =
        function
        | LinkDescr.IL (label, url, title) ->
            let destParts =
                seq {
                    for part in [ url; title ] do
                        match Option.map StringLoc.content part with
                        | Some str -> yield str
                        | None -> ()
                }

            let dest = String.Join(" ", destParts)

            $"[{label.content}]({dest})"

type Link = { text: StringLoc; descr: LinkDescr }

module Link =
    let fmt (l: Link) = $"L: {LinkDescr.fmt l.descr}"

type Element =
    | H of Heading
    | R of Ref
    | L of Link
    | CP of CompletionPoint

and Heading =
    { level: int
      text: string
      range: Range
      scope: Range
      children: array<Element> }

let rec private fmtElement =
    function
    | H h -> fmtHeading h
    | R x -> Ref.fmt x
    | L l -> Link.fmt l
    | CP cp -> CompletionPoint.fmt cp

and private fmtHeading h =
    let l1 =
        $"H{h.level}: range={h.range.DebuggerDisplay}; scope={h.scope.DebuggerDisplay}"

    let l2 = $"  text=`{h.text}`"

    let rest =
        Array.map (indentFmt fmtElement) h.children

    String.Join(Environment.NewLine, Array.concat [ [| l1; l2 |]; rest ])

module Heading =
    let fmt = fmtHeading

    let text (heading: Heading) : string = heading.text

    let title (heading: Heading) : string =
        heading.text.TrimStart(' ', '#').TrimEnd(' ')

    let isTitle (heading: Heading) = heading.level <= 1

    let range (heading: Heading) : Range = heading.range

    let scope (heading: Heading) : Range = heading.scope

module Element =
    let fmt = fmtElement

    let range =
        function
        | H h -> h.range
        | R ref -> ref.range
        | L l -> l.text.range
        | CP cp -> cp.range

    let text =
        function
        | H h -> h.text
        | R ref -> ref.text
        | L l -> l.text.content
        | CP cp -> cp.text

    let asHeading =
        function
        | H h -> Some h
        | _ -> None

    let asRef =
        function
        | R ref -> Some ref
        | _ -> None

    let pickHeadings (elements: array<Element>) : array<Heading> =
        elements
        |> Array.map asHeading
        |> Array.collect Option.toArray

module Markdown =
    open Markdig
    open Markdig.Syntax
    open Markdig.Syntax.Inlines
    open Markdig.Parsers
    open Markdig.Helpers

    type MarksmanLinkType =
        | DoubleBracket
        | BracketColon

    type MarksmanLink(text: string) =
        inherit LeafInline()
        member val Text = text

    type MarksmanCompletionPoint(text: string) =
        inherit LeafInline()
        member val Text = text

    type MarksmanLinkParser() as this =
        inherit InlineParser()

        do this.OpeningCharacters <- [| '[' |]

        override this.Match(processor, slice) =
            let nextChar = slice.PeekCharExtra(1)

            let isRef = nextChar = '['

            if isRef then
                let start = slice.Start

                let offsetStart =
                    processor.GetSourcePosition(start)

                let mutable found = false
                let mutable current = slice.NextChar()

                let shouldStop (c: char) =
                    c.IsNewLineOrLineFeed() || c.IsZero() || found

                while not (shouldStop current) do
                    if current = ']' then
                        let prev = slice.PeekCharExtra(-1)

                        if prev = ']' then
                            found <- true
                        else
                            current <- slice.NextChar()
                    else
                        current <- slice.NextChar()

                if found then
                    let end_ = slice.Start
                    let offsetEnd = offsetStart + (end_ - start)

                    let text =
                        slice.Text.Substring(start, end_ - start + 1)

                    let link = MarksmanLink(text)
                    link.Span <- SourceSpan(offsetStart, offsetEnd)
                    processor.Inline <- link

                found
            else
                false

    type MarksmanCompletionPointParser() as this =
        inherit InlineParser()

        do this.OpeningCharacters <- [| '[' |]

        override this.Match(processor, slice) =
            let start = slice.Start

            let offsetStart =
                processor.GetSourcePosition(start)

            let mutable current = slice.NextChar()

            let shouldStop (char: char) =
                char.IsWhiteSpaceOrZero()
                || char.IsNewLineOrLineFeed()
                || char = ']'

            while not (shouldStop current) do
                current <- slice.NextChar()

            // We've found an unclosed link/ref
            if current <> ']' then
                // -1 to exclude the whitespace/newline/0 from the completion point element
                let end_ = slice.Start - 1

                let offsetEnd = offsetStart + (end_ - start)

                let text =
                    slice.Text.Substring(start, end_ - start + 1)

                let link = MarksmanCompletionPoint(text)
                link.Span <- SourceSpan(offsetStart, offsetEnd)
                processor.Inline <- link
                true
            else
                false

    let markdigPipeline =
        let pipelineBuilder =
            MarkdownPipelineBuilder()
                .UsePreciseSourceLocation()

        pipelineBuilder.InlineParsers.Insert(0, MarksmanCompletionPointParser())
        pipelineBuilder.InlineParsers.Insert(0, MarksmanLinkParser())

        pipelineBuilder.Build()

    let sourceSpanToRange (text: Text) (span: SourceSpan) : Range =
        let start =
            text.lineMap.FindPosition(span.Start)

        let endInclusive =
            text.lineMap.FindPosition(span.End)

        let endOffset =
            if Char.IsSurrogate(text.content, span.End) then
                2
            else
                1


        { Start = start
          End = { endInclusive with Character = endInclusive.Character + endOffset } }


    let scrapeText (text: Text) : array<Element> =
        let parsed: MarkdownObject =
            Markdown.Parse(text.content, markdigPipeline)

        let elements = ResizeArray()

        for b in parsed.Descendants() do
            match b with
            | :? HeadingBlock as h ->
                let level = h.Level

                let title =
                    text.content.Substring(h.Span.Start, h.Span.Length)

                let range = sourceSpanToRange text h.Span

                let heading =
                    H
                        { level = level
                          text = title
                          range = range
                          scope = range
                          children = [||] }

                elements.Add(heading)
            | :? MarksmanLink as link ->
                let fullText = link.Text

                match RefDest.tryFromString fullText with
                | Some dest ->
                    let range = sourceSpanToRange text link.Span

                    let xref =
                        R
                            { text = fullText
                              dest = dest
                              range = range }

                    elements.Add(xref)
                | _ -> ()
            | :? MarksmanCompletionPoint as cp ->
                let fullText = cp.Text
                let range = sourceSpanToRange text cp.Span
                elements.Add(CP { text = fullText; range = range })
            | :? LinkInline as l ->
                let linkRange =
                    sourceSpanToRange text l.Span

                let linkText =
                    text.content.Substring(l.Span.Start, l.Span.Length)

                let labelSpan = l.LabelSpan

                let label =
                    if labelSpan.IsEmpty then
                        String.Empty
                    else
                        text.content.Substring(labelSpan.Start, labelSpan.End)

                let titleSpan = l.TitleSpan
                let title = l.Title

                let urlSpan = l.UrlSpan
                let url = l.Url

                if not l.IsShortcut then
                    let label =
                        StringLoc.mk label (sourceSpanToRange text labelSpan)

                    let url =
                        if urlSpan.IsEmpty then
                            None
                        else
                            Some(StringLoc.mk url (sourceSpanToRange text urlSpan))

                    let title =
                        if titleSpan.IsEmpty then
                            None
                        else
                            Some(StringLoc.mk title (sourceSpanToRange text titleSpan))

                    let link =
                        LinkDescr.IL(label = label, url = url, title = title)

                    elements.Add(
                        L
                            { text = StringLoc.mk linkText linkRange
                              descr = link }
                    )
            | _ -> ()

        elements.ToArray()

let rec private reconstructHierarchy (text: Text) (flat: seq<Element>) : seq<Element> =
    seq {
        let mutable headStack: list<Heading> = []
        let mutable accChildren: list<Element> = []

        let mutable accStandalone: list<Element> =
            []

        let rec unwindHeadStack newHead : unit =
            match headStack with
            | [] ->
                accStandalone <-
                    List.concat [ accChildren
                                  accStandalone ]

                accChildren <- []
                headStack <- [ newHead ]
            | curHead :: rest ->
                // #
                // ##
                //  e1, e2
                // ###       |
                //  e3, e4   |
                // ##        |
                //  e5
                let curHeadChildren =
                    Array.concat [ curHead.children
                                   Array.ofList accChildren ]

                let curHead =
                    { curHead with children = curHeadChildren }

                accChildren <- []

                // Unwind further until we find a parent heading or none at all
                if curHead.level >= newHead.level then
                    let newScope =
                        { Start = curHead.scope.Start
                          End = newHead.scope.Start }

                    let curHead =
                        { curHead with scope = newScope }

                    accChildren <- [ H curHead ]
                    headStack <- rest
                    unwindHeadStack newHead
                else // cur.level < new.level; should stack the child
                    headStack <- newHead :: curHead :: rest

        for el in flat do
            match el with
            | R _
            | L _
            | CP _ ->
                match headStack with
                | _ :: _ -> accChildren <- el :: accChildren
                | [] -> yield el
            | H newHead -> unwindHeadStack newHead

        let guardHead =
            { level = -1
              text = ""
              scope = text.EndRange()
              range = text.EndRange()
              children = [||] }

        unwindHeadStack guardHead

        for child in accChildren do
            yield child

        for child in accStandalone do
            yield child
    }

let rec private sortElements (text: Text) (elements: array<Element>) : unit =
    for el in elements do
        match el with
        | H h -> sortElements text h.children
        | _ -> ()

    let elementStart el =
        let range = (Element.range el)

        let start =
            text.lineMap.FindOffset(range.Start)

        let end_ =
            text.lineMap.FindOffset(range.End)

        (start, end_)

    Array.sortInPlaceBy elementStart elements

let rec parseText (text: Text) : array<Element> =
    let flatElements = Markdown.scrapeText text

    let hierarchicalElements =
        reconstructHierarchy text flatElements

    let elements =
        Array.ofSeq hierarchicalElements

    sortElements text elements
    elements
