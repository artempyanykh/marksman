module Marksman.Parser

open System
open Ionide.LanguageServerProtocol.Types
open Markdig.Syntax

open Marksman.Text
open Marksman.Cst
open Marksman.Misc

module Markdown =
    open Markdig
    open Markdig.Syntax.Inlines
    open Markdig.Parsers
    open Markdig.Helpers
    open Markdig.Extensions.Yaml

    type WikiLinkInline
        (
            text: string,
            doc: Option<string * SourceSpan>,
            heading: option<string * SourceSpan>
        ) =
        inherit LeafInline()
        member val Text = text

        member val Doc = Option.map fst doc
        member val DocSpan = Option.map snd doc

        member val Heading = Option.map fst heading
        member val HeadingSpan = Option.map snd heading

    type TagInline(text: string) =
        inherit LeafInline()

        member val Text = text


    type TagsParser() as this =
        inherit InlineParser()

        do this.OpeningCharacters <- [| '#' |]

        override this.Match(processor, slice) =
            let start = slice.Start
            let offsetStart = processor.GetSourcePosition(slice.Start)

            let shouldAccept (c: char) = c.IsAlphaNumeric() || c = '-' || c = '_'

            while (shouldAccept (slice.PeekChar())) do
                slice.NextChar() |> ignore

            let end_ = slice.Start
            let offsetEnd = offsetStart + (end_ - start)

            if end_ > start then
                let text = slice.Text.Substring(start, end_ - start + 1)
                let tag = TagInline(text)
                tag.Span <- SourceSpan(offsetStart, offsetEnd)
                processor.Inline <- tag
                true
            else
                false


    type WikiLinkParser() as this =
        inherit InlineParser()

        do this.OpeningCharacters <- [| '[' |]

        override this.Match(processor, slice) =
            let nextChar = slice.PeekCharExtra(1)

            let isRef = nextChar = '['

            if isRef then
                let start = slice.Start
                let offsetStart = processor.GetSourcePosition(start)
                let offsetInnerStart = offsetStart + 2

                let mutable offsetHashDelim: option<int> = None
                let mutable found = false
                let mutable current = slice.NextChar()

                let shouldStop (c: char) = c.IsNewLineOrLineFeed() || c.IsZero() || found

                while not (shouldStop current) do
                    if
                        current = '#'
                        && slice.PeekCharExtra(-1) <> '\\'
                        && offsetHashDelim.IsNone
                    then
                        offsetHashDelim <- Some(processor.GetSourcePosition(slice.Start))

                    if current = ']' then
                        let prev = slice.PeekCharExtra(-1)

                        if prev = ']' then found <- true else current <- slice.NextChar()
                    else
                        current <- slice.NextChar()

                if found then
                    let end_ = slice.Start
                    let offsetEnd = offsetStart + (end_ - start)
                    let offsetInnerEnd = offsetEnd - 2

                    let text = slice.Text.Substring(start, end_ - start + 1)

                    let doc, heading =
                        match offsetHashDelim with
                        | Some offsetHashDelim ->
                            let offsetDocStart = offsetInnerStart
                            let offsetDocEnd = offsetHashDelim - 1
                            let offsetHeadingStart = offsetHashDelim + 1
                            let offsetHeadingEnd = offsetInnerEnd

                            let docText =
                                if offsetDocEnd >= offsetDocStart then
                                    slice.Text.Substring(
                                        start + 2,
                                        offsetDocEnd - offsetDocStart + 1
                                    )
                                else
                                    String.Empty

                            let headingText =
                                if offsetHeadingEnd >= offsetHeadingStart then
                                    slice.Text.Substring(
                                        start + 2 + (offsetDocEnd - offsetDocStart + 1) + 1,
                                        offsetHeadingEnd - offsetHeadingStart + 1
                                    )
                                else
                                    String.Empty

                            let doc =
                                if String.IsNullOrEmpty docText then
                                    None
                                else
                                    (docText, SourceSpan(offsetDocStart, offsetDocEnd)) |> Some

                            let heading =
                                (headingText, SourceSpan(offsetHeadingStart, offsetHeadingEnd))
                                |> Some

                            doc, heading

                        | None ->
                            let offsetDocStart = offsetStart + 2
                            let offsetDocEnd = offsetEnd - 2

                            let docText =
                                slice.Text.Substring(start + 2, offsetDocEnd - offsetDocStart + 1)

                            Some(docText, SourceSpan(offsetDocStart, offsetDocEnd)), None


                    let link = WikiLinkInline(text, doc, heading)
                    link.Span <- SourceSpan(offsetStart, offsetEnd)
                    processor.Inline <- link

                found
            else
                false

    let markdigPipeline =
        let pipelineBuilder =
            MarkdownPipelineBuilder()
                .UsePreciseSourceLocation()
                .UseYamlFrontMatter()

        pipelineBuilder.InlineParsers.Insert(0, MarkdigPatches.PatchedLinkInlineParser())
        pipelineBuilder.InlineParsers.Insert(0, WikiLinkParser())
        pipelineBuilder.InlineParsers.Add(TagsParser())
        pipelineBuilder.Build()

    let sourceSpanToRange (text: Text) (span: SourceSpan) : Range =
        let start = text.lineMap.FindPosition(span.Start)

        if span.IsEmpty then
            { Start = start; End = start }
        else
            let endInclusive = text.lineMap.FindPosition(span.End)
            let endOffset = if Char.IsSurrogate(text.content, span.End) then 2 else 1

            { Start = start
              End = { endInclusive with Character = endInclusive.Character + endOffset } }


    let scrapeText (text: Text) : array<Element> =
        let parsed: MarkdownObject = Markdown.Parse(text.content, markdigPipeline)

        let elements = ResizeArray()

        for b in parsed.Descendants() do
            match b with
            | :? YamlFrontMatterBlock as y ->
                let fullText = text.content.Substring(y.Span.Start, y.Span.Length)
                let range = sourceSpanToRange text y.Span

                let node: TextNode = Node.mkText fullText range

                elements.Add(YML node)

            | :? HeadingBlock as h ->
                let level = h.Level

                // TODO: remove after https://github.com/xoofx/markdig/pull/696 is released
                if h.Span.End < text.content.Length then
                    let fullText = text.content.Substring(h.Span.Start, h.Span.Length)
                    let title0 = fullText.TrimStart(' ', '#')
                    let headingPrefixLen = fullText.Length - title0.Length
                    let title = title0.TrimEnd(' ')
                    let headingSuffixLen = title0.Length - title.Length

                    let titleRange =
                        sourceSpanToRange
                            text
                            (SourceSpan(
                                h.Span.Start + headingPrefixLen,
                                h.Span.End - headingSuffixLen
                            ))

                    let range = sourceSpanToRange text h.Span

                    let heading =
                        Node.mk
                            fullText
                            range
                            { level = level
                              title = Node.mkText title titleRange
                              scope = range
                              children = [||] }

                    elements.Add(H heading)
            | :? WikiLinkInline as link ->
                let doc =
                    match link.Doc, link.DocSpan with
                    | Some doc, Some docSpan ->
                        Node.mkText doc (sourceSpanToRange text docSpan) |> Some
                    | _ -> None

                let heading =
                    match link.Heading, link.HeadingSpan with
                    | Some heading, Some headingSpan ->
                        Node.mkText heading (sourceSpanToRange text headingSpan) |> Some
                    | _ -> None

                let wikiLink: WikiLink = { doc = doc; heading = heading }
                let range = sourceSpanToRange text link.Span
                let xref = Node.mk link.Text range wikiLink
                elements.Add(WL xref)
            | :? LinkInline as l ->
                let linkRange = sourceSpanToRange text l.Span

                let linkText = text.content.Substring(l.Span.Start, l.Span.Length)

                let labelSpan = l.LabelSpan

                let isRegularLink = linkText.EndsWith(')')

                let label =
                    if labelSpan.IsEmpty then
                        String.Empty
                    else
                        text.content.Substring(labelSpan.Start, labelSpan.Length)

                let titleSpan = l.TitleSpan
                let title = l.Title

                let urlSpan = l.UrlSpan
                let url = l.Url

                if not l.IsShortcut then
                    if isRegularLink then
                        let label = Node.mkText label (sourceSpanToRange text labelSpan)

                        let url =
                            if urlSpan.IsEmpty then
                                None
                            else
                                Some(Node.mkText url (sourceSpanToRange text urlSpan))

                        let title =
                            if titleSpan.IsEmpty then
                                None
                            else
                                Some(Node.mkText title (sourceSpanToRange text titleSpan))

                        let link =
                            MdLink.IL(text = label, url = url, title = title)
                            |> Node.mk linkText linkRange

                        elements.Add(ML link)
                    // Another hack: url span = label span => collapsed ref
                    else if urlSpan = labelSpan then
                        let label = Node.mkText label (sourceSpanToRange text labelSpan)
                        let link = MdLink.RC label |> Node.mk linkText linkRange
                        elements.Add(ML link)
                    // The last remaining option is full reference
                    else
                        let text_ = Node.mkText label (sourceSpanToRange text labelSpan)
                        let label = Node.mkText url (sourceSpanToRange text urlSpan)
                        let link = MdLink.RF(text_, label) |> Node.mk linkText linkRange
                        elements.Add(ML link)
                else
                    let label = Node.mkText label (sourceSpanToRange text labelSpan)
                    let link = MdLink.RS(label) |> Node.mk linkText linkRange
                    elements.Add(ML link)
            | :? LinkReferenceDefinition as linkDef ->
                let defRange = sourceSpanToRange text linkDef.Span

                let defText =
                    text.content.Substring(linkDef.Span.Start, linkDef.Span.Length)

                let label = linkDef.Label
                let labelSpan = linkDef.LabelSpan
                let label = Node.mkText label (sourceSpanToRange text labelSpan)

                let url = linkDef.Url
                let urlSpan = linkDef.UrlSpan
                let url = Node.mkText url (sourceSpanToRange text urlSpan)

                let title =
                    if linkDef.TitleSpan.IsEmpty then
                        None
                    else
                        let title = linkDef.Title
                        let titleSpan = linkDef.TitleSpan
                        Node.mkText title (sourceSpanToRange text titleSpan) |> Some

                let def = MdLinkDef.mk label url title |> Node.mk defText defRange

                elements.Add(MLD def)

                ()
            | :? TagInline as tag ->
                let tagText = tag.Text
                let tagRange = sourceSpanToRange text tag.Span

                let nameText, nameRange =
                    if tagText.StartsWith('#') then
                        tagText.Substring(1),
                        { Start = tagRange.Start.NextChar(1); End = tagRange.End }
                    else
                        tagText, tagRange

                let tag = { name = Node.mkText nameText nameRange }
                let tag = Node.mk tagText tagRange tag
                elements.Add(T tag)

                ()
            | _ -> ()

        elements.ToArray()

let rec private reconstructHierarchy (text: Text) (flat: seq<Element>) : seq<Element> =
    seq {
        let mutable headStack: list<Node<Heading>> = []
        let mutable accChildren: list<Element> = []

        let mutable accStandalone: list<Element> = []

        let rec unwindHeadStack (newHead: Node<Heading>) : unit =
            match headStack with
            | [] ->
                accStandalone <- List.concat [ accChildren; accStandalone ]

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
                    Array.concat [ curHead.data.children; Array.ofList accChildren ]

                let curHead =
                    { curHead with data = { curHead.data with children = curHeadChildren } }

                accChildren <- []

                // Unwind further until we find a parent heading or none at all
                if curHead.data.level >= newHead.data.level then
                    let newScope =
                        { Start = curHead.data.scope.Start; End = newHead.data.scope.Start }

                    let curHead =
                        { curHead with data = { curHead.data with scope = newScope } }

                    accChildren <- [ H curHead ]
                    headStack <- rest
                    unwindHeadStack newHead
                else // cur.level < new.level; should stack the child
                    headStack <- newHead :: curHead :: rest

        for el in flat do
            match el with
            | YML _
            | T _
            | WL _
            | ML _
            | MLD _ ->
                match headStack with
                | _ :: _ -> accChildren <- el :: accChildren
                | [] -> yield el
            | H newHead -> unwindHeadStack newHead

        let guardHead =
            { level = -1
              title = Node.mkText "" (text.EndRange())
              scope = text.EndRange()
              children = [||] }
            |> Node.mk "" (text.EndRange())

        unwindHeadStack guardHead

        for child in accChildren do
            yield child

        for child in accStandalone do
            yield child
    }

let rec private sortElements (text: Text) (elements: array<Element>) : unit =
    for el in elements do
        match el with
        | H h -> sortElements text h.data.children
        | _ -> ()

    let elementStart el =
        let range = (Element.range el)

        let start = text.lineMap.FindOffset(range.Start)

        let end_ = text.lineMap.FindOffset(range.End)

        (start, end_)

    Array.sortInPlaceBy elementStart elements

let rec parseText (text: Text) : Cst =
    if String.IsNullOrEmpty text.content then
        [||]
    else
        let flatElements = Markdown.scrapeText text

        let hierarchicalElements = reconstructHierarchy text flatElements

        let elements = Array.ofSeq hierarchicalElements

        sortElements text elements
        elements
