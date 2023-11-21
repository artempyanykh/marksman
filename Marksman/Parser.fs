module Marksman.Parser

open System
open Ionide.LanguageServerProtocol.Types
open Markdig.Syntax

open Marksman.Cst
open Marksman.Misc
open Marksman.Names
open Marksman.Text

module Markdown =
    open Markdig
    open Markdig.Syntax.Inlines
    open Markdig.Parsers
    open Markdig.Helpers
    open Markdig.Extensions.Yaml

    type WikiLinkInline
        (
            text: string,
            doc: option<string * SourceSpan>,
            heading: option<string * SourceSpan>,
            title: option<string * SourceSpan>
        ) =
        inherit LeafInline()
        member val Text = text

        member val Doc = Option.map fst doc
        member val DocSpan = Option.map snd doc

        member val Heading = Option.map fst heading
        member val HeadingSpan = Option.map snd heading

        member val Title = Option.map fst title
        member val TitleSpan = Option.map snd title

    type TagInline(text: string) =
        inherit LeafInline()

        member val Text = text


    type TagsParser() as this =
        inherit InlineParser()

        do this.OpeningCharacters <- [| '#' |]

        override this.Match(processor, slice) =
            // tags should not be placed inside words, URLs etc.
            if (slice.PeekCharExtra -1).IsAlphaNumeric() then
                false
            else
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

    /// <summary>Match links of the form `[[doc#heading|title]]`, where at least one of `doc` and `#heading` must be present (`|title` may be omitted).</summary>
    type WikiLinkParser() as this =
        inherit InlineParser()

        do this.OpeningCharacters <- [| '[' |]

        override this.Match(processor, slice_) =
            let mutable docSpan: option<SourceSpan> = None
            let mutable headingSpan: option<SourceSpan> = None
            let mutable titleSpan: option<SourceSpan> = None

            let mutable slice = slice_ // this copy is necessary because byrefs cannot be captured

            // helper functions for finite state machine
            let updateSpanEnd offset (span: option<SourceSpan>) =
                span |> Option.map (fun s -> SourceSpan(s.Start, offset - 1))

            let isNotEscaped () = slice.PeekCharExtra(-1) <> '\\'
            let advance () = (slice.NextChar(), processor.GetSourcePosition(slice.Start))

            // state transition functions
            let parseEnd () =
                let c, _ = advance ()
                c = ']'

            let rec parseTitle () =
                let c, offset = advance ()

                if titleSpan.IsNone then
                    titleSpan <- SourceSpan(offset, -1) |> Some

                match c with
                | ']' when isNotEscaped () ->
                    titleSpan <- updateSpanEnd offset titleSpan
                    parseEnd ()
                | c when c.IsNewLineOrLineFeed() || c.IsZero() -> false
                | _ -> parseTitle ()

            let rec parseHeading () =
                let c, offset = advance ()

                if headingSpan.IsNone then
                    headingSpan <- SourceSpan(offset, -1) |> Some

                match c with
                | '|' when isNotEscaped () ->
                    headingSpan <- updateSpanEnd offset headingSpan
                    parseTitle ()
                | ']' when isNotEscaped () ->
                    headingSpan <- updateSpanEnd offset headingSpan
                    parseEnd ()
                | c when c.IsNewLineOrLineFeed() || c.IsZero() -> false
                | _ -> parseHeading ()

            let rec parseDoc offset =
                if docSpan.IsNone then
                    docSpan <- SourceSpan(offset, -1) |> Some

                let c, offset = advance ()

                match c with
                | '#' when isNotEscaped () ->
                    docSpan <- updateSpanEnd offset docSpan
                    parseHeading ()
                | '|' when isNotEscaped () ->
                    docSpan <- updateSpanEnd offset docSpan
                    parseTitle ()
                | ']' when isNotEscaped () ->
                    docSpan <- updateSpanEnd offset docSpan
                    parseEnd ()
                | c when c.IsNewLineOrLineFeed() || c.IsZero() -> false
                | _ -> parseDoc offset

            let parse () =
                let c, _ = advance ()

                if c <> '[' then
                    false
                else
                    let c, offset = advance ()

                    match c with
                    | '#' -> parseHeading ()
                    | '|' -> parseTitle ()
                    | ']' -> parseEnd ()
                    | _ -> parseDoc offset

            // do the parsing (run the finite state machine)
            let start = slice.Start
            let offsetStart = processor.GetSourcePosition(start)
            let hasParsedLink = parse ()
            slice_ <- slice // update output parameter to modified slice state

            if hasParsedLink then
                let offsetEnd = processor.GetSourcePosition(slice.Start)
                let text = slice.Text.Substring(start, offsetEnd - offsetStart + 1)

                let contentAndSpan (span: SourceSpan) =
                    let contentSliceStart = start + (span.Start - offsetStart)
                    let contentSliceLen = span.End - span.Start + 1
                    let content = slice.Text.Substring(contentSliceStart, contentSliceLen)

                    (content, span)

                let link =
                    WikiLinkInline(
                        text,
                        Option.map contentAndSpan docSpan,
                        Option.map contentAndSpan headingSpan,
                        Option.map contentAndSpan titleSpan
                    )

                link.Span <- SourceSpan(offsetStart, offsetEnd)
                processor.Inline <- link

            hasParsedLink

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

                let fullText = text.content.Substring(h.Span.Start, h.Span.Length)
                let title0 = fullText.TrimStart(' ', '#')
                let headingPrefixLen = fullText.Length - title0.Length
                let title = title0.TrimEnd(' ')
                let headingSuffixLen = title0.Length - title.Length

                let titleRange =
                    sourceSpanToRange
                        text
                        (SourceSpan(h.Span.Start + headingPrefixLen, h.Span.End - headingSuffixLen))

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
                        Node.mk doc (sourceSpanToRange text docSpan) (WikiEncoded.mkUnchecked doc)
                        |> Some
                    | _ -> None

                let heading =
                    match link.Heading, link.HeadingSpan with
                    | Some heading, Some headingSpan ->
                        Node.mk
                            heading
                            (sourceSpanToRange text headingSpan)
                            (WikiEncoded.mkUnchecked heading)
                        |> Some
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
                                Some(
                                    Node.mk
                                        url
                                        (sourceSpanToRange text urlSpan)
                                        (UrlEncoded.mkUnchecked url)
                                )

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

                let url =
                    Node.mk url (sourceSpanToRange text urlSpan) (UrlEncoded.mkUnchecked url)

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
