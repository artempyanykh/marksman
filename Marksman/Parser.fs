module Marksman.Parser

open System
open Ionide.LanguageServerProtocol.Types

open Markdig.Syntax
open MarkdigPatches
open Text
open Misc

type Node<'a> = { text: string; range: Range; data: 'a }

type TextNode = Node<unit>

module Node =
    let mk text range inner = { text = text; range = range; data = inner }
    let mk_text text range : TextNode = mk text range ()
    let text node = node.text
    let range node = node.range
    let data node = node.data
    let inner node = node.data
    let fmtText (node: TextNode) : string = $"{node.text} @ {node.range.DebuggerDisplay}"

[<RequireQualifiedAccess>]
type WikiLink = { doc: option<TextNode>; heading: option<TextNode> }

module WikiLink =
    let destDoc (dest: WikiLink) : option<string> = dest.doc |> Option.map Node.text

    let destHeading (dest: WikiLink) : option<string> = dest.heading |> Option.map Node.text

    let fmt (wl: WikiLink) : string =
        let lines = ResizeArray()

        wl.doc
        |> Option.iter (fun d -> $"doc={d.text}; {d.range.DebuggerDisplay}" |> lines.Add)

        wl.heading
        |> Option.iter (fun h -> $"head={h.text}; {h.range.DebuggerDisplay}" |> lines.Add)

        String.Join(Environment.NewLine, lines)

[<RequireQualifiedAccess>]
type MdLink =
    // inline
    | IL of label: TextNode * url: option<TextNode> * title: option<TextNode>
    // reference full
    | RF of text: TextNode * label: TextNode
    // reference collapsed
    | RC of label: TextNode
    // reference shortcut
    | RS of label: TextNode

module MdLink =
    let fmt (ml: MdLink) : string =
        match ml with
        | MdLink.IL (label, url, title) ->
            let fmtLabel = Node.fmtText label
            let fmtUrl = Option.map Node.fmtText url |> Option.defaultValue "∅"
            let fmtTitle = Option.map Node.fmtText title |> Option.defaultValue "∅"
            $"IL: label={fmtLabel}; url={fmtUrl}; title={fmtTitle}"
        | MdLink.RF (text, label) ->
            let fmtText = Node.fmtText text
            let fmtLabel = Node.fmtText label
            $"RF: text={fmtText}; label={fmtLabel}"
        | MdLink.RC label ->
            let fmtLabel = Node.fmtText label
            $"RC: label={fmtLabel}"
        | MdLink.RS label ->
            let fmtLabel = Node.fmtText label
            $"RS: label={fmtLabel}"

    let referenceLabel =
        function
        | MdLink.RF (_, label)
        | MdLink.RC label
        | MdLink.RS label -> Some label
        | MdLink.IL _ -> None

type MdLinkDef = { label: TextNode; url: TextNode; title: option<TextNode> }

module MdLinkDef =
    let fmt (mld: MdLinkDef) =
        let fmtLabel = Node.fmtText mld.label
        let fmtUrl = Node.fmtText mld.url

        let fmtTitle =
            mld.title |> Option.map Node.fmtText |> Option.defaultValue "∅"

        $"label={fmtLabel}; url={fmtUrl}; title={fmtTitle}"

type Element =
    | H of Node<Heading>
    | WL of Node<WikiLink>
    | ML of Node<MdLink>
    | MLD of Node<MdLinkDef>

and Heading =
    { level: int
      title: TextNode
      scope: Range
      children: array<Element> }

let rec private fmtElement =
    function
    | H h -> fmtHeading h
    | WL x -> fmtWikiLink x
    | ML l -> fmtMdLink l
    | MLD r -> fmtMdLinkDef r

and private fmtHeading node =
    let inner = node.data

    let l1 =
        $"H{inner.level}: range={node.range.DebuggerDisplay}; scope={inner.scope.DebuggerDisplay}"

    let l2 = $"  text=`{node.text}`"

    let rest = Array.map (indentFmt fmtElement) inner.children

    String.Join(Environment.NewLine, Array.concat [ [| l1; l2 |]; rest ])

and private fmtWikiLink node =
    let first = $"WL: {node.text}; {node.range.DebuggerDisplay}"
    let rest = (indentFmt WikiLink.fmt) node.data
    String.Join(Environment.NewLine, [ first; rest ])

and private fmtMdLink node =
    let first = $"ML: {node.text} @ {node.range.DebuggerDisplay}"
    let rest = (indentFmt MdLink.fmt) node.data
    String.Join(Environment.NewLine, [ first; rest ])

and private fmtMdLinkDef node =
    let first = $"MLD: {node.text} @ {node.range.DebuggerDisplay}"
    let rest = (indentFmt MdLinkDef.fmt) node.data
    String.Join(Environment.NewLine, [ first; rest ])

module Heading =
    let fmt = fmtHeading

    let name (heading: Heading) : string =
        (Node.text heading.title).TrimStart(' ', '#').TrimEnd(' ')

    let slug (heading: Heading) : Slug = name heading |> Slug.ofString

    let isTitle (heading: Heading) = heading.level <= 1

    let range (heading: Heading) : Range = heading.title.range

    let scope (heading: Heading) : Range = heading.scope

module Element =
    let fmt = fmtElement

    let range =
        function
        | H n -> n.range
        | WL n -> n.range
        | ML n -> n.range
        | MLD n -> n.range

    let text =
        function
        | H n -> n.text
        | WL n -> n.text
        | ML n -> n.text
        | MLD n -> n.text

    let asHeading =
        function
        | H h -> Some h
        | _ -> None

    let asWikiLink =
        function
        | WL ref -> Some ref
        | _ -> None

    let asLinkDef =
        function
        | MLD def -> Some def
        | _ -> None

    let pickHeadings (elements: array<Element>) : array<Node<Heading>> =
        elements |> Array.map asHeading |> Array.collect Option.toArray

module Markdown =
    open Markdig
    open Markdig.Syntax.Inlines
    open Markdig.Parsers
    open Markdig.Helpers

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
                    if current = '#'
                       && slice.PeekCharExtra(-1) <> '\\'
                       && offsetHashDelim.IsNone then
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
        let pipelineBuilder = MarkdownPipelineBuilder().UsePreciseSourceLocation()

        pipelineBuilder.InlineParsers.Insert(0, PatchedLinkInlineParser())
        pipelineBuilder.InlineParsers.Insert(0, WikiLinkParser())
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
                          title = Node.mk_text title titleRange
                          scope = range
                          children = [||] }

                elements.Add(H heading)
            | :? WikiLinkInline as link ->
                let doc =
                    match link.Doc, link.DocSpan with
                    | Some doc, Some docSpan ->
                        Node.mk_text doc (sourceSpanToRange text docSpan) |> Some
                    | _ -> None

                let heading =
                    match link.Heading, link.HeadingSpan with
                    | Some heading, Some headingSpan ->
                        Node.mk_text heading (sourceSpanToRange text headingSpan) |> Some
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
                        let label = Node.mk_text label (sourceSpanToRange text labelSpan)

                        let url =
                            if urlSpan.IsEmpty then
                                None
                            else
                                Some(Node.mk_text url (sourceSpanToRange text urlSpan))

                        let title =
                            if titleSpan.IsEmpty then
                                None
                            else
                                Some(Node.mk_text title (sourceSpanToRange text titleSpan))

                        let link =
                            MdLink.IL(label = label, url = url, title = title)
                            |> Node.mk linkText linkRange

                        elements.Add(ML link)
                    // Another hack: url span = label span => collapsed ref
                    else if urlSpan = labelSpan then
                        let label = Node.mk_text label (sourceSpanToRange text labelSpan)
                        let link = MdLink.RC label |> Node.mk linkText linkRange
                        elements.Add(ML link)
                    // The last remaining option is full reference
                    else
                        let text_ = Node.mk_text label (sourceSpanToRange text labelSpan)
                        let label = Node.mk_text url (sourceSpanToRange text urlSpan)
                        let link = MdLink.RF(text_, label) |> Node.mk linkText linkRange
                        elements.Add(ML link)
                else
                    let label = Node.mk_text label (sourceSpanToRange text labelSpan)
                    let link = MdLink.RS(label) |> Node.mk linkText linkRange
                    elements.Add(ML link)
            | :? LinkReferenceDefinition as linkDef ->
                let defRange = sourceSpanToRange text linkDef.Span

                let defText =
                    text.content.Substring(linkDef.Span.Start, linkDef.Span.Length)

                let label = linkDef.Label
                let labelSpan = linkDef.LabelSpan
                let label = Node.mk_text label (sourceSpanToRange text labelSpan)

                let url = linkDef.Url
                let urlSpan = linkDef.UrlSpan
                let url = Node.mk_text url (sourceSpanToRange text urlSpan)

                let title =
                    if linkDef.TitleSpan.IsEmpty then
                        None
                    else
                        let title = linkDef.Title
                        let titleSpan = linkDef.TitleSpan
                        Node.mk_text title (sourceSpanToRange text titleSpan) |> Some

                let def =
                    { label = label; url = url; title = title }
                    |> Node.mk defText defRange

                elements.Add(MLD def)

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
            | WL _
            | ML _
            | MLD _ ->
                match headStack with
                | _ :: _ -> accChildren <- el :: accChildren
                | [] -> yield el
            | H newHead -> unwindHeadStack newHead

        let guardHead =
            { level = -1
              title = Node.mk_text "" (text.EndRange())
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

let rec parseText (text: Text) : array<Element> =
    if String.IsNullOrEmpty text.content then
        [||]
    else
        let flatElements = Markdown.scrapeText text

        let hierarchicalElements = reconstructHierarchy text flatElements

        let elements = Array.ofSeq hierarchicalElements

        sortElements text elements
        elements
