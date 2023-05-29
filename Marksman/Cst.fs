module Marksman.Cst

open System

open FSharpPlus.Operators
open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names

type Node<'A> = { text: string; range: Range; data: 'A }

type TextNode = Node<unit>
type UrlEncodedNode = Node<UrlEncoded>
type WikiEncodedNode = Node<WikiEncoded>

module Node =
    let mk text range inner = { text = text; range = range; data = inner }
    let mkText text range : TextNode = mk text range ()
    let asText node = mkText node.text node.range
    let text node = node.text
    let textOpt nodeOpt def = Option.map text nodeOpt |> Option.defaultValue def
    let range node = node.range
    let data node = node.data
    let inner node = node.data
    let fmtText (node: TextNode) : string = $"{node.text} @ {node.range}"
    let fmtUrl (node: UrlEncodedNode) : string = $"{node.text} @ {node.range}"
    let fmtWiki (node: WikiEncodedNode) : string = $"{node.text} @ {node.range}"

    let fmtOptText (node: option<TextNode>) : string = fmtOption fmtText node
    let fmtOptUrl (node: option<UrlEncodedNode>) : string = fmtOption fmtUrl node
    let fmtOptWiki (node: option<WikiEncodedNode>) : string = fmtOption fmtWiki node

[<RequireQualifiedAccess>]
type WikiLink = { doc: option<WikiEncodedNode>; heading: option<WikiEncodedNode> }

module WikiLink =
    let destDoc (dest: WikiLink) : option<WikiEncoded> = dest.doc |> Option.map Node.data

    let destHeading (dest: WikiLink) : option<WikiEncoded> = dest.heading |> Option.map Node.data

    let fmt (wl: WikiLink) : string =
        let lines = ResizeArray()

        wl.doc
        |> Option.iter (fun d -> $"doc={d.text}; {d.range}" |> lines.Add)

        wl.heading
        |> Option.iter (fun h -> $"head={h.text}; {h.range}" |> lines.Add)

        String.Join(Environment.NewLine, lines)

    let render
        (doc: option<WikiEncoded>)
        (heading: option<WikiEncoded>)
        (includeBraces: bool)
        : string =
        let docText = doc |> Option.map WikiEncoded.raw |> Option.defaultValue ""

        let renderHeading h = "#" + (WikiEncoded.raw h)

        let headingText =
            heading
            |> Option.map renderHeading

            |> Option.defaultValue ""

        let linkText = $"{docText}{headingText}"
        if includeBraces then $"[[{linkText}]]" else linkText

    let contentRange ({ doc = doc; heading = heading }: WikiLink) =
        match Option.map Node.range doc, Option.map Node.range heading with
        | None, None -> None
        | Some _ as range, None -> range
        | None, (Some _ as range) -> range
        | Some docRange, Some headingRange ->
            Some { Start = docRange.Start; End = headingRange.End }

[<RequireQualifiedAccess>]
type MdLink =
    // inline
    | IL of text: TextNode * url: option<UrlEncodedNode> * title: option<TextNode>
    // reference full
    | RF of text: TextNode * label: TextNode
    // reference collapsed
    | RC of label: TextNode
    // reference shortcut
    | RS of label: TextNode

type Url<'T> =
    { url: Option<Node<'T>>
      anchor: Option<Node<'T>> }

    override this.ToString() : string =
        let parts =
            [ this.url
              |> Option.map (fun x -> x.text, x.range)
              |> Option.map (fun (text, range) -> $"docUrl={text} @ {range}")
              |> Option.toList
              this.anchor
              |> Option.map (fun x -> x.text, x.range)
              |> Option.map (fun (text, range) -> $"anchor={text} @ {range}")
              |> Option.toList ]
            |> List.concat

        String.Join(';', parts)

module Url =
    let anchor (x: Url<'T>) = x.anchor

    let ofTextNode (coder: string -> 'T) (url: TextNode) : Url<'T> =
        let offsetHash = url.text.IndexOf('#')

        if offsetHash < 0 then
            let url = Node.mk url.text url.range (coder url.text)
            { url = Some url; anchor = None }
        else if offsetHash = 0 then
            let text = url.text.TrimStart('#')
            let range = { url.range with Start = url.range.Start.NextChar(1) }
            let anchor = Node.mk text range (coder text)
            { url = None; anchor = Some anchor }
        else
            let docText = url.text.Substring(0, offsetHash)

            let docRange =
                { url.range with
                    End = Position.Mk(url.range.Start.Line, url.range.Start.Character + offsetHash) }

            let docUrl = Node.mk docText docRange (coder docText)

            let anchorText = url.text.Substring(offsetHash + 1)

            let anchorRange =
                { url.range with
                    Start =
                        Position.Mk(
                            url.range.Start.Line,
                            url.range.Start.Character + offsetHash + 1
                        ) }

            let anchor = Node.mk anchorText anchorRange (coder anchorText)

            { url = Some docUrl; anchor = Some anchor }

    let ofUrlNode (url: UrlEncodedNode) : Url<UrlEncoded> =
        ofTextNode UrlEncoded.mkUnchecked (Node.asText url)

module MdLink =
    let fmt (ml: MdLink) : string =
        match ml with
        | MdLink.IL (label, url, title) ->
            let fmtLabel = Node.fmtText label
            let fmtUrl = Option.map Node.fmtUrl url |> Option.defaultValue "∅"
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

    let renderInline text path anchor =
        let text = text |> Option.defaultValue String.Empty
        let path = path |> Option.defaultValue String.Empty

        let anchor =
            anchor
            |> Option.map (fun x -> "#" + x)
            |> Option.defaultValue String.Empty

        $"[{text}]({path}{anchor})"

type MdLinkDef =
    private
        { label: TextNode
          url: UrlEncodedNode
          title: option<TextNode> }

module MdLinkDef =
    let mk label url title = { label = label; url = url; title = title }

    let normalizedLabel t = LinkLabel.ofString (Node.text t.label)

    let label t = t.label
    let labelContent t = Node.text t.label

    let titleContent t = t.title |> Option.map Node.text

    let urlContent t = Node.text t.url

    let fmt (mld: MdLinkDef) =
        let fmtLabel = Node.fmtText mld.label
        let fmtUrl = Node.fmtUrl mld.url

        let fmtTitle =
            mld.title |> Option.map Node.fmtText |> Option.defaultValue "∅"

        $"label={fmtLabel}; url={fmtUrl}; title={fmtTitle}"

    let name (mld: MdLinkDef) = mld.label |> Node.text

type Tag = { name: TextNode }

module Tag =
    let fmt (t: Tag) = $"name={t.name.text}; range={t.name.range}"

type Element =
    | H of Node<Heading>
    | WL of Node<WikiLink>
    | ML of Node<MdLink>
    | MLD of Node<MdLinkDef>
    | T of Node<Tag>
    | YML of TextNode

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
    | T t -> fmtTag t
    | YML y -> Node.fmtText y

and private fmtHeading node =
    let inner = node.data

    let l1 = $"H{inner.level}: range={node.range}; scope={inner.scope}"

    let l2 = $"  text=`{node.text}`"

    let l3 = $"  title=`{inner.title.text}` @ {inner.title.range}"

    let rest = Array.map (indentFmt fmtElement) inner.children

    String.Join(Environment.NewLine, Array.concat [ [| l1; l2; l3 |]; rest ])

and private fmtWikiLink node =
    let first = $"WL: {node.text}; {node.range}"
    let rest = (indentFmt WikiLink.fmt) node.data
    String.Join(Environment.NewLine, [ first; rest ])

and private fmtMdLink node =
    let first = $"ML: {node.text} @ {node.range}"
    let rest = (indentFmt MdLink.fmt) node.data
    String.Join(Environment.NewLine, [ first; rest ])

and private fmtMdLinkDef node =
    let first = $"MLD: {node.text} @ {node.range}"
    let rest = (indentFmt MdLinkDef.fmt) node.data
    String.Join(Environment.NewLine, [ first; rest ])

and private fmtTag node = $"T: {Tag.fmt node.data} @ {node.range}"

module Heading =
    let fmt = fmtHeading

    let name (heading: Heading) : string = Node.text heading.title

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
        | T n -> n.range
        | YML n -> n.range

    let rangeStart el = (range el).Start

    let text =
        function
        | H n -> n.text
        | WL n -> n.text
        | ML n -> n.text
        | MLD n -> n.text
        | T n -> n.text
        | YML n -> n.text

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

    let isDecl =
        function
        | WL _
        | ML _
        | T _ -> false
        | YML _
        | H _
        | MLD _ -> true

    let isLink =
        function
        | WL _
        | ML _ -> true
        | H _
        | MLD _
        | T _
        | YML _ -> false

    let isTitle el =
        asHeading el |>> Node.data |>> Heading.isTitle
        |> Option.defaultValue false

type Cst = array<Element>

module Cst =
    let elementsAll (cst: Cst) : seq<Element> =
        let rec collect els =
            seq {
                for el in els do
                    yield el

                    match el with
                    | H h -> yield! collect h.data.children
                    | YML _
                    | T _
                    | WL _
                    | ML _
                    | MLD _ -> ()
            }

        collect cst

    let elementAtPos (pos: Position) (cst: Cst) : option<Element> =
        elementsAll cst
        |> Seq.tryFind (fun el -> (Element.range el).ContainsInclusive(pos))
