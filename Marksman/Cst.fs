module Marksman.Cst

open System

open Ionide.LanguageServerProtocol.Types

open FSharpPlus.Operators

open Marksman.Misc

type Node<'A> = { text: string; range: Range; data: 'A }

type TextNode = Node<unit>

module Node =
    let mk text range inner = { text = text; range = range; data = inner }
    let mkText text range : TextNode = mk text range ()
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

type Url =
    { url: Option<TextNode>
      anchor: Option<TextNode> }

    override this.ToString() : string =
        let parts =
            [ this.url
              |> Option.map Node.fmtText
              |> Option.map (fun x -> $"docUrl={x}")
              |> Option.toList
              this.anchor
              |> Option.map Node.fmtText
              |> Option.map (fun x -> $"anchor={x}")
              |> Option.toList ]
            |> List.concat

        String.Join(';', parts)

module Url =
    let anchor (x: Url) = x.anchor

    let ofUrlNode (url: TextNode) : Url =
        let offsetHash = url.text.IndexOf('#')

        if offsetHash < 0 then
            { url = Some url; anchor = None }
        else if offsetHash = 0 then
            let anchor =
                { url with
                    text = url.text.TrimStart('#')
                    range = { url.range with Start = url.range.Start.NextChar(1) } }

            { url = None; anchor = Some anchor }
        else
            let docText = url.text.Substring(0, offsetHash)

            let docRange =
                { url.range with
                    End = Position.Mk(url.range.Start.Line, url.range.Start.Character + offsetHash) }

            let docUrl = Node.mkText docText docRange

            let anchorText = url.text.Substring(offsetHash + 1)

            let anchorRange =
                { url.range with
                    Start =
                        Position.Mk(
                            url.range.Start.Line,
                            url.range.Start.Character + offsetHash + 1
                        ) }

            let anchor = Node.mkText anchorText anchorRange

            { url = Some docUrl; anchor = Some anchor }

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

type MdLinkDef = private { label: TextNode; url: TextNode; title: option<TextNode> }

module MdLinkDef =
    let mk label url title = { label = label; url = url; title = title }

    let normalizedLabel t = LinkLabel.ofString (Node.text t.label)

    let label t = t.label
    let labelContent t = Node.text t.label

    let titleContent t = t.title |> Option.map Node.text

    let urlContent t = Node.text t.url

    let fmt (mld: MdLinkDef) =
        let fmtLabel = Node.fmtText mld.label
        let fmtUrl = Node.fmtText mld.url

        let fmtTitle =
            mld.title |> Option.map Node.fmtText |> Option.defaultValue "∅"

        $"label={fmtLabel}; url={fmtUrl}; title={fmtTitle}"

    let name (mld: MdLinkDef) = mld.label |> Node.text

type Element =
    | H of Node<Heading>
    | WL of Node<WikiLink>
    | ML of Node<MdLink>
    | MLD of Node<MdLinkDef>
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
    | YML y -> Node.fmtText y

and private fmtHeading node =
    let inner = node.data

    let l1 =
        $"H{inner.level}: range={node.range.DebuggerDisplay}; scope={inner.scope.DebuggerDisplay}"

    let l2 = $"  text=`{node.text}`"

    let l3 =
        $"  title=`{inner.title.text}` @ {inner.title.range.DebuggerDisplay}"

    let rest = Array.map (indentFmt fmtElement) inner.children

    String.Join(Environment.NewLine, Array.concat [ [| l1; l2; l3 |]; rest ])

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
        | YML n -> n.range

    let rangeStart el = (range el).Start

    let text =
        function
        | H n -> n.text
        | WL n -> n.text
        | ML n -> n.text
        | MLD n -> n.text
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
        | ML _ -> false
        | YML _
        | H _
        | MLD _ -> true

    let isLink =
        function
        | WL _
        | ML _ -> true
        | H _
        | MLD _
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
                    | WL _
                    | ML _
                    | MLD _ -> ()
            }

        collect cst

    let elementAtPos (pos: Position) (cst: Cst) : option<Element> =
        elementsAll cst
        |> Seq.tryFind (fun el -> (Element.range el).ContainsInclusive(pos))
