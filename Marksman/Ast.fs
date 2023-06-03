module Marksman.Ast

open System.Collections.Generic
open Marksman.Misc
open Marksman.Names

type Heading =
    { level: int
      text: string
      id: Slug }

    member this.CompactFormat() =
        let prefix = String.replicate this.level "#"
        $"{prefix} {this.text} {{{this.id.Raw}}}"

type WikiLink =
    { doc: option<WikiEncoded>
      heading: option<WikiEncoded> }

    member this.CompactFormat() =
        let doc = this.doc |> Option.map WikiEncoded.raw |> Option.defaultValue ""

        let heading =
            this.heading
            |> Option.map (fun x -> $"#{WikiEncoded.raw x}")
            |> Option.defaultValue ""

        $"[[{doc}{heading}]]"


// [text](url "title")
type MdLink =
    { text: string
      url: option<UrlEncoded>
      title: option<string> }

    member this.CompactFormat() =
        let url =
            this.url
            |> Option.map (fun x -> $"{UrlEncoded.raw x}")
            |> Option.defaultValue ""

        let title =
            this.title |> Option.map (fun x -> $" {x}") |> Option.defaultValue ""

        $"[{this.text}]({url}{title})"

type MdRef =
    // [text][dest]
    | Full of text: string * dest: string
    // [dest][]
    | Collapsed of dest: string
    // [label]
    | Shortcut of dest: string

    member this.CompactFormat() =
        match this with
        | Full (text, dest) -> $"[{text}][{dest}]"
        | Collapsed dest -> $"[{dest}][]"
        | Shortcut dest -> $"[{dest}]"

type MdLinkDef =
    { label: string
      url: UrlEncoded
      title: option<string> }

    member this.CompactFormat() =
        let title =
            Option.map (fun x -> $" \"{x}\"") this.title |> Option.defaultValue ""

        $"[{this.label}]: {UrlEncoded.raw this.url}{title}"

[<Struct>]
type Tag = Tag of string

[<RequireQualifiedAccess>]
type Element =
    | H of Heading
    | WL of WikiLink
    | ML of MdLink
    | MR of MdRef
    | MLD of MdLinkDef
    | T of Tag

    member this.CompactFormat() =
        match this with
        | Element.H heading -> heading.CompactFormat()
        | Element.WL wikiLink -> wikiLink.CompactFormat()
        | Element.ML mdLink -> mdLink.CompactFormat()
        | Element.MR mdRef -> mdRef.CompactFormat()
        | Element.MLD mdLinkDef -> mdLinkDef.CompactFormat()
        | Element.T (Tag tag) -> $"#{tag}"


type Ast = Ast of abs: IndexMap<Element> * concrete: IndexMap<Cst.Element>

module Ast =
    let private getNodeOptData nodeOpt =
        Option.map (fun ({ data = data }: Cst.Node<'A>) -> data) nodeOpt

    let private getNodeOptText nodeOpt =
        Option.map (fun ({ text = text }: Cst.Node<'A>) -> text) nodeOpt

    let elements (Ast (abs, _)) = abs.elements

    let tryFindMatchingAbstract (cel: Cst.Element) (Ast (absMap, concreteMap)) : option<Element> =
        match Map.tryFind cel concreteMap.revMap with
        | Some n -> Some absMap.elements[n]
        | None -> None

    let tryFindMatchingConcrete (ael: Element) (Ast (absMap, concreteMap)) : option<Cst.Element> =
        match Map.tryFind ael absMap.revMap with
        | Some n -> Some concreteMap.elements[n]
        | None -> None

    let ofCst (cst: Cst.Cst) : Ast =
        let rec go cst =
            seq {
                for el in cst do
                    match el with
                    | Cst.H { data = cHead } ->
                        let aHead =
                            Element.H
                                { level = cHead.level
                                  text = cHead.title.text
                                  id = Cst.Heading.slug cHead }

                        yield el, aHead
                    | Cst.WL { data = cWiki } ->
                        let aWiki =
                            Element.WL
                                { doc = getNodeOptData cWiki.doc
                                  heading = getNodeOptData cWiki.heading }

                        yield el, aWiki
                    | Cst.ML { data = mdLink } ->
                        match mdLink with
                        | Cst.MdLink.IL (text, url, title) ->
                            yield
                                el,
                                Element.ML
                                    { text = text.text
                                      url = getNodeOptData url
                                      title = getNodeOptText title }
                        | Cst.MdLink.RF (text, label) ->
                            yield el, Element.MR(Full(text.text, label.text))
                        | Cst.MdLink.RC label -> yield el, Element.MR(Collapsed(label.text))
                        | Cst.MdLink.RS label -> yield el, Element.MR(Shortcut(label.text))
                    | Cst.MLD { data = mdDef } ->
                        yield
                            el,
                            Element.MLD
                                { label = mdDef.label.text
                                  url = mdDef.url.data
                                  title = getNodeOptText mdDef.title }
                    | Cst.T { data = tag } -> yield el, Element.T(Tag tag.name.text)
                    | Cst.YML _ -> ()
            }

        let abs = ResizeArray<Element>()
        let a2n = Dictionary<Element, int>()
        let concrete = ResizeArray<Cst.Element>()
        let c2n = Dictionary<Cst.Element, int>()
        // Accumulate AST elements and mapping
        for cel, ael in go cst.elements do
            let n = abs.Count
            abs.Add(ael)
            a2n.Add(ael, n)
            concrete.Add(cel)
            c2n.Add(cel, n)
        // Freeze components of the AST
        let abs = abs.ToArray()
        let a2n = seq { for KeyValue (k, v) in a2n -> k, v } |> Map.ofSeq
        let concrete = concrete.ToArray()
        let c2n = seq { for KeyValue (k, v) in c2n -> k, v } |> Map.ofSeq
        Ast({ elements = abs; revMap = a2n }, { elements = concrete; revMap = c2n })
