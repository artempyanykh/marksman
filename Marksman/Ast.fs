module Marksman.Ast

open FSharpPlus.Operators

open Marksman.Misc
open Marksman.Names

type Heading =
    { level: int
      text: string
      id: Slug }

    member this.CompactFormat() =
        let prefix = String.replicate this.level "#"
        $"{prefix} {this.text} {{{this.id.Raw}}}"

    static member OfCst(cHead: Cst.Heading) : Heading =
        { level = cHead.level
          text = cHead.title.text
          id = Cst.Heading.slug cHead }

type WikiLink =
    { doc: option<string>
      heading: option<string> }

    member this.CompactFormat() =
        let doc = this.doc |> Option.defaultValue ""

        let heading =
            this.heading
            |> Option.map (fun x -> $"#{x}")
            |> Option.defaultValue ""

        $"[[{doc}{heading}]]"


// [text](url "title")
type MdLink =
    { text: string
      url: option<string>
      anchor: option<string> }

    member this.CompactFormat() =
        let url =
            this.url |> Option.map (fun x -> $"{x}") |> Option.defaultValue ""

        let anchor =
            this.anchor |> Option.map (fun x -> $"#{x}") |> Option.defaultValue ""

        $"[{this.text}]({url}{anchor})"

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

    member this.Dest =
        match this with
        | Full (_, dest)
        | Collapsed dest
        | Shortcut dest -> dest

    member this.DestLabel = LinkLabel.ofString this.Dest

type MdLinkDef =
    { label: string
      url: UrlEncoded }

    member this.CompactFormat() = $"[{this.label}]: {UrlEncoded.raw this.url}"

    static member OfCst(mdDef: Cst.MdLinkDef) : MdLinkDef =
        { label = mdDef.label.text; url = mdDef.url.data }

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

module Element =
    let asHeading =
        function
        | Element.H heading -> Some heading
        | _ -> None

    let asLinkDef =
        function
        | Element.MLD mld -> Some mld
        | _ -> None

    let private getNodeOptData nodeOpt =
        Option.map (fun ({ data = data }: Cst.Node<'A>) -> data) nodeOpt

    let private getNodeOptText nodeOpt =
        Option.map (fun ({ text = text }: Cst.Node<'A>) -> text) nodeOpt

    let ofCst (cel: Cst.Element) : option<Element> =
        match cel with
        | Cst.H { data = cHead } -> Heading.OfCst cHead |> Element.H |> Some
        | Cst.WL { data = cWiki } ->
            Element.WL
                { doc = getNodeOptData cWiki.doc |>> WikiEncoded.decode
                  heading = getNodeOptData cWiki.heading |>> WikiEncoded.decode }
            |> Some
        | Cst.ML { data = mdLink } ->
            match mdLink with
            | Cst.MdLink.IL (text, url, _) ->
                let urlNode = url |>> Cst.Url.ofUrlNode

                let url =
                    urlNode >>= (fun x -> x.url) |>> (fun x -> UrlEncoded.decode x.data)

                let anchor =
                    urlNode >>= (fun x -> x.anchor)
                    |>> (fun x -> UrlEncoded.decode x.data)

                Element.ML { text = text.text; url = url; anchor = anchor } |> Some
            | Cst.MdLink.RF (text, label) -> Element.MR(Full(text.text, label.text)) |> Some
            | Cst.MdLink.RC label -> Element.MR(Collapsed(label.text)) |> Some
            | Cst.MdLink.RS label -> Element.MR(Shortcut(label.text)) |> Some
        | Cst.MLD { data = mdDef } -> MdLinkDef.OfCst mdDef |> Element.MLD |> Some
        | Cst.T { data = tag } -> Element.T(Tag tag.name.text) |> Some
        | Cst.YML _ -> None

type Ast = { elements: Element[] }
