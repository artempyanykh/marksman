module Marksman.Ast

open System.Collections.Generic

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

type Ast = Ast of abs: IndexMap<Element> * concrete: IndexMap<Cst.Element>

module Ast =
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
                for cel in cst do
                    match Element.ofCst cel with
                    | Some ael -> yield cel, ael
                    | None -> ()
            }

        let abs = ResizeArray<Element>()
        let a2n = Dictionary<Element, int>()
        let concrete = ResizeArray<Cst.Element>()
        let c2n = Dictionary<Cst.Element, int>()
        // Accumulate AST elements and mapping
        for cel, ael in go (Cst.Cst.elements cst) do
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
