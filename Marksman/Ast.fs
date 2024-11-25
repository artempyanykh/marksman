module Marksman.Ast

open Marksman.Misc
open Marksman.Names
open Marksman.Syms

type Heading = {
    level: int
    text: string
    id: Slug
} with

    member this.CompactFormat() =
        let prefix = String.replicate this.level "#"
        $"{prefix} {this.text} {{{this.id.Raw}}}"

type WikiLink = {
    doc: option<string>
    heading: option<string>
} with

    member this.CompactFormat() =
        let doc = this.doc |> Option.defaultValue ""

        let heading =
            this.heading
            |> Option.map (fun x -> $"#{x}")
            |> Option.defaultValue ""

        $"[[{doc}{heading}]]"


// [text](url "title")
type MdLink = {
    text: string
    url: option<string>
    anchor: option<string>
} with

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
        | Full(text, dest) -> $"[{text}][{dest}]"
        | Collapsed dest -> $"[{dest}][]"
        | Shortcut dest -> $"[{dest}]"

    member this.Dest =
        match this with
        | Full(_, dest)
        | Collapsed dest
        | Shortcut dest -> dest

    member this.DestLabel = LinkLabel.ofString this.Dest

type MdLinkDef = {
    label: string
    url: UrlEncoded
} with

    member this.Label = LinkLabel.ofString this.label
    member this.CompactFormat() = $"[{this.label}]: {UrlEncoded.raw this.url}"

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
        | Element.T(Tag tag) -> $"#{tag}"

module Element =
    let asHeading =
        function
        | Element.H heading -> Some heading
        | _ -> None

    let asLinkDef =
        function
        | Element.MLD mld -> Some mld
        | _ -> None

    // TODO: instead of checking for 'all whitespace' symbols all the time, create smart constructors
    let toSym (exts: seq<string>) (el: Element) : option<Sym> =
        match el with
        | Element.H { level = level; id = id } ->
            if Slug.isEmpty id then
                None
            else
                let id = Slug.toString id

                // TODO: make this configurable
                if level <= 1 then
                    Syms.Sym.Def(Title(id)) |> Some
                else
                    Syms.Sym.Def(Header(level, id)) |> Some
        // Wiki-links mapping
        | Element.WL { doc = None; heading = None } -> None
        | Element.WL { doc = Some doc; heading = None } ->
            if doc.IsWhitespace() then
                None
            else
                Syms.Sym.Ref(Ref.CrossRef(CrossDoc doc)) |> Some
        | Element.WL { doc = Some doc; heading = Some heading } ->
            if heading.IsWhitespace() then
                None
            else
                Syms.Sym.Ref(Ref.CrossRef(CrossSection(doc, Slug.ofString heading)))
                |> Some
        | Element.WL { doc = None; heading = Some heading } ->
            if heading.IsWhitespace() then
                None
            else
                Syms.Sym.Ref(Ref.IntraRef(IntraSection <| Slug.ofString heading))
                |> Some
        // Markdown links mapping
        | Element.ML { url = url; anchor = anchor } ->
            let urlIsRef =
                url |> Option.map (fun url -> url, isPotentiallyInternalRef exts url)

            match urlIsRef, anchor with
            | None, None -> None
            | None, Some anchor ->
                if anchor.IsWhitespace() then
                    None
                else
                    Some(Syms.Sym.Ref(IntraRef(IntraSection <| Slug.ofString anchor)))
            | Some(_, false), _ -> None
            | Some(url, true), None ->
                if url.IsWhitespace() then
                    None
                else
                    Some(Syms.Sym.Ref(CrossRef(CrossDoc url)))
            | Some(url, true), Some anchor ->
                if url.IsWhitespace() || anchor.IsWhitespace() then
                    None
                else
                    Some(Syms.Sym.Ref(CrossRef(CrossSection(url, Slug.ofString anchor))))
        // The rest
        | Element.MR mdRef -> Some(Syms.Sym.Ref(IntraRef(IntraLinkDef mdRef.DestLabel)))
        | Element.MLD mdLinkDef -> Some(Syms.Sym.Def(LinkDef(mdLinkDef.Label)))
        | Element.T(Tag tag) -> Some(Syms.Sym.Tag(Syms.Tag tag))

type Ast = { elements: Element[] }
