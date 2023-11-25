module Marksman.Syms

open System

open Marksman.Misc
open Marksman.Names

type Ref =
    | SectionRef of doc: option<string> * section: option<string>
    | LinkDefRef of LinkLabel

    member this.CompactFormat() =
        match this with
        | SectionRef (doc, section) ->
            let doc = Option.defaultValue String.Empty doc

            let section =
                section
                |> Option.map (fun s -> "#" + s)
                |> Option.defaultValue String.Empty

            $"[[{doc}{section}]]"
        | LinkDefRef label -> $"[{label}]"

    member this.ToSection() =
        match this with
        | SectionRef (d, s) -> (d, s)
        | LinkDefRef _ -> failwith $"Ref.ToSection: {this} is not a Section"

    member this.ToLinkDef() =
        match this with
        | LinkDefRef label -> label
        | SectionRef _ -> failwith $"Ref.ToLinkDef: {this} is not a LinkDef"

module Ref =
    let hasExplicitDoc =
        function
        | SectionRef (Some _, _) -> true
        | SectionRef (None, _)
        | LinkDefRef _ -> false

    let asSection =
        function
        | SectionRef (doc, section) -> Some(doc, section)
        | LinkDefRef _ -> None

[<Struct>]
type Tag =
    | Tag of string

    member this.CompactFormat() = $"#{this}"

    member this.Name =
        let (Tag name) = this
        name

type Def =
    | Doc
    | Header of level: int * id: string
    | LinkDef of LinkLabel

    member this.CompactFormat() =
        match this with
        | Doc -> "Doc"
        | Header (l, h) -> $"H{l} {{{h}}}"
        | LinkDef ld -> $"[{ld}]:"

module Def =
    let isDoc =
        function
        | Def.Doc -> true
        | _ -> false

    let isTitle =
        function
        | Def.Header (level, _) when level <= 1 -> true
        | _ -> false

    let isHeader =
        function
        | Def.Header _ -> true
        | _ -> false

    let isHeaderWithId id =
        function
        | Def.Header (_, id') when id = id' -> true
        | _ -> false

    let isLinkDefWithLabel label =
        function
        | Def.LinkDef label' when label = label' -> true
        | _ -> false

    let asHeader =
        function
        | Header (level, id) -> Some(level, id)
        | _ -> None

[<RequireQualifiedAccess>]
type Sym =
    | Def of Def
    | Ref of Ref
    | Tag of Tag

    member this.CompactFormat() =
        match this with
        | Ref link -> link.CompactFormat()
        | Def def -> def.CompactFormat()
        | Tag tag -> tag.CompactFormat()

    member this.ToRef() =
        match this with
        | Ref ref -> ref
        | Def _
        | Tag _ -> failwith $"Sym.ToRef: {this} is not a Ref"

[<RequireQualifiedAccess>]
[<StructuredFormatDisplay("{CompactFormat}")>]
type Scope =
    | Doc of DocId
    | Global

    override this.ToString() =
        match this with
        | Doc docId -> $"{docId}"
        | Global -> "Global"

    member this.CompactFormat = this.ToString()

type ScopedSym = Scope * Sym

module Sym =
    let asRef =
        function
        | Sym.Ref ref -> Some ref
        | Sym.Def _
        | Sym.Tag _ -> None

    let asDef =
        function
        | Sym.Def def -> Some def
        | Sym.Ref _
        | Sym.Tag _ -> None

    let asTag =
        function
        | Sym.Tag tag -> Some tag
        | Sym.Ref _
        | Sym.Def _ -> None

    let isDoc sym = asDef sym |> Option.exists Def.isDoc

    let isTitle sym = asDef sym |> Option.exists Def.isTitle

    let isRefWithExplicitDoc sym = asRef sym |> Option.exists Ref.hasExplicitDoc

    let scoped (scope: Scope) (sym: Sym) = scope, sym

    let scopedToDoc (docId: DocId) (sym: Sym) = Scope.Doc docId, sym

    let allScopedToDoc (docId: DocId) (syms: seq<Sym>) = syms |> Seq.map (scopedToDoc docId)
