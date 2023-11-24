module Marksman.Sym

open System

open Marksman.Misc
open Marksman.Names

[<RequireQualifiedAccess>]
type Ref =
    | Section of doc: option<string> * section: option<string>
    | LinkDef of LinkLabel

    member this.CompactFormat() =
        match this with
        | Section (doc, section) ->
            let doc = Option.defaultValue String.Empty doc

            let section =
                section
                |> Option.map (fun s -> "#" + s)
                |> Option.defaultValue String.Empty

            $"[[{doc}{section}]]"
        | LinkDef label -> $"[{label}]"

[<Struct>]
type Tag =
    | Tag of string

    member this.CompactFormat() = $"#{this}"

[<RequireQualifiedAccess>]
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

    let isHeaderWithId id =
        function
        | Def.Header (_, id') when id = id' -> true
        | _ -> false

    let isLinkDefWithLabel label =
        function
        | Def.LinkDef label' when label = label' -> true
        | _ -> false

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
        | Sym.Ref link -> Some link
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

    let scoped (scope: Scope) (sym: Sym) = scope, sym

    let scopedToDoc (docId: DocId) (sym: Sym) = Scope.Doc docId, sym

    let allScopedToDoc (docId: DocId) (syms: seq<Sym>) = syms |> Seq.map (scopedToDoc docId)
