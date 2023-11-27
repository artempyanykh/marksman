module Marksman.Syms

open Marksman.Misc
open Marksman.Names
open Marksman.Paths


[<StructuredFormatDisplay("{AsString}")>]
type IntraRef =
    | IntraSection of section: Slug
    | IntraLinkDef of LinkLabel

    override this.ToString() =
        match this with
        | IntraSection section -> $"[[#{Slug.toString section}]]"
        | IntraLinkDef linkLabel -> $"[{linkLabel}]"

    member this.AsString = this.ToString()

[<StructuredFormatDisplay("{AsString}")>]
type CrossRef =
    | CrossDoc of doc: string
    | CrossSection of doc: string * section: Slug

    override this.ToString() =
        match this with
        | CrossDoc doc -> $"[[{doc}]]"
        | CrossSection (doc, section) -> $"[[{doc}#{Slug.toString section}]]"

    member this.AsString = this.ToString()

    member this.Doc =
        match this with
        | CrossDoc doc
        | CrossSection (doc, _) -> doc

[<StructuredFormatDisplay("{AsString}")>]
type Ref =
    | IntraRef of IntraRef
    | CrossRef of CrossRef

    override this.ToString() =
        match this with
        | IntraRef intraRef -> intraRef.ToString()
        | CrossRef crossRef -> crossRef.ToString()

    member this.AsString = this.ToString()

module Ref =
    let isIntra =
        function
        | IntraRef _ -> true
        | _ -> false

    let isCross =
        function
        | CrossRef _ -> true
        | _ -> false

    let trySection =
        function
        | CrossRef (CrossSection (_, section))
        | IntraRef (IntraSection section) -> Some section
        | _ -> None

[<Struct>]
[<StructuredFormatDisplay("{AsString}")>]
type Tag =
    | Tag of string

    override this.ToString() = $"#{this}"

    member this.AsString = this.ToString()

    member this.Name =
        let (Tag name) = this
        name

[<StructuredFormatDisplay("{AsString}")>]
type Def =
    | Doc
    | Header of level: int * id: string
    | LinkDef of LinkLabel

    override this.ToString() =
        match this with
        | Doc -> "Doc"
        | Header (l, h) -> $"H{l} {{{h}}}"
        | LinkDef ld -> $"[{ld}]:"

    member this.AsString = this.ToString()

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
[<StructuredFormatDisplay("{AsString}")>]
type Sym =
    | Def of Def
    | Ref of Ref
    | Tag of Tag

    override this.ToString() =
        match this with
        | Ref link -> link.ToString()
        | Def def -> def.ToString()
        | Tag tag -> tag.ToString()

    member this.AsString = this.ToString()

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

module ScopedSym =
    let asScopedRef =
        function
        | scope, Sym.Ref ref -> Some(scope, ref)
        | _ -> None

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

    let isRefWithExplicitDoc sym = asRef sym |> Option.exists Ref.isCross

    let scoped (scope: Scope) (sym: Sym) = scope, sym

    let scopedToDoc (docId: DocId) (sym: Sym) = Scope.Doc docId, sym

    let allScopedToDoc (docId: DocId) (syms: seq<Sym>) = syms |> Seq.map (scopedToDoc docId)
