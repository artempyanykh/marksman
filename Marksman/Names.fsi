module Marksman.Names

open Ionide.LanguageServerProtocol.Types
open Marksman.Misc
open Marksman.Paths

type UrlEncoded = UrlEncoded of string

module UrlEncoded =
    val mkUnchecked: string -> UrlEncoded
    val encode: string -> UrlEncoded
    val decode: UrlEncoded -> string

type WikiEncoded = WikiEncoded of string

module WikiEncoded =
    val mkUnchecked: string -> WikiEncoded
    val encode: string -> WikiEncoded
    val decode: WikiEncoded -> string
    val raw: WikiEncoded -> string

type FolderId = UriWith<RootPath>

module FolderId =
    val ofUri: DocumentUri -> FolderId

type DocId = UriWith<RootedRelPath>

type InternName = { src: DocId; name: string }

type InternPath =
    | ExactAbs of RootedRelPath
    | ExactRel of src: DocId * path: RootedRelPath
    | Approx of RelPath

module InternPath =
    val toRel: InternPath -> RelPath

module InternName =
    val mkUnchecked: DocId -> string -> InternName
    val mkChecked: exts: seq<string> -> DocId -> name: string -> option<InternName>
    val name: InternName -> string
    val slug: InternName -> Slug
    val src: InternName -> DocId
    val tryAsPath: InternName -> option<InternPath>
    val asPath: InternName -> InternPath
