module Marksman.Doc

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Config
open Marksman.Names
open Marksman.Structure
open Marksman.Paths
open Marksman.Text
open Marksman.Cst
open Marksman.Index
open Marksman.Syms

[<Sealed>]
type Doc =
    member Id: DocId
    member RootPath: RootPath
    member RelPath: RelPath
    member Structure: Structure
    member Index: Index

    interface System.IComparable
    interface System.IComparable<Doc>
    interface System.IEquatable<Doc>

module Doc =
    val id: Doc -> DocId
    val uri: Doc -> DocumentUri
    val rootPath: Doc -> RootPath
    val path: Doc -> AbsPath
    val pathFromRoot: Doc -> RelPath
    val text: Doc -> Text
    val version: Doc -> option<int>
    val structure: Doc -> Structure
    val cst: Doc -> Cst
    val ast: Doc -> Ast.Ast
    val syms: Doc -> seq<Sym>
    val symsDifference: Doc -> Doc -> Difference<Sym>
    val title: Doc -> option<Node<Heading>>
    val name: Doc -> string
    val slug: Doc -> Slug
    val index: Doc -> Index

    val tryLoad: ParserSettings -> FolderId -> path: LocalPath -> option<Doc>
    val mk: ParserSettings -> DocId -> version: option<int> -> Text -> Doc
    val fromLsp: ParserSettings -> FolderId -> TextDocumentItem -> Doc
    val applyLspChange: ParserSettings -> DidChangeTextDocumentParams -> Doc -> Doc
