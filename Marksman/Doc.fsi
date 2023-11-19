module Marksman.Doc

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names
open Marksman.Parser
open Marksman.Paths
open Marksman.Text
open Marksman.Cst
open Marksman.Index

[<Sealed>]
type Doc =
    member Id: DocId
    member RootPath: RootPath
    member RelPath: RelPath

    interface System.IComparable

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
    val title: Doc -> option<Node<Heading>>
    val name: Doc -> string
    val slug: Doc -> Slug
    val index: Doc -> Index

    val tryLoad: FolderId -> path: LocalPath -> option<Doc>

    val mk: DocId -> version: option<int> -> Text -> Doc
    val fromLsp: FolderId -> TextDocumentItem -> Doc
    val applyLspChange: DidChangeTextDocumentParams -> Doc -> Doc
