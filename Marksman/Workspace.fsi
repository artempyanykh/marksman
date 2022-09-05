module Marksman.Workspace

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Cst
open Marksman.Index
open Marksman.Text

type Doc

module Doc =
    val rootPath: Doc -> PathUri
    val path: Doc -> PathUri
    val pathFromRoot: Doc -> string
    val text: Doc -> Text
    val version: Doc -> option<int>
    val cst: Doc -> Cst
    val title: Doc -> option<Node<Heading>>
    val name: Doc -> string
    val index: Doc -> Index
    val uri: Doc -> DocumentUri

    val tryLoad: root: PathUri -> path: PathUri -> option<Doc>

    val mk: path: PathUri -> rootPath: PathUri -> version: option<int> -> Text -> Doc
    val fromLsp: root: PathUri -> TextDocumentItem -> Doc
    val applyLspChange: DidChangeTextDocumentParams -> Doc -> Doc

type Folder

module Folder =
    val keyPath: Folder -> PathUri
    val rootPath: Folder -> PathUri

    val docs: Folder -> seq<Doc>
    val docCount: Folder -> int

    val tryLoad: name: string -> root: PathUri -> option<Folder>

    val singleFile: Doc -> Folder
    val multiFile: name: string -> root: PathUri -> docs: Map<PathUri, Doc> -> Folder

    val withDoc: Doc -> Folder -> Folder
    val withoutDoc: PathUri -> Folder -> option<Folder>
    val closeDoc: PathUri -> Folder -> option<Folder>

    val tryFindDocByPath: PathUri -> Folder -> option<Doc>
    val tryFindDocByUrl: string -> Folder -> option<Doc>
    val filterDocsBySlug: Slug -> Folder -> seq<Doc>

type Workspace

module Workspace =
    val folders: Workspace -> seq<Folder>
    val docCount: Workspace -> int

    val withFolder: Folder -> Workspace -> Workspace
    val withFolders: seq<Folder> -> Workspace -> Workspace
    val ofFolders: seq<Folder> -> Workspace

    val withoutFolder: PathUri -> Workspace -> Workspace
    val withoutFolders: seq<PathUri> -> Workspace -> Workspace

    val tryFindFolderEnclosing: PathUri -> Workspace -> option<Folder>
