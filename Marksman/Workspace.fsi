module Marksman.Workspace

open Ionide.LanguageServerProtocol.Types

open Marksman.Config
open Marksman.Cst
open Marksman.Index
open Marksman.Misc
open Marksman.Paths
open Marksman.Text

[<Sealed>]
type FolderId =
    interface System.IComparable

module FolderId =
    val ofPath: PathUri -> FolderId

type Doc

module Doc =
    val rootPath: Doc -> RootPath
    val path: Doc -> PathUri
    val pathFromRoot: Doc -> string
    val text: Doc -> Text
    val version: Doc -> option<int>
    val cst: Doc -> Cst
    val title: Doc -> option<Node<Heading>>
    val name: Doc -> string
    val slug: Doc -> Slug
    val index: Doc -> Index
    val uri: Doc -> DocumentUri

    val tryLoad: root: RootPath -> path: PathUri -> option<Doc>

    val mk: path: PathUri -> rootPath: RootPath -> version: option<int> -> Text -> Doc
    val fromLsp: root: RootPath -> TextDocumentItem -> Doc
    val applyLspChange: DidChangeTextDocumentParams -> Doc -> Doc

type Folder

module Folder =
    val id: Folder -> FolderId
    val rootPath: Folder -> RootPath

    val config: Folder -> option<Config>
    val configOrDefault: Folder -> Config
    val withConfig: option<Config> -> Folder -> Folder

    val docs: Folder -> seq<Doc>
    val docCount: Folder -> int

    val tryLoad: userConfig: option<Config> -> name: string -> root: RootPath -> option<Folder>

    val singleFile: doc: Doc -> config: option<Config> -> Folder
    val multiFile: name: string -> root: RootPath -> docs: Map<PathUri, Doc> -> config: option<Config> -> Folder
    val isSingleFile: Folder -> bool

    val withDoc: Doc -> Folder -> Folder
    val withoutDoc: PathUri -> Folder -> option<Folder>
    val closeDoc: PathUri -> Folder -> option<Folder>

    val tryFindDocByPath: PathUri -> Folder -> option<Doc>
    val tryFindDocByUrl: string -> Folder -> option<Doc>
    val filterDocsBySlug: Slug -> Folder -> seq<Doc>

type Workspace

module Workspace =
    val folders: Workspace -> seq<Folder>
    val folderCount: Workspace -> int
    val docCount: Workspace -> int
    val userConfig: Workspace -> option<Config>

    val ofFolders: userConfig: option<Config> -> seq<Folder> -> Workspace
    val withFolder: Folder -> Workspace -> Workspace
    val withFolders: seq<Folder> -> Workspace -> Workspace

    val withoutFolder: FolderId -> Workspace -> Workspace
    val withoutFolders: seq<FolderId> -> Workspace -> Workspace

    val tryFindFolderEnclosing: PathUri -> Workspace -> option<Folder>
