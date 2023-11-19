module Marksman.Folder

open Marksman.Misc
open Marksman.MMap
open Marksman.Config
open Marksman.Names
open Marksman.Paths
open Marksman.Doc

type Folder

module Folder =
    val id: Folder -> FolderId
    val rootPath: Folder -> RootPath

    val config: Folder -> option<Config>
    val configOrDefault: Folder -> Config
    val withConfig: option<Config> -> Folder -> Folder

    val docs: Folder -> seq<Doc>
    val docCount: Folder -> int

    val tryLoad: userConfig: option<Config> -> name: string -> FolderId -> option<Folder>

    val singleFile: doc: Doc -> config: option<Config> -> Folder
    val multiFile: name: string -> FolderId -> docs: seq<Doc> -> config: option<Config> -> Folder
    val isSingleFile: Folder -> bool

    val withDoc: Doc -> Folder -> Folder
    val withoutDoc: DocId -> Folder -> option<Folder>
    val closeDoc: DocId -> Folder -> option<Folder>

    val tryFindDocByPath: AbsPath -> Folder -> option<Doc>
    val tryFindDocByRelPath: RelPath -> Folder -> option<Doc>
    val tryFindDocByUrl: string -> Folder -> option<Doc>
    val findDocById: DocId -> Folder -> Doc
    val filterDocsBySlug: Slug -> Folder -> seq<Doc>
    val filterDocsByInternPath: InternPath -> Folder -> seq<Doc>

    val oracle: Folder -> Conn.Oracle
    val syms: Folder -> MMap<DocId, Conn.Sym>
