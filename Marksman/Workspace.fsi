module Marksman.Workspace

open Marksman.Config
open Marksman.Paths
open Marksman.Names
open Marksman.Folder


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

    val tryFindFolderEnclosing: AbsPath -> Workspace -> option<Folder>
