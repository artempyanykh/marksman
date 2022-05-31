module Marksman.DB

open Marksman.Domain
open Marksman.Misc
open Marksman.Parser

type DocDB

module DocDB =
    val ofDoc: Doc -> DocDB
    val path: DocDB -> PathUri
    val titles: DocDB -> list<Node<Heading>>
    val headingsBySlug: DocDB -> Map<Slug, list<Node<Heading>>>
    val tryFindHeadingBySlug: Slug -> DocDB -> option<Node<Heading>>
    val wikiLinks: DocDB -> list<Node<WikiLink>>

type FolderDB

module FolderDB =
    val ofFolder: Folder -> FolderDB
    val tryFindDocBySlug: Slug -> FolderDB -> option<DocDB>
    val findDocBySlug: Slug -> FolderDB -> DocDB
    val docsBySlug: FolderDB -> seq<Slug * DocDB>

type WorkspaceDB

module WorkspaceDB =
    val ofWorkspace: Workspace -> WorkspaceDB
