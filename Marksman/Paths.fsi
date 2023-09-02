module Marksman.Paths

open Ionide.LanguageServerProtocol.Types

val systemPathToUriString: string -> DocumentUri

type AbsPath = AbsPath of string
type RelPath = RelPath of string

type LocalPath =
    | Abs of AbsPath
    | Rel of RelPath

module AbsPath =
    val ofUri: DocumentUri -> AbsPath
    val ofSystem: string -> AbsPath
    val toSystem: AbsPath -> string
    val toUri: AbsPath -> DocumentUri
    val append: AbsPath -> RelPath -> AbsPath
    val appendFile: AbsPath -> filename: string -> AbsPath
    val contains: outer: AbsPath -> inner: AbsPath -> bool
    val filename: AbsPath -> string
    val filenameStem: AbsPath -> string

module RelPath =
    val toSystem: RelPath -> string
    val filename: RelPath -> string
    val filenameStem: RelPath -> string
    val hasExtension: RelPath -> bool

module LocalPath =
    val tryOfSystem: string -> Option<LocalPath>
    val ofSystem: string -> LocalPath
    val ofUri: DocumentUri -> LocalPath
    val ofComponents: array<string> -> LocalPath
    val toSystem: LocalPath -> string
    val isAbsolute: LocalPath -> bool
    val asAbsolute: LocalPath -> AbsPath
    val isRelative: LocalPath -> bool
    val components: LocalPath -> array<string>
    val hasDotComponents: LocalPath -> bool
    val normalize: LocalPath -> LocalPath
    val appendFile: LocalPath -> filename: string -> LocalPath
    val combine: LocalPath -> LocalPath -> LocalPath
    val filename: LocalPath -> string
    val filenameStem: LocalPath -> string
    val directory: LocalPath -> LocalPath

type RootPath =
    | RootPath of AbsPath

    member Path: AbsPath

module RootPath =
    val ofPath: AbsPath -> RootPath
    val toLocal: RootPath -> LocalPath
    val toSystem: RootPath -> string
    val toUri: RootPath -> DocumentUri
    val append: RootPath -> RelPath -> AbsPath
    val appendFile: RootPath -> filename: string -> AbsPath
    val contains: RootPath -> LocalPath -> bool
    val filename: RootPath -> string
    val filenameStem: RootPath -> string

type RootedRelPath =
    private
        { root: RootPath
          path: option<RelPath> }

module RootedRelPath =
    val mk: RootPath -> LocalPath -> RootedRelPath
    val toAbs: RootedRelPath -> AbsPath
    val toSystem: RootedRelPath -> string
    val toUri: RootedRelPath -> DocumentUri
    val combine: RootedRelPath -> LocalPath -> option<RootedRelPath>
    val filename: RootedRelPath -> string
    val filenameStem: RootedRelPath -> string
    val directory: RootedRelPath -> option<RootedRelPath>

    val rootPath: RootedRelPath -> RootPath
    /// This method will give slightly misleading results for
    /// rooted paths where the root and the path are the same
    /// (e.g. a document inside a single-file folder), but
    /// it's the least evil compared to propagating an optional
    /// rel path to all use-sites.
    val relPathForced: RootedRelPath -> RelPath

type UriWith<'T> = { uri: DocumentUri; data: 'T }

module UriWith =
    val mkAbs: DocumentUri -> UriWith<AbsPath>
    val mkRoot: DocumentUri -> UriWith<RootPath>
    val mkRooted: UriWith<RootPath> -> LocalPath -> UriWith<RootedRelPath>
    val rootedRelToAbs: UriWith<RootedRelPath> -> UriWith<AbsPath>

/// Canonical document path is a path relative to the document's folder with
/// any known markdown extensions removed from the file name
type CanonDocPath = private CanonDocPath of string

module CanonDocPath =
    val mk: mdExts: seq<string> -> RelPath -> CanonDocPath
    val components: CanonDocPath -> list<string>
    val toRel: CanonDocPath -> RelPath
