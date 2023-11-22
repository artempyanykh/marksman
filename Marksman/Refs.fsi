module Marksman.Refs

open Ionide.LanguageServerProtocol.Types

open Marksman.Doc
open Marksman.Folder
open Marksman.Cst
open Marksman.Names

type InternNameNode = Node<InternName>

/// Unresolved reference.
[<RequireQualifiedAccess>]
type Uref =
    | Doc of InternNameNode
    | Heading of doc: option<InternNameNode> * heading: HeadingNode
    | LinkDef of TextNode

and HeadingNode =
    | Wiki of WikiEncodedNode
    | Url of UrlEncodedNode

    member DecodedText: string

module Uref =
    val ofElement: string array -> DocId -> Element -> Uref option

[<RequireQualifiedAccess>]
type FileLinkKind =
    | FilePath
    | FileName
    | FileStem
    | Title

type FileLink =
    { link: string
      kind: FileLinkKind
      dest: Doc }

module FileLink =
    val dest: FileLink -> Doc
    val filterMatchingDocs: Folder -> InternName -> seq<FileLink>
    val filterFuzzyMatchingDocs: Folder -> InternName -> seq<Doc>

type DocLink =
    | Explicit of FileLink
    | Implicit of Doc

module DocLink =
    val doc: DocLink -> Doc

[<RequireQualifiedAccess>]
type Dest =
    | Doc of FileLink
    | Heading of DocLink * Node<Heading>
    | LinkDef of Doc * Node<MdLinkDef>
    | Tag of Doc * Node<Tag>

module Dest =
    val doc: Dest -> Doc
    val range: Dest -> Range
    val scope: Dest -> Range
    val location: Dest -> Location

    val tryResolveUref: Uref -> Doc -> Folder -> seq<Dest>
    val findElementRefs: bool -> Folder -> Doc -> Element -> seq<Doc * Element * Dest array>
