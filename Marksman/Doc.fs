module Marksman.Doc

open System
open System.IO

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names
open Marksman.Text
open Marksman.Index
open Marksman.Paths
open Marksman.Parser

open Marksman.Cst

[<CustomEquality; CustomComparison>]
type Doc =
    { id: DocId
      version: option<int>
      text: Text
      structure: Structure
      index: Index }

    member this.RootPath = RootedRelPath.rootPath this.id.Path

    member this.RelPath = RootedRelPath.relPathForced this.id.Path

    member this.Id = this.id

    override this.Equals(obj) =
        match obj with
        | :? Doc as other -> this.id = other.id && this.version = other.version
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(hash this.id, hash this.version)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Doc as other ->
                match compare this.id other.id with
                | 0 -> compare this.version other.version
                | non0 -> non0
            | _ -> failwith $"Comparison with non-Doc type: {obj}"

module Doc =

    let logger = LogProvider.getLoggerByName "Doc"

    let mk id version text =
        let structure = Structure.ofText text
        let index = Index.ofCst structure.cst

        { id = id
          version = version
          text = text
          structure = structure
          index = index }

    let id { id = id } = id
    let text doc = doc.text

    let withText newText doc =
        let newStructure = Structure.ofText newText
        let newIndex = Index.ofCst newStructure.cst

        { doc with
            text = newText
            structure = newStructure
            index = newIndex }


    let applyLspChange (change: DidChangeTextDocumentParams) (doc: Doc) : Doc =
        let newVersion = change.TextDocument.Version

        logger.trace (
            Log.setMessage "Processing text change"
            >> Log.addContext "uri" doc.id.Uri
            >> Log.addContext "currentVersion" doc.version
            >> Log.addContext "newVersion" newVersion
        )

        // Sanity checking
        match newVersion, doc.version with
        | Some newVersion, Some curVersion when curVersion > 0 ->
            let expectedVersion = curVersion + change.ContentChanges.Length

            if expectedVersion <> newVersion then
                logger.warn (
                    Log.setMessage "Unexpected document version"
                    >> Log.addContext "uri" doc.id.Uri
                    >> Log.addContext "currentVersion" curVersion
                    >> Log.addContext "newVersion" newVersion
                )
        | _ -> ()

        let newText = applyTextChange change.ContentChanges doc.text

        { withText newText doc with version = newVersion }

    let fromLsp (folderId: FolderId) (item: TextDocumentItem) : Doc =
        let path = LocalPath.ofUri item.Uri
        let id = DocId(UriWith.mkRooted folderId path)
        let text = mkText item.Text

        mk id (Some item.Version) text

    let tryLoad (folderId: FolderId) (path: LocalPath) : option<Doc> =
        try
            let content =
                using (new StreamReader(LocalPath.toSystem path)) (fun f -> f.ReadToEnd())

            let text = mkText content

            let id = DocId(UriWith.mkRooted folderId path)

            Some(mk id None text)
        with :? FileNotFoundException ->
            None

    let uri (doc: Doc) : DocumentUri = doc.id.Uri

    let rootPath (doc: Doc) : RootPath = RootedRelPath.rootPath doc.id.Path
    let path (doc: Doc) : AbsPath = RootedRelPath.toAbs doc.id.Path

    let pathFromRoot (doc: Doc) = RootedRelPath.relPathForced doc.id.Path

    let title (doc: Doc) : option<Node<Heading>> = Index.title doc.index

    let index (doc: Doc) : Index = doc.index

    let structure (doc: Doc) : Structure = doc.structure

    let cst (doc: Doc) : Cst = doc.structure.cst

    let ast (doc: Doc) : Ast.Ast = doc.structure.ast

    let name (doc: Doc) : string =
        match title doc with
        | Some { data = hd } -> Heading.name hd
        | None -> pathFromRoot doc |> RelPath.filenameStem

    let slug (doc: Doc) : Slug = name doc |> Slug.ofString

    let version (doc: Doc) : option<int> = doc.version
