module Marksman.Doc

open System
open System.IO

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names
open Marksman.Text
open Marksman.Cst
open Marksman.Index
open Marksman.Paths

[<CustomEquality; CustomComparison>]
type Doc =
    { id: DocId
      version: option<int>
      text: Text
      cst: Cst
      index: Index }

    member this.RootPath = RootedRelPath.rootPath this.id.data

    member this.RelPath = RootedRelPath.relPathForced this.id.data

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
        let cst = Parser.parseText text
        let index = Index.ofCst cst

        { id = id; version = version; text = text; cst = cst; index = index }

    let id { id = id } = id
    let text doc = doc.text

    let withText newText doc =
        let newCst = Parser.parseText newText
        let newIndex = Index.ofCst newCst
        { doc with text = newText; cst = newCst; index = newIndex }


    let applyLspChange (change: DidChangeTextDocumentParams) (doc: Doc) : Doc =
        let newVersion = change.TextDocument.Version

        logger.trace (
            Log.setMessage "Processing text change"
            >> Log.addContext "uri" doc.id.uri
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
                    >> Log.addContext "uri" doc.id.uri
                    >> Log.addContext "currentVersion" curVersion
                    >> Log.addContext "newVersion" newVersion
                )
        | _ -> ()

        let newText = applyTextChange change.ContentChanges doc.text

        { withText newText doc with version = newVersion }

    let fromLsp (folderId: FolderId) (item: TextDocumentItem) : Doc =
        let path = LocalPath.ofUri item.Uri
        let id = UriWith.mkRooted folderId path
        let text = mkText item.Text

        mk id (Some item.Version) text

    let tryLoad (folderId: FolderId) (path: LocalPath) : option<Doc> =
        try
            let content =
                using (new StreamReader(LocalPath.toSystem path)) (fun f -> f.ReadToEnd())

            let text = mkText content

            let id = UriWith.mkRooted folderId path

            Some(mk id None text)
        with :? FileNotFoundException ->
            None

    let uri (doc: Doc) : DocumentUri = doc.id.uri

    let rootPath (doc: Doc) : RootPath = RootedRelPath.rootPath doc.id.data
    let path (doc: Doc) : AbsPath = RootedRelPath.toAbs doc.id.data

    let pathFromRoot (doc: Doc) = RootedRelPath.relPathForced doc.id.data

    let title (doc: Doc) : option<Node<Heading>> = Index.title doc.index

    let index (doc: Doc) : Index = doc.index

    let cst (doc: Doc) : Cst = doc.cst

    let name (doc: Doc) : string =
        match title doc with
        | Some { data = hd } -> Heading.name hd
        | None -> pathFromRoot doc |> RelPath.filenameStem

    let slug (doc: Doc) : Slug = name doc |> Slug.ofString

    let headings (doc: Doc) : seq<Node<Heading>> = Index.headings doc.index

    let linkDefs (doc: Doc) : array<Node<MdLinkDef>> = Index.linkDefs doc.index

    let linkDefMatching (sub: LinkLabel) (doc: Doc) : seq<Node<MdLinkDef>> =
        Index.filterLinkDefs (LinkLabel.isSubSequenceOf sub) doc.index

    let linkAtPos (pos: Position) (doc: Doc) : option<Element> = Index.linkAtPos pos doc.index

    let version (doc: Doc) : option<int> = doc.version
