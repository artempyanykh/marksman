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
open Marksman.Structure

open Marksman.Cst

[<CustomEquality; CustomComparison>]
type Doc = {
    id: DocId
    version: option<int>
    text: Text
    structure: Structure
    index: Index
} with

    member this.RootPath = RootedRelPath.rootPath this.id.Path

    member this.RelPath = RootedRelPath.relPathForced this.id.Path

    member this.Id = this.id

    member this.Structure = this.structure

    member this.Index = this.index

    interface IEquatable<Doc> with
        member this.Equals(other) = this.id = other.id && this.text = other.text

    override this.Equals(obj) =
        match obj with
        | :? Doc as other -> (this :> IEquatable<_>).Equals(other)
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(hash this.id, hash this.text)

    interface IComparable<Doc> with
        member this.CompareTo(other) =
            match compare this.id other.id with
            | 0 -> compare this.text other.text
            | non0 -> non0

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Doc as other -> (this :> IComparable<_>).CompareTo(other)
            | _ -> failwith $"Comparison with non-Doc type: {obj}"

exception DocumentError of doc: RootedRelPath * cause: exn with
    override this.Message =
        $"Error while processing {RootedRelPath.filename this.doc}{Environment.NewLine}{this.cause.Message}"


module Doc =
    open Marksman.Syms

    let logger = LogProvider.getLoggerByName "Doc"

    let mk parserSettings id version text =
        try
            let structure = Parser.parse parserSettings text
            let index = Index.ofCst (Structure.concreteElements structure)

            {
                id = id
                version = version
                text = text
                structure = structure
                index = index
            }
        with exn ->
            raise (DocumentError(id.Path, exn))

    let id { id = id } = id
    let text doc = doc.text

    let withText config newText doc =
        let newStructure = Parser.parse config newText
        let newIndex = Index.ofCst (Structure.concreteElements newStructure)

        {
            doc with
                text = newText
                structure = newStructure
                index = newIndex
        }


    let applyLspChange parserSettings (change: DidChangeTextDocumentParams) (doc: Doc) : Doc =
        let newVersion = change.TextDocument.Version

        logger.trace (
            Log.setMessage "Processing text change"
            >> Log.addContext "uri" doc.id.Uri
            >> Log.addContext "currentVersion" doc.version
            >> Log.addContext "newVersion" newVersion
        )

        let newText = applyTextChange change.ContentChanges doc.text

        { withText parserSettings newText doc with version = Some newVersion }

    let fromLsp parserSettings (folderId: FolderId) (item: TextDocumentItem) : Doc =
        let path = LocalPath.ofUri item.Uri
        let id = DocId(UriWith.mkRooted folderId path)
        let text = mkText item.Text

        mk parserSettings id (Some item.Version) text

    let tryLoad parserSettings (folderId: FolderId) (path: LocalPath) : option<Doc> =
        try
            let content =
                using (new StreamReader(LocalPath.toSystem path)) (fun f -> f.ReadToEnd())

            let text = mkText content

            let id = DocId(UriWith.mkRooted folderId path)

            Some(mk parserSettings id None text)
        with :? FileNotFoundException ->
            None

    let uri (doc: Doc) : DocumentUri = doc.id.Uri

    let rootPath (doc: Doc) : RootPath = RootedRelPath.rootPath doc.id.Path
    let path (doc: Doc) : AbsPath = RootedRelPath.toAbs doc.id.Path

    let pathFromRoot (doc: Doc) = RootedRelPath.relPathForced doc.id.Path

    let title (doc: Doc) : option<Node<Heading>> = Index.title doc.index

    let index (doc: Doc) : Index = doc.index

    let structure (doc: Doc) : Structure = doc.structure

    let cst (doc: Doc) : Cst = doc.structure.Cst

    let ast (doc: Doc) : Ast.Ast = doc.structure.Ast

    let name (doc: Doc) : string =
        match title doc with
        | Some { data = hd } -> Heading.name hd
        | None -> pathFromRoot doc |> RelPath.filenameStem

    let slug (doc: Doc) : Slug = name doc |> Slug.ofString

    let version (doc: Doc) : option<int> = doc.version

    let syms (doc: Doc) : seq<Sym> =
        seq {
            for s in doc.structure.Symbols do
                // HACK. Generate a CrossDoc sym for every CrossSection sym
                // so that the difference of symbols is consistent with what
                // Conn expects in terms of dependencies between symbols.
                match s with
                | Syms.Sym.Ref(CrossRef(CrossSection(docName, _))) ->
                    yield Syms.Sym.Ref(CrossRef(CrossDoc docName))
                | _ -> ()

                yield s
        }

    let symsDifference (beforeDoc: Doc) (afterDoc: Doc) : Difference<Sym> =
        Difference.mk (syms beforeDoc) (syms afterDoc)
