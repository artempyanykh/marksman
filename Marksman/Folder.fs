module Marksman.Folder

open System
open System.IO

open Ionide.LanguageServerProtocol.Logging

open Marksman.Structure
open Marksman.GitIgnore
open Marksman.SuffixTree
open Marksman.Config
open Marksman.Doc
open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.MMap
open Marksman.Syms

type MultiFile =
    { name: string
      root: FolderId
      docs: Map<CanonDocPath, Doc>
      config: option<Config> }

    member this.RootPath = this.root.data


type SingleFile = { doc: Doc; config: option<Config> }

type FolderData =
    | MultiFile of MultiFile
    | SingleFile of SingleFile

module FolderData =
    let config =
        function
        | SingleFile { config = config }
        | MultiFile { config = config } -> config

    let configOrDefault = config >> Config.orDefault

    let docs data =
        match data with
        | SingleFile { doc = doc } -> Seq.singleton doc
        | MultiFile { docs = docs } -> Map.values docs


    let tryFindDocByRelPath (path: RelPath) data : option<Doc> =
        match data with
        | SingleFile { doc = doc } ->
            Some doc
            |> Option.filter (fun x ->
                let sysPath = ((Doc.path x) |> AbsPath.filenameStem)
                sysPath.EndsWith(path |> RelPath.filenameStem))
        | MultiFile { docs = docs } ->
            let canonPath =
                path
                |> CanonDocPath.mk ((configOrDefault data).CoreMarkdownFileExtensions())

            Map.tryFind canonPath docs

    let tryFindDocByPath (uri: AbsPath) data : option<Doc> =
        match data with
        | SingleFile { doc = doc } -> Some doc |> Option.filter (fun x -> Doc.path x = uri)
        | MultiFile { root = root; docs = docs } ->
            let canonPath =
                RootedRelPath.mk root.data (Abs uri)
                |> RootedRelPath.relPathForced
                |> CanonDocPath.mk ((configOrDefault data).CoreMarkdownFileExtensions())

            Map.tryFind canonPath docs

    let tryFindDocById (id: DocId) data : option<Doc> =
        let docRelPath = id.Path |> RootedRelPath.relPathForced
        tryFindDocByRelPath docRelPath data

    let findDocById (id: DocId) data : Doc =
        tryFindDocById id data
        |> Option.defaultWith (fun () -> failwith $"Expected doc could not be found: {id.Uri}")

    let syms (data: FolderData) =
        let mutable mapping = MMap.empty

        for doc in docs data do
            for sym in Doc.syms doc do
                mapping <- MMap.add (Doc.id doc) sym mapping

        mapping

type FolderLookup =
    { docsBySlug: Map<Slug, Set<Doc>>
      docsByPath: SuffixTree<CanonDocPath, Doc>
      config: option<Config> }

module FolderLookup =
    let ofData (data: FolderData) =
        let config = FolderData.config data

        let mdExt = (FolderData.configOrDefault data).CoreMarkdownFileExtensions()

        match data with
        | SingleFile data ->
            let bySlug = Map.ofList [ Doc.slug data.doc, Set.ofList [ data.doc ] ]

            let path = Doc.pathFromRoot data.doc |> CanonDocPath.mk mdExt
            let byPath = SuffixTree.ofSeq CanonDocPath.components [ path, data.doc ]

            { docsBySlug = bySlug; docsByPath = byPath; config = config }
        | MultiFile data ->
            let bySlug =
                Map.toSeq data.docs
                |> Seq.map snd
                |> Seq.groupBy Doc.slug
                |> Seq.map (fun (slug, docs) -> slug, Set.ofSeq docs)
                |> Map.ofSeq

            let byPath =
                Map.toSeq data.docs |> SuffixTree.ofSeq CanonDocPath.components

            { docsBySlug = bySlug; docsByPath = byPath; config = config }

    let withoutDoc (doc: Doc) (lookup: FolderLookup) =
        let slug = Doc.slug doc

        let docPath =
            Doc.pathFromRoot doc
            |> CanonDocPath.mk ((Config.orDefault lookup.config).CoreMarkdownFileExtensions())

        let updateBySlug =
            function
            | None -> None
            | Some docs -> Set.remove doc docs |> Some

        let bySlug = Map.change slug updateBySlug lookup.docsBySlug
        let byPath = SuffixTree.remove docPath lookup.docsByPath
        { docsBySlug = bySlug; docsByPath = byPath; config = lookup.config }

    let withDoc (doc: Doc) (lookup: FolderLookup) =
        let slug = Doc.slug doc

        let docPath =
            Doc.pathFromRoot doc
            |> CanonDocPath.mk ((Config.orDefault lookup.config).CoreMarkdownFileExtensions())

        let updateBySlug =
            function
            | None -> Some(Set.singleton doc)
            | Some docs -> Set.add doc docs |> Some

        let bySlug = Map.change slug updateBySlug lookup.docsBySlug
        let byPath = SuffixTree.add docPath doc lookup.docsByPath
        { docsBySlug = bySlug; docsByPath = byPath; config = lookup.config }

    /// Find document matching a slug.
    let filterDocsBySlug (slug: Slug) (lookup: FolderLookup) : seq<Doc> =
        lookup.docsBySlug
        |> Map.tryFind slug
        |> Option.defaultValue Set.empty
        |> Set.toSeq

module Oracle =
    open Conn

    let filterDocsByInternPath
        (path: InternPath)
        (data: FolderData)
        (lookup: FolderLookup)
        : seq<Doc> =
        match path with
        | ExactAbs rooted
        | ExactRel (_, rooted) ->
            FolderData.tryFindDocByRelPath (RootedRelPath.relPathForced rooted) data
            |> Option.toList
            |> Seq.ofList
        | Approx relPath ->
            let canonPath =
                CanonDocPath.mk
                    ((FolderData.configOrDefault data).CoreMarkdownFileExtensions())
                    relPath

            SuffixTree.filterMatchingValues canonPath lookup.docsByPath

    let filterDocsByName (data: FolderData) (lookup: FolderLookup) (name: InternName) : seq<DocId> =
        let byTitle: seq<DocId> =
            FolderLookup.filterDocsBySlug (InternName.name name |> Slug.ofString) lookup
            |> Seq.map Doc.id

        let byPath: seq<DocId> =
            InternName.tryAsPath name
            |> Option.map (fun path -> filterDocsByInternPath path data lookup |> Seq.map Doc.id)
            |> Option.defaultValue []

        Set.ofSeq (Seq.append byTitle byPath)

    let private resolveToDoc (data: FolderData) (lookup: FolderLookup) (fromDoc: DocId) (ref: Ref) =
        match ref with
        | Ref.CrossRef r ->
            let internName = InternName.mkUnchecked fromDoc r.Doc

            filterDocsByName data lookup internName
            |> Seq.map Scope.Doc
            |> Seq.toArray
        | Ref.IntraRef _ -> [| Scope.Doc fromDoc |]

    let private resolveInDoc (data: FolderData) (ref: Ref) (inDoc: DocId) : Def[] =
        let destDoc = FolderData.findDocById inDoc data
        let destStruct = Doc.structure destDoc

        match ref with
        | Ref.CrossRef (CrossDoc _) ->
            let titles =
                Structure.symbols destStruct
                |> Seq.choose Sym.asDef
                |> Seq.filter Def.isTitle
                |> Seq.toArray

            if Array.isEmpty titles then [| Doc |] else titles
        | Ref.CrossRef (CrossSection (_, section))
        | Ref.IntraRef (IntraSection section) ->
            Structure.symbols destStruct
            |> Seq.choose Sym.asDef
            |> Seq.filter (Def.isHeaderWithId (Slug.toString section))
            |> Seq.toArray
        | Ref.IntraRef (IntraLinkDef label) ->
            Structure.symbols destStruct
            |> Seq.choose Sym.asDef
            |> Seq.filter (Def.isLinkDefWithLabel label)
            |> Seq.toArray

    let oracle data lookup : Oracle =
        let resolveToScope scope ref =
            match scope, ref with
            | Scope.Doc docId, ref -> resolveToDoc data lookup docId ref
            | _ -> [||]

        let resolveInScope ref scope =
            match ref, scope with
            | ref, Scope.Doc docId -> resolveInDoc data ref docId
            | _ -> [||]

        { resolveToScope = resolveToScope; resolveInScope = resolveInScope }

type Folder = { data: FolderData; lookup: FolderLookup; conn: Conn.Conn }

module Folder =
    let private logger = LogProvider.getLoggerByName "Folder"

    let private ignoreFiles = [ ".ignore"; ".gitignore"; ".hgignore" ]

    let isSingleFile folder =
        match folder.data with
        | SingleFile _ -> true
        | MultiFile _ -> false

    let config folder = FolderData.config folder.data

    let configOrDefault folder = FolderData.configOrDefault folder.data

    let docs folder = FolderData.docs folder.data

    let conn { conn = conn } = conn

    let id folder =
        match folder.data with
        | MultiFile { root = root } -> root
        | SingleFile { doc = doc } -> { uri = Doc.uri doc; data = RootPath(Doc.path doc) }

    let rootPath folder : RootPath =
        match folder.data with
        | MultiFile { root = root } -> root.data
        | SingleFile { doc = doc } -> Doc.rootPath doc

    let rec tryFindDocByPath (uri: AbsPath) folder : option<Doc> =
        FolderData.tryFindDocByPath uri folder.data

    let tryFindDocByRelPath (path: RelPath) folder = FolderData.tryFindDocByRelPath path folder.data

    let findDocById (id: DocId) folder = FolderData.findDocById id folder.data

    let private readIgnoreFiles (root: LocalPath) : array<string> =
        let lines = ResizeArray()

        for file in ignoreFiles do
            let path = LocalPath.appendFile root file |> LocalPath.toSystem

            if File.Exists(path) then
                logger.trace (Log.setMessage "Reading ignore globs" >> Log.addContext "file" path)

                try
                    let content = using (new StreamReader(path)) (fun f -> f.ReadToEnd())
                    lines.AddRange(content.Lines())
                with
                | :? FileNotFoundException
                | :? IOException ->
                    logger.trace (
                        Log.setMessage "Failed to read ignore globs"
                        >> Log.addContext "file" path
                    )

        lines.ToArray()

    let private loadDocs (configuredExts: array<string>) (folderId: FolderId) : seq<Doc> =
        let rec collect (cur: LocalPath) (ignoreMatchers: list<GlobMatcher>) =
            let ignoreMatchers =
                match readIgnoreFiles cur with
                | [||] -> ignoreMatchers
                | pats -> GlobMatcher.mk (LocalPath.toSystem cur) pats :: ignoreMatchers

            let di = DirectoryInfo(LocalPath.toSystem cur)

            try
                let files = di.GetFiles()
                let dirs = di.GetDirectories()

                seq {
                    for file in files do
                        if
                            (isMarkdownFile configuredExts file.FullName)
                            && not (GlobMatcher.ignoresAny ignoreMatchers file.FullName)
                        then
                            let pathUri = LocalPath.ofSystem file.FullName

                            let document = Doc.tryLoad configuredExts folderId pathUri

                            match document with
                            | Some document -> yield document
                            | _ -> ()
                        else
                            logger.trace (
                                Log.setMessage "Skipping ignored file"
                                >> Log.addContext "file" file.FullName
                            )

                    for dir in dirs do
                        if not (GlobMatcher.ignoresAny ignoreMatchers dir.FullName) then
                            yield! collect (LocalPath.ofSystem dir.FullName) ignoreMatchers
                        else
                            logger.trace (
                                Log.setMessage "Skipping ignored directory"
                                >> Log.addContext "file" dir.FullName
                            )
                }
            with
            | :? UnauthorizedAccessException as exn ->
                logger.warn (
                    Log.setMessage "Couldn't read the folder"
                    >> Log.addContext "dir" cur
                    >> Log.addException exn
                )

                Seq.empty
            | :? DirectoryNotFoundException as exn ->
                logger.warn (
                    Log.setMessage "The folder doesn't exist"
                    >> Log.addContext "dir" cur
                    >> Log.addException exn
                )

                Seq.empty

        collect
            (RootPath.toLocal folderId.data)
            [ GlobMatcher.mkDefault (RootPath.toSystem folderId.data) ]

    let private tryLoadFolderConfig (folderId: FolderId) : option<Config> =
        let folderConfigPath =
            RootPath.appendFile folderId.data ".marksman.toml" |> AbsPath.toSystem

        if File.Exists(folderConfigPath) then
            logger.trace (
                Log.setMessage "Found folder config"
                >> Log.addContext "config" folderConfigPath
            )

            let config = Config.read folderConfigPath

            if Option.isNone config then
                logger.error (
                    Log.setMessage "Malformed folder config, skipping"
                    >> Log.addContext "config" folderConfigPath
                )

            config
        else
            logger.trace (
                Log.setMessage "No folder config found"
                >> Log.addContext "path" folderConfigPath
            )

            None

    let oracle folder = Oracle.oracle folder.data folder.lookup

    let syms (folder: Folder) = FolderData.syms folder.data

    /// Identify added, removed, changed, and unchanged docs between
    /// two folders.
    let docsDifference (before: Folder) (after: Folder) =
        let idsBefore = docs before |> Seq.map Doc.id |> Set.ofSeq
        let idsAfter = docs after |> Seq.map Doc.id |> Set.ofSeq

        let idsRemoved = idsBefore - idsAfter
        let idsAdded = idsAfter - idsBefore
        let idsBoth = Set.intersect idsBefore idsAfter

        let idsChanged =
            seq {
                for id in idsBoth do
                    let beforeDoc = findDocById id before
                    let afterDoc = findDocById id after

                    if beforeDoc <> afterDoc then
                        yield id
            }
            |> Set.ofSeq

        let idsUnchanged = idsBoth - idsChanged

        { added = idsAdded
          removed = idsRemoved
          changed = idsChanged
          unchanged = idsUnchanged }

    let symsDifference (before: Folder) (after: Folder) =
        let docsDifference = docsDifference before after
        let mutable added = Set.empty
        let mutable removed = Set.empty

        for id in docsDifference.removed do
            let doc = findDocById id before

            let symsInDoc = Sym.allScopedToDoc id (Doc.syms doc) |> Set.ofSeq
            removed <- removed + symsInDoc

        for id in docsDifference.added do
            let doc = findDocById id after

            let symsInDoc = Sym.allScopedToDoc id (Doc.syms doc) |> Set.ofSeq
            added <- added + symsInDoc

        for id in docsDifference.changed do
            let beforeDoc = findDocById id before
            let afterDoc = findDocById id after
            let docDiff = Doc.symsDifference beforeDoc afterDoc

            let removedSyms = Sym.allScopedToDoc id docDiff.removed |> Set.ofSeq
            removed <- removed + removedSyms

            let addedSyms = Sym.allScopedToDoc id docDiff.added |> Set.ofSeq
            added <- added + addedSyms

        let symsDifference = { added = added; removed = removed }

        docsDifference, symsDifference

    let mk data =
        let lookup = FolderLookup.ofData data
        let conn = Conn.Conn.mk (Oracle.oracle data lookup) (FolderData.syms data)
        { data = data; lookup = lookup; conn = conn }

    let singleFile doc config : Folder =
        let data = SingleFile { doc = doc; config = config }
        mk data

    let multiFile name root (docs: seq<Doc>) config =
        let byCanonPath =
            docs
            |> Seq.map (fun doc ->
                Doc.pathFromRoot doc
                |> CanonDocPath.mk ((Config.orDefault config).CoreMarkdownFileExtensions()),
                doc)
            |> Map.ofSeq

        let data =
            MultiFile({ name = name; root = root; docs = byCanonPath; config = config })

        mk data

    let withConfig config folder =
        if config = (FolderData.config folder.data) then
            folder
        else
            match folder.data with
            | SingleFile folder -> SingleFile { folder with config = config } |> mk
            | MultiFile folder -> MultiFile { folder with config = config } |> mk

    let tryLoad (userConfig: option<Config>) (name: string) (folderId: FolderId) : option<Folder> =
        logger.trace (
            Log.setMessage "Loading folder documents"
            >> Log.addContext "uri" folderId.uri
        )

        let root = folderId.data

        if Directory.Exists(RootPath.toSystem root) then

            let folderConfig = tryLoadFolderConfig folderId
            let folderConfig = Config.mergeOpt folderConfig userConfig

            let configuredExts =
                (Option.defaultValue Config.Default folderConfig)
                    .CoreMarkdownFileExtensions()

            let documents = loadDocs configuredExts folderId


            multiFile name folderId documents folderConfig |> Some
        else
            logger.warn (
                Log.setMessage "Folder path doesn't exist"
                >> Log.addContext "uri" root
            )

            None

    let withDoc (newDoc: Doc) { data = data; lookup = lookup; conn = conn } : Folder =
        match data with
        | MultiFile folder ->
            if newDoc.RootPath <> folder.RootPath then
                failwith
                    $"Updating a folder with an unrelated doc: folder={folder.root}; doc={newDoc.RootPath}"

            let canonPath =
                CanonDocPath.mk
                    ((FolderData.configOrDefault data).CoreMarkdownFileExtensions())
                    newDoc.RelPath

            let existingDoc = Map.tryFind canonPath folder.docs

            let data =
                MultiFile { folder with docs = Map.add canonPath newDoc folder.docs }

            let lookup =
                match existingDoc with
                | None -> lookup
                | Some doc -> FolderLookup.withoutDoc doc lookup

            let lookup = FolderLookup.withDoc newDoc lookup

            let conn =
                let diff =
                    match existingDoc with
                    | None ->
                        let newSyms = Doc.syms newDoc |> Sym.allScopedToDoc newDoc.Id |> Set.ofSeq

                        { added = newSyms; removed = Set.empty }
                    | Some existingDoc ->
                        Doc.symsDifference existingDoc newDoc
                        |> Difference.map (Sym.scopedToDoc newDoc.Id)

                Conn.Conn.update (Oracle.oracle data lookup) diff conn

            if ProcessFlags.paranoid then
                let fromScratchConn =
                    Conn.Conn.mk (Oracle.oracle data lookup) (FolderData.syms data)

                let connDiff = Conn.Conn.difference fromScratchConn conn

                if connDiff.IsEmpty() |> not then
                    failwith
                        $"""PARANOID MODE ERROR:
Compared to the one built from scratch, the incremental graph has:
{connDiff.CompactFormat()}
"""

            { data = data; lookup = lookup; conn = conn }
        | SingleFile ({ doc = existingDoc } as folder) ->
            if newDoc.Id <> existingDoc.Id then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={existingDoc.RootPath}; doc={newDoc.RootPath}"

            mk (SingleFile { folder with doc = newDoc })

    let withoutDoc (docId: DocId) folder : option<Folder> =
        match folder.data with
        | MultiFile mf ->
            let canonPath =
                docId.Path
                |> RootedRelPath.relPathForced
                |> CanonDocPath.mk ((configOrDefault folder).CoreMarkdownFileExtensions())

            match Map.tryFind canonPath mf.docs with
            | None -> Some folder
            | Some doc ->
                let docs = Map.remove canonPath mf.docs
                let data = MultiFile { mf with docs = docs }
                let lookup = FolderLookup.withoutDoc doc folder.lookup

                let conn =
                    let removedSyms = Doc.syms doc |> Sym.allScopedToDoc docId |> Set.ofSeq

                    let diff = { added = Set.empty; removed = removedSyms }
                    Conn.Conn.update (Oracle.oracle data lookup) diff folder.conn

                Some { data = data; lookup = lookup; conn = conn }
        | SingleFile { doc = doc } ->
            if doc.Id <> docId then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={doc.RootPath}; doc={docId}"
            else
                None

    let closeDoc (docId: DocId) (folder: Folder) : option<Folder> =
        let exts = (configOrDefault folder).CoreMarkdownFileExtensions()

        match folder.data with
        | MultiFile { root = root } ->
            match Doc.tryLoad exts root (Abs <| RootedRelPath.toAbs docId.Path) with
            | Some doc -> withDoc doc folder |> Some
            | _ -> withoutDoc docId folder
        | SingleFile { doc = doc } ->
            if doc.Id <> docId then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={doc.RootPath}; doc={docId}"
            else
                None

    let tryFindDocByUrl (folderRelUrl: string) (folder: Folder) : option<Doc> =
        let urlEncoded = folderRelUrl.AbsPathUrlEncode()

        let isMatchingDoc (doc: Doc) =
            let docUrl = (RelPath.toSystem doc.RelPath).AbsPathUrlEncode()
            docUrl = urlEncoded

        docs folder |> Seq.tryFind isMatchingDoc

    let docCount folder : int =
        match folder.data with
        | SingleFile _ -> 1
        | MultiFile { docs = docs } -> docs.Values.Count

    let filterDocsBySlug (slug: Slug) (folder: Folder) : seq<Doc> =
        FolderLookup.filterDocsBySlug slug folder.lookup

    let filterDocsByInternPath (path: InternPath) (folder: Folder) : seq<Doc> =
        Oracle.filterDocsByInternPath path folder.data folder.lookup

    let filterDocsByName (name: InternName) (folder: Folder) : seq<Doc> =
        Oracle.filterDocsByName folder.data folder.lookup name
        |> Seq.map (flip findDocById folder)

    let configuredMarkdownExts folder =
        (configOrDefault folder).CoreMarkdownFileExtensions() |> Seq.ofArray
