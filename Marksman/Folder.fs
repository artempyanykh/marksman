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

type MultiFile = {
    name: string
    root: FolderId
    docs: Map<RelPath, Doc>
    config: option<Config>
} with

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
        | MultiFile { docs = docs } -> Map.tryFind path docs

    let tryFindDocByPath (uri: AbsPath) data : option<Doc> =
        match data with
        | SingleFile { doc = doc } -> Some doc |> Option.filter (fun x -> Doc.path x = uri)
        | MultiFile { root = root; docs = docs } ->
            RootedRelPath.mk root.data (Abs uri)
            |> RootedRelPath.relPathForced
            |> fun path -> Map.tryFind path docs

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

type FolderLookup = {
    docsBySlug: Map<Slug, Set<Doc>>
    docsByPath: SuffixTree<CanonDocPath, Doc>
    config: option<Config>
}

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
                Map.values data.docs
                |> Seq.groupBy Doc.slug
                |> Seq.map (fun (slug, docs) -> slug, Set.ofSeq docs)
                |> Map.ofSeq

            let byPath =
                Map.values data.docs
                |> Seq.map (fun doc -> Doc.pathFromRoot doc |> CanonDocPath.mk mdExt, doc)
                |> SuffixTree.ofSeq CanonDocPath.components

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
        let byPath = SuffixTree.removeValue docPath doc lookup.docsByPath
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

    let resolveDocumentsByPath
        (path: InternPath)
        (data: FolderData)
        (lookup: FolderLookup)
        : seq<Doc> =
        match path with
        | ExactAbs rooted
        | ExactRel(_, rooted) ->
            let path = RootedRelPath.relPathForced rooted

            match FolderData.tryFindDocByRelPath path data with
            | Some doc when Doc.pathFromRoot doc = path -> Seq.singleton doc
            | _ ->
                let canonPath =
                    CanonDocPath.mk
                        ((FolderData.configOrDefault data).CoreMarkdownFileExtensions())
                        path

                SuffixTree.findExactValues canonPath lookup.docsByPath |> Set.toSeq
        | Approx relPath ->
            let canonPath =
                CanonDocPath.mk
                    ((FolderData.configOrDefault data).CoreMarkdownFileExtensions())
                    relPath

            if CanonDocPath.components canonPath |> List.isEmpty then
                Seq.empty
            else
                SuffixTree.filterMatchingValues canonPath lookup.docsByPath

    let resolveCandidateDocuments
        (data: FolderData)
        (lookup: FolderLookup)
        (name: InternName)
        : CandidateDocumentResolution =
        let exts = (FolderData.configOrDefault data).CoreMarkdownFileExtensions()
        let aliasesRead = DocumentAlias.ofReferenceName exts name

        let documents =
            aliasesRead
            |> Seq.collect (function
                | DocumentAlias.TitleSlug slug -> FolderLookup.filterDocsBySlug slug lookup
                | DocumentAlias.CanonicalPath path ->
                    SuffixTree.findExactValues path lookup.docsByPath |> Set.toSeq
                | DocumentAlias.PathSuffix parts ->
                    SuffixTree.filterMatchingParts parts lookup.docsByPath)
            |> Seq.map Doc.id
            |> Set.ofSeq

        { documents = documents; aliasesRead = aliasesRead }

    let private selectDefinitions (data: FolderData) (selector: Conn.DefinitionSelector) : Def[] =
        match Conn.DefinitionSelector.scope selector with
        | Scope.Global -> [||]
        | Scope.Doc docId ->
            let definitionsRead =
                FolderData.findDocById docId data
                |> Doc.structure
                |> Structure.symbols
                |> Seq.choose Sym.asDef
                |> Seq.filter (Conn.DefinitionSelector.readsDefinition selector)

            match selector with
            | Conn.DefinitionSelector.DocumentTarget _ ->
                let titles = definitionsRead |> Seq.filter Def.isTitle |> Seq.toArray
                if Array.isEmpty titles then [| Def.Doc |] else titles
            | Conn.DefinitionSelector.SectionTarget _
            | Conn.DefinitionSelector.LinkDefinitionTarget _ ->
                definitionsRead |> Seq.toArray

    let oracle data lookup : Oracle = {
        resolveCandidateDocuments = resolveCandidateDocuments data lookup
        selectDefinitions = selectDefinitions data
    }

type Folder = { data: FolderData; lookup: FolderLookup; conn: Conn.Conn }

module Folder =
    let private logger = LogProvider.getLoggerByName "Folder"

    let private ignoreFiles = [ ".ignore"; ".gitignore"; ".hgignore" ]

    let private isRealWorkspaceFolder (root: RootPath) : bool =
        let root = RootPath.toSystem root

        if Directory.Exists(root) then
            let markerFiles = [| ".marksman.toml" |]
            let markerDirs = [| ".git"; ".hg"; ".svn"; ".jj" |]

            let hasMarkerFile () =
                Array.exists (fun marker -> File.Exists(Path.Join(root, marker))) markerFiles

            let hasMarkerDir () =
                Array.exists (fun marker -> Directory.Exists(Path.Join(root, marker))) markerDirs

            hasMarkerDir () || hasMarkerFile ()
        else
            false

    // Context is in
    // * https://github.com/helix-editor/helix/issues/4436
    // * https://github.com/artempyanykh/marksman/discussions/377
    let checkWorkspaceFolderWithWarn (folderId: FolderId) : bool =
        if isRealWorkspaceFolder folderId.data then
            true
        else
            logger.warn (
                Log.setMessage "Workspace folder is bogus"
                >> Log.addContext "root" folderId.data
            )

            false

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

    let tryFindDocByRelPath (path: RelPath) folder =
        match FolderData.tryFindDocByRelPath path folder.data with
        | Some doc -> Some doc
        | None ->
            let extensions = (configOrDefault folder).CoreMarkdownFileExtensions()
            let canonPath = CanonDocPath.mk extensions path

            SuffixTree.findExactValues canonPath folder.lookup.docsByPath
            |> Set.toList
            |> function
                | [ doc ] -> Some doc
                | _ -> None

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

    let private loadDocs (parserSettings: ParserSettings) (folderId: FolderId) : seq<Doc> =
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
                            (isMarkdownFile parserSettings.mdFileExt file.FullName)
                            && not (GlobMatcher.ignoresAny ignoreMatchers file.FullName)
                        then
                            let pathUri = LocalPath.ofSystem file.FullName

                            let document = Doc.tryLoad parserSettings folderId pathUri

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

        collect (RootPath.toLocal folderId.data) [
            GlobMatcher.mkDefault (RootPath.toSystem folderId.data)
        ]

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
        let keyedDocs =
            function
            | SingleFile { doc = doc } -> Seq.singleton (Doc.pathFromRoot doc, doc)
            | MultiFile { docs = docs } -> Map.toSeq docs

        let mutable added = Set.empty
        let mutable removed = Set.empty
        let mutable changed = Set.empty
        let mutable unchanged = Set.empty

        use oldDocs = (keyedDocs before.data).GetEnumerator()
        use newDocs = (keyedDocs after.data).GetEnumerator()
        let mutable hasOld = oldDocs.MoveNext()
        let mutable hasNew = newDocs.MoveNext()

        while hasOld && hasNew do
            let oldPath, oldDoc = oldDocs.Current
            let newPath, newDoc = newDocs.Current

            match compare oldPath newPath with
            | n when n < 0 ->
                removed <- Set.add (Doc.id oldDoc) removed
                hasOld <- oldDocs.MoveNext()
            | n when n > 0 ->
                added <- Set.add (Doc.id newDoc) added
                hasNew <- newDocs.MoveNext()
            | _ ->
                let oldId = Doc.id oldDoc
                let newId = Doc.id newDoc

                if oldId <> newId then
                    removed <- Set.add oldId removed
                    added <- Set.add newId added
                elif obj.ReferenceEquals(oldDoc, newDoc) || oldDoc = newDoc then
                    unchanged <- Set.add oldId unchanged
                else
                    changed <- Set.add oldId changed

                hasOld <- oldDocs.MoveNext()
                hasNew <- newDocs.MoveNext()

        while hasOld do
            removed <- Set.add (Doc.id (snd oldDocs.Current)) removed
            hasOld <- oldDocs.MoveNext()

        while hasNew do
            added <- Set.add (Doc.id (snd newDocs.Current)) added
            hasNew <- newDocs.MoveNext()

        {
            added = added
            removed = removed
            changed = changed
            unchanged = unchanged
        }

    let mk data =
        let lookup = FolderLookup.ofData data
        let conn = Conn.Conn.mk (Oracle.oracle data lookup) (FolderData.syms data)
        { data = data; lookup = lookup; conn = conn }

    let singleFile doc config : Folder =
        let data = SingleFile { doc = doc; config = config }
        mk data

    let multiFile name root (docs: seq<Doc>) config =
        let byPath = docs |> Seq.map (fun doc -> Doc.pathFromRoot doc, doc) |> Map.ofSeq

        let data =
            MultiFile({ name = name; root = root; docs = byPath; config = config })

        mk data

    let withConfig config folder =
        if config = (FolderData.config folder.data) then
            folder
        else
            match folder.data with
            | SingleFile folder -> SingleFile { folder with config = config } |> mk
            | MultiFile folder ->
                // Extension changes also change canonical path keys. Rebuild
                // the document map, not just the indexes over its old keys.
                multiFile folder.name folder.root (Map.values folder.docs) config

    let tryLoad (userConfig: option<Config>) (name: string) (folderId: FolderId) : option<Folder> =
        logger.info (
            Log.setMessage "Loading folder documents"
            >> Log.addContext "uri" folderId.uri
        )

        let root = folderId.data

        if Directory.Exists(RootPath.toSystem root) then

            let folderConfig = tryLoadFolderConfig folderId
            let folderConfig = Config.mergeOpt folderConfig userConfig

            let parserSettings =
                ParserSettings.OfConfig(Option.defaultValue Config.Default folderConfig)

            let documents = loadDocs parserSettings folderId

            multiFile name folderId documents folderConfig |> Some
        else
            logger.warn (
                Log.setMessage "Folder path doesn't exist"
                >> Log.addContext "uri" root
            )

            None

    let private documentInput doc : Conn.DocumentInput = {
        id = Doc.id doc
        slug = Doc.slug doc
        path = Doc.pathFromRoot doc
        symbols = Doc.syms doc
    }

    let private updateConnectionGraph data lookup (change: Conn.ConnectionChange) previous =
        let config = FolderData.configOrDefault data
        let oracle = Oracle.oracle data lookup

        let conn =
            if config.CoreIncrementalReferences() then
                Conn.Conn.update oracle change previous
            else
                Conn.Conn.mk oracle (FolderData.syms data)

        if config.CoreParanoid() then
            let rebuilt = Conn.Conn.mk oracle (FolderData.syms data)
            let diff = Conn.Conn.difference rebuilt conn

            if not (diff.IsEmpty()) then
                failwith $"PARANOID MODE ERROR:\n{diff.CompactFormat()}"

        conn

    let withDoc (newDoc: Doc) { data = prevData; lookup = prevLookup; conn = prevConn } : Folder =
        match prevData with
        | MultiFile folder ->
            if newDoc.RootPath <> folder.RootPath then
                failwith
                    $"Updating a folder with an unrelated doc: folder={folder.root}; doc={newDoc.RootPath}"

            let config = FolderData.configOrDefault prevData

            let path = newDoc.RelPath
            let existingDoc = Map.tryFind path folder.docs

            let data =
                MultiFile { folder with docs = Map.add path newDoc folder.docs }

            let lookup =
                match existingDoc with
                | None -> prevLookup
                | Some doc -> FolderLookup.withoutDoc doc prevLookup

            let lookup = FolderLookup.withDoc newDoc lookup

            let documentChange =
                match existingDoc with
                | None -> Conn.DocumentChange.Added(documentInput newDoc)
                | Some existingDoc ->
                    Conn.DocumentChange.Replaced(documentInput existingDoc, documentInput newDoc)

            let change =
                Conn.ConnectionChange.ofDocuments
                    (config.CoreMarkdownFileExtensions())
                    [ documentChange ]

            let conn =
                if Conn.ConnectionChange.isEmpty change && not (config.CoreParanoid()) then
                    prevConn
                else
                    updateConnectionGraph data lookup change prevConn

            { data = data; lookup = lookup; conn = conn }
        | SingleFile({ doc = existingDoc } as folder) ->
            if newDoc.Id <> existingDoc.Id then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={existingDoc.RootPath}; doc={newDoc.RootPath}"

            mk (SingleFile { folder with doc = newDoc })

    let withoutDoc (docId: DocId) folder : option<Folder> =
        match folder.data with
        | MultiFile mf ->
            let path = docId.Path |> RootedRelPath.relPathForced

            match Map.tryFind path mf.docs with
            | None -> Some folder
            | Some doc ->
                let docs = Map.remove path mf.docs
                let data = MultiFile { mf with docs = docs }
                let lookup = FolderLookup.withoutDoc doc folder.lookup

                let conn =
                    let config = FolderData.configOrDefault data
                    let change =
                        Conn.ConnectionChange.ofDocuments
                            (config.CoreMarkdownFileExtensions())
                            [ Conn.DocumentChange.Removed(documentInput doc) ]

                    updateConnectionGraph data lookup change folder.conn

                Some { data = data; lookup = lookup; conn = conn }
        | SingleFile { doc = doc } ->
            if doc.Id <> docId then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={doc.RootPath}; doc={docId}"
            else
                None

    let parserSettings folder = ParserSettings.OfConfig(configOrDefault folder)

    let closeDoc (docId: DocId) (folder: Folder) : option<Folder> =
        let parserSettings = parserSettings folder

        match folder.data with
        | MultiFile { root = root } ->
            match Doc.tryLoad parserSettings root (Abs <| RootedRelPath.toAbs docId.Path) with
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
        Oracle.resolveDocumentsByPath path folder.data folder.lookup

    let filterDocsByName (name: InternName) (folder: Folder) : seq<Doc> =
        Oracle.resolveCandidateDocuments folder.data folder.lookup name
        |> fun resolution -> resolution.documents
        |> Seq.map (flip findDocById folder)

    let configuredMarkdownExts folder =
        (configOrDefault folder).CoreMarkdownFileExtensions() |> Seq.ofArray
