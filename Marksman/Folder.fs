module Marksman.Folder

open System
open System.IO

open Ionide.LanguageServerProtocol.Logging

open Marksman.GitIgnore
open Marksman.SuffixTree
open Marksman.Config
open Marksman.Doc
open Marksman.Misc
open Marksman.Names
open Marksman.Paths

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

type FolderLookup =
    { docsBySlug: Map<Slug, Set<Doc>>
      docsByPath: SuffixTree<CanonDocPath, Doc>
      config: option<Config> }

module FolderLookup =
    let ofData (data: FolderData) =
        let config = FolderData.config data

        let mdExt =
            (config |> Option.defaultValue Config.Default)
                .CoreMarkdownFileExtensions()

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
            | None -> None
            | Some docs -> Set.add doc docs |> Some

        let bySlug = Map.change slug updateBySlug lookup.docsBySlug
        let byPath = SuffixTree.add docPath doc lookup.docsByPath
        { docsBySlug = bySlug; docsByPath = byPath; config = lookup.config }


type Folder = { data: FolderData; lookup: FolderLookup }

module Folder =
    let private logger = LogProvider.getLoggerByName "Folder"

    let private ignoreFiles = [ ".ignore"; ".gitignore"; ".hgignore" ]

    let mk data =
        let lookup = FolderLookup.ofData data
        { data = data; lookup = lookup }

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

    let isSingleFile folder =
        match folder.data with
        | SingleFile _ -> true
        | MultiFile _ -> false

    let config folder =
        match folder.data with
        | SingleFile { config = config } -> config
        | MultiFile { config = config } -> config

    let configOrDefault folder = Config.orDefault (config folder)

    let withConfig config folder =
        match folder.data with
        | SingleFile folder -> SingleFile { folder with config = config } |> mk
        | MultiFile folder -> MultiFile { folder with config = config } |> mk

    let docs folder =
        match folder.data with
        | SingleFile { doc = doc } -> Seq.singleton doc
        | MultiFile { docs = docs } -> Map.values docs

    let id folder =
        match folder.data with
        | MultiFile { root = root } -> root
        | SingleFile { doc = doc } -> { uri = Doc.uri doc; data = RootPath(Doc.path doc) }

    let rootPath folder : RootPath =
        match folder.data with
        | MultiFile { root = root } -> root.data
        | SingleFile { doc = doc } -> Doc.rootPath doc

    let tryFindDocByPath (uri: AbsPath) folder : option<Doc> =
        match folder.data with
        | SingleFile { doc = doc } -> Some doc |> Option.filter (fun x -> Doc.path x = uri)
        | MultiFile { root = root; docs = docs } ->
            let canonPath =
                RootedRelPath.mk root.data (Abs uri)
                |> RootedRelPath.relPathForced
                |> CanonDocPath.mk ((configOrDefault folder).CoreMarkdownFileExtensions())

            Map.tryFind canonPath docs

    let tryFindDocByRelPath (path: RelPath) folder : option<Doc> =
        match folder.data with
        | SingleFile { doc = doc } ->
            Some doc
            |> Option.filter (fun x ->
                let sysPath = ((Doc.path x) |> AbsPath.filenameStem)
                sysPath.EndsWith(path |> RelPath.filenameStem))
        | MultiFile { docs = docs } ->
            let canonPath =
                path
                |> CanonDocPath.mk ((configOrDefault folder).CoreMarkdownFileExtensions())

            Map.tryFind canonPath docs

    let findDocById (id: DocId) folder : Doc =
        let docRelPath = id.data |> RootedRelPath.relPathForced

        tryFindDocByRelPath docRelPath folder
        |> Option.defaultWith (fun () -> failwith $"Expected doc could not be found: {id.uri}")

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

                            let document = Doc.tryLoad folderId pathUri

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

    let withDoc (newDoc: Doc) { data = data; lookup = lookup } : Folder =
        match data with
        | MultiFile folder ->
            if newDoc.RootPath <> folder.RootPath then
                failwith
                    $"Updating a folder with an unrelated doc: folder={folder.root}; doc={newDoc.RootPath}"

            let canonPath =
                CanonDocPath.mk
                    ((FolderData.configOrDefault data).CoreMarkdownFileExtensions())
                    newDoc.RelPath

            let lookup =
                match Map.tryFind canonPath folder.docs with
                | None -> lookup
                | Some doc -> FolderLookup.withoutDoc doc lookup

            let lookup = FolderLookup.withDoc newDoc lookup

            let data =
                MultiFile { folder with docs = Map.add canonPath newDoc folder.docs }

            { data = data; lookup = lookup }
        | SingleFile ({ doc = existingDoc } as folder) ->
            if newDoc.Id <> existingDoc.Id then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={existingDoc.RootPath}; doc={newDoc.RootPath}"

            mk (SingleFile { folder with doc = newDoc })

    let withoutDoc (docId: DocId) folder : option<Folder> =
        match folder.data with
        | MultiFile mf ->
            let canonPath =
                docId.data
                |> RootedRelPath.relPathForced
                |> CanonDocPath.mk ((configOrDefault folder).CoreMarkdownFileExtensions())

            match Map.tryFind canonPath mf.docs with
            | None -> Some folder
            | Some doc ->
                let docs = Map.remove canonPath mf.docs
                let data = MultiFile { mf with docs = docs }

                let lookup = FolderLookup.withoutDoc doc folder.lookup
                Some { data = data; lookup = lookup }
        | SingleFile { doc = doc } ->
            if doc.Id <> docId then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={doc.RootPath}; doc={docId}"
            else
                None

    let closeDoc (docId: DocId) (folder: Folder) : option<Folder> =
        match folder.data with
        | MultiFile { root = root } ->
            match Doc.tryLoad root (Abs <| RootedRelPath.toAbs docId.data) with
            | Some doc -> withDoc doc folder |> Some
            | _ -> withoutDoc docId folder
        | SingleFile { doc = doc } ->
            if doc.Id <> docId then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={doc.RootPath}; doc={docId}"
            else
                None

    /// Find document matching a slug.
    /// First check for full match. When nothing matches, check fuzzy substring match.
    let filterDocsBySlug (slug: Slug) (folder: Folder) : seq<Doc> =
        folder.lookup.docsBySlug
        |> Map.tryFind slug
        |> Option.defaultValue Set.empty
        |> Set.toSeq

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

    let filterDocsByInternPath (path: InternPath) (folder: Folder) : seq<Doc> =
        match path with
        | ExactAbs rooted
        | ExactRel (_, rooted) ->
            tryFindDocByRelPath (RootedRelPath.relPathForced rooted) folder
            |> Option.toList
            |> Seq.ofList
        | Approx relPath ->
            let canonPath =
                CanonDocPath.mk ((configOrDefault folder).CoreMarkdownFileExtensions()) relPath

            SuffixTree.filterMatchingValues canonPath folder.lookup.docsByPath

