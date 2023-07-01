module Marksman.Workspace

open System
open System.IO
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open FSharpPlus.Operators

open Marksman.GitIgnore
open Marksman.Config
open Marksman.Parser
open Marksman.Text
open Marksman.Misc
open Marksman.Paths
open Marksman.Names
open Marksman.Cst
open Marksman.Index
open Marksman.SuffixTree

open Microsoft.FSharp.Core


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
        let cst = parseText text
        let index = Index.ofCst cst

        { id = id; version = version; text = text; cst = cst; index = index }

    let id { id = id } = id
    let text doc = doc.text

    let withText newText doc =
        let newCst = parseText newText
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

type MultiFile =
    { name: string
      root: FolderId
      docs: Map<PathStem<RelPath>, Doc>
      config: option<Config> }

    member this.RootPath = this.root.data


type SingleFile = { doc: Doc; config: option<Config> }

type FolderData =
    | MultiFile of MultiFile
    | SingleFile of SingleFile

type FolderLookup =
    { docsBySlug: Map<Slug, Set<Doc>>
      docsByPath: SuffixTree<PathStem<RelPath>, Doc> }

module FolderLookup =
    let private docPathComps relPathStem =
        let (PathStem relPath) = relPathStem
        LocalPath.components (Rel relPath) |> List.ofArray

    let ofData (data: FolderData) =
        match data with
        | SingleFile data ->
            let bySlug = Map.ofList [ Doc.slug data.doc, Set.ofList [ data.doc ] ]

            let path = Doc.pathFromRoot data.doc |> RelPath.filepathStem
            let byPath = SuffixTree.ofSeq docPathComps [ path, data.doc ]

            { docsBySlug = bySlug; docsByPath = byPath }
        | MultiFile data ->
            let bySlug =
                Map.toSeq data.docs
                |> Seq.map snd
                |> Seq.groupBy Doc.slug
                |> Seq.map (fun (slug, docs) -> slug, Set.ofSeq docs)
                |> Map.ofSeq

            let byPath = Map.toSeq data.docs |> SuffixTree.ofSeq docPathComps

            { docsBySlug = bySlug; docsByPath = byPath }

    let withoutDoc (doc: Doc) (lookup: FolderLookup) =
        let slug = Doc.slug doc

        let docPath = Doc.pathFromRoot doc |> RelPath.filepathStem

        let updateBySlug =
            function
            | None -> None
            | Some docs -> Set.remove doc docs |> Some

        let bySlug = Map.change slug updateBySlug lookup.docsBySlug
        let byPath = SuffixTree.remove docPath lookup.docsByPath
        { docsBySlug = bySlug; docsByPath = byPath }

    let withDoc (doc: Doc) (lookup: FolderLookup) =
        let slug = Doc.slug doc

        let docPath = Doc.pathFromRoot doc |> RelPath.filepathStem

        let updateBySlug =
            function
            | None -> None
            | Some docs -> Set.add doc docs |> Some

        let bySlug = Map.change slug updateBySlug lookup.docsBySlug
        let byPath = SuffixTree.add docPath doc lookup.docsByPath
        { docsBySlug = bySlug; docsByPath = byPath }


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
        let byRelPathStem =
            docs
            |> Seq.map (fun doc -> Doc.pathFromRoot doc |> RelPath.filepathStem, doc)
            |> Map.ofSeq

        let data =
            MultiFile({ name = name; root = root; docs = byRelPathStem; config = config })

        mk data

    let isSingleFile folder =
        match folder.data with
        | SingleFile _ -> true
        | MultiFile _ -> false

    let config folder =
        match folder.data with
        | SingleFile { config = config } -> config
        | MultiFile { config = config } -> config

    let configOrDefault folder = config folder |> Option.defaultValue Config.Default

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
            let filepathStem =
                RootedRelPath.mk root.data (Abs uri)
                |> RootedRelPath.relPathForced
                |> RelPath.filepathStem

            Map.tryFind filepathStem docs

    let tryFindDocByRelPath (path: RelPath) folder : option<Doc> =
        match folder.data with
        | SingleFile { doc = doc } ->
            Some doc
            |> Option.filter (fun x ->
                let sysPath = ((Doc.path x) |> AbsPath.filenameStem)
                sysPath.EndsWith(path |> RelPath.filenameStem))
        | MultiFile { docs = docs } -> Map.tryFind (path |> RelPath.filepathStem) docs

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

            let docPathStem = newDoc.RelPath |> RelPath.filepathStem

            let lookup =
                match Map.tryFind docPathStem folder.docs with
                | None -> lookup
                | Some doc -> FolderLookup.withoutDoc doc lookup

            let lookup = FolderLookup.withDoc newDoc lookup

            let data =
                MultiFile { folder with docs = Map.add docPathStem newDoc folder.docs }

            { data = data; lookup = lookup }
        | SingleFile ({ doc = existingDoc } as folder) ->
            if newDoc.id <> existingDoc.id then
                failwith
                    $"Updating a singleton folder with an unrelated doc: folder={existingDoc.RootPath}; doc={newDoc.RootPath}"

            mk (SingleFile { folder with doc = newDoc })

    let withoutDoc (docId: DocId) folder : option<Folder> =
        match folder.data with
        | MultiFile mf ->
            let docPathStem =
                docId.data |> RootedRelPath.relPathForced |> RelPath.filepathStem

            match Map.tryFind docPathStem mf.docs with
            | None -> Some folder
            | Some doc ->
                let docs = Map.remove docPathStem mf.docs
                let data = MultiFile { mf with docs = docs }

                let lookup = FolderLookup.withoutDoc doc folder.lookup
                Some { data = data; lookup = lookup }
        | SingleFile { doc = doc } ->
            if doc.id <> docId then
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
            if doc.id <> docId then
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
            // TODO: fix to work with paths that contain '.'s
            let docPath = RelPath.filepathStem relPath

            SuffixTree.filterMatchingValues docPath folder.lookup.docsByPath

type Workspace = { config: option<Config>; folders: Map<FolderId, Folder> }

module Workspace =
    // TODO(arr): reconsider the need for this function (when we load folders we require userConfig)
    let mergeFolderConfig userConfig (folder: Folder) =
        let merged = Config.mergeOpt (Folder.config folder) userConfig
        Folder.withConfig merged folder

    let ofFolders (userConfig: option<Config>) (folders: seq<Folder>) : Workspace =
        let folders = folders |> Seq.map (mergeFolderConfig userConfig)

        { config = userConfig
          folders = folders |> Seq.map (fun f -> Folder.id f, f) |> Map.ofSeq }

    let folders (workspace: Workspace) : seq<Folder> =
        seq {
            for KeyValue (_, f) in workspace.folders do
                yield f
        }

    let userConfig { Workspace.config = config } = config

    let tryFindFolderEnclosing (innerPath: AbsPath) (workspace: Workspace) : option<Folder> =
        workspace.folders
        |> Map.tryPick (fun folderId folder ->
            let folderPath = folderId.data

            if RootPath.contains folderPath (Abs innerPath) then
                Some folder
            else
                None)

    let withoutFolder (keyPath: FolderId) (workspace: Workspace) : Workspace =
        { workspace with folders = Map.remove keyPath workspace.folders }

    let withoutFolders (roots: seq<FolderId>) (workspace: Workspace) : Workspace =
        let newFolders = roots |> Seq.fold (flip Map.remove) workspace.folders

        { workspace with folders = newFolders }

    let withFolder (newFolder: Folder) (workspace: Workspace) : Workspace =
        let newFolder = mergeFolderConfig workspace.config newFolder

        let updatedFolders =
            match newFolder.data with
            | SingleFile _ -> Map.add (Folder.id newFolder) newFolder workspace.folders
            | MultiFile { root = root } ->
                let newRoot = root.data

                let isEnclosed _ (existingFolder: Folder) =
                    match existingFolder.data with
                    | MultiFile _ -> false
                    | SingleFile _ ->
                        let existingRoot = Abs (Folder.rootPath existingFolder).Path

                        RootPath.contains newRoot existingRoot

                let isNotEnclosed id existingFolder = not (isEnclosed id existingFolder)

                workspace.folders
                |> Map.filter isNotEnclosed
                |> Map.add (Folder.id newFolder) newFolder

        { workspace with folders = updatedFolders }

    let withFolders (folders: seq<Folder>) (workspace: Workspace) : Workspace =
        Seq.fold (flip withFolder) workspace folders

    let docCount (workspace: Workspace) : int =
        workspace.folders.Values |> Seq.sumBy Folder.docCount

    let folderCount (workspace: Workspace) : int = workspace.folders.Count
