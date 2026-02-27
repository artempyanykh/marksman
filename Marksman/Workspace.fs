module Marksman.Workspace

open System.IO

open Ionide.LanguageServerProtocol.Logging

open Marksman.Config
open Marksman.Misc
open Marksman.Paths
open Marksman.Names
open Marksman.Folder

type Workspace = {
    config: option<Config>
    folders: Map<FolderId, Folder>
    extraFolderIds: Set<FolderId>
}

module Workspace =
    let private logger = LogProvider.getLoggerByName "Workspace"

    // TODO(arr): reconsider the need for this function (when we load folders we require userConfig)
    let mergeFolderConfig userConfig (folder: Folder) =
        let merged = Config.mergeOpt (Folder.config folder) userConfig
        Folder.withConfig merged folder

    let private loadExtraFolders
        (userConfig: option<Config>)
        (primaryFolder: Folder)
        : seq<Folder> =
        let roots = Folder.extraFolderRoots primaryFolder

        seq {
            for absPath in roots do
                let sysPath = AbsPath.toSystem absPath
                let name = Path.GetFileName(sysPath)
                let folderId = { uri = AbsPath.toUri absPath; data = RootPath absPath }

                match Folder.tryLoad userConfig name folderId with
                | Some extraFolder ->
                    logger.debug (
                        Log.setMessage "Loaded extra folder"
                        >> Log.addContext "path" sysPath
                        >> Log.addContext
                            "primary"
                            (Folder.rootPath primaryFolder |> RootPath.toSystem)
                    )

                    yield extraFolder
                | None ->
                    logger.warn (
                        Log.setMessage
                            "Failed to load extra folder — path may not exist or contain no markdown files"
                        >> Log.addContext "path" sysPath
                        >> Log.addContext
                            "primary"
                            (Folder.rootPath primaryFolder |> RootPath.toSystem)
                    )
        }

    let ofFolders (userConfig: option<Config>) (folders: seq<Folder>) : Workspace =
        let primaryFolders =
            folders |> Seq.map (mergeFolderConfig userConfig) |> Array.ofSeq

        let primaryFolderMap =
            primaryFolders |> Array.map (fun f -> Folder.id f, f) |> Map.ofArray

        // Load extra folders declared by primary folders; deduplicate by folder ID
        let allFolders, extraFolderIds =
            primaryFolders
            |> Seq.collect (loadExtraFolders userConfig)
            |> Seq.fold
                (fun (fm, ids) ef ->
                    let eid = Folder.id ef

                    if Map.containsKey eid fm then
                        fm, Set.add eid ids
                    else
                        Map.add eid ef fm, Set.add eid ids)
                (primaryFolderMap, Set.empty)

        {
            config = userConfig
            folders = allFolders
            extraFolderIds = extraFolderIds
        }

    let folders (workspace: Workspace) : seq<Folder> =
        seq {
            for KeyValue(_, f) in workspace.folders do
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

    let withoutFolder (keyPath: FolderId) (workspace: Workspace) : Workspace = {
        workspace with
            folders = Map.remove keyPath workspace.folders
    }

    let withoutFolders (roots: seq<FolderId>) (workspace: Workspace) : Workspace =
        let newFolders = roots |> Seq.fold (flip Map.remove) workspace.folders

        { workspace with folders = newFolders }

    let withFolder (newFolder: Folder) (workspace: Workspace) : Workspace =
        let newFolder = mergeFolderConfig workspace.config newFolder

        let updatedFolders =
            if Folder.isSingleFile newFolder then
                Map.add (Folder.id newFolder) newFolder workspace.folders
            else
                let newRoot = Folder.rootPath newFolder

                let isEnclosed _ (existingFolder: Folder) =
                    if Folder.isSingleFile existingFolder then
                        let existingRoot = Abs (Folder.rootPath existingFolder).Path

                        RootPath.contains newRoot existingRoot
                    else
                        false


                let isNotEnclosed id existingFolder = not (isEnclosed id existingFolder)

                workspace.folders
                |> Map.filter isNotEnclosed
                |> Map.add (Folder.id newFolder) newFolder

        { workspace with folders = updatedFolders }

    let withFolders (folders: seq<Folder>) (workspace: Workspace) : Workspace =
        Seq.fold (flip withFolder) workspace folders

    let isExtraFolder (ws: Workspace) (folderId: FolderId) : bool =
        Set.contains folderId ws.extraFolderIds

    let primaryFolders (ws: Workspace) : seq<Folder> =
        ws.folders
        |> Map.toSeq
        |> Seq.choose (fun (id, folder) ->
            if Set.contains id ws.extraFolderIds then None else Some folder)

    let extraFoldersFor (folder: Folder) (ws: Workspace) : seq<Folder> =
        let roots = Folder.extraFolderRoots folder

        seq {
            for absPath in roots do
                let folderId = { uri = AbsPath.toUri absPath; data = RootPath absPath }

                match Map.tryFind folderId ws.folders with
                | Some extraFolder -> yield extraFolder
                | None -> ()
        }

    let primaryFoldersReferencing (extraFolderId: FolderId) (ws: Workspace) : seq<Folder> =
        let extraRoot = extraFolderId.data.Path

        // Search all folders (not just primary) because an extra folder may itself declare
        // the primary folder as its extra_folder, creating a mutual reference. In that case
        // its docs can reference primary-folder elements and must receive rename edits.
        ws.folders
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (fun folder ->
            Folder.extraFolderRoots folder
            |> Array.exists (fun root -> root = extraRoot))

    let docCount (workspace: Workspace) : int =
        workspace.folders.Values |> Seq.sumBy Folder.docCount

    let folderCount (workspace: Workspace) : int = workspace.folders.Count
