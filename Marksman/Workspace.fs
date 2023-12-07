module Marksman.Workspace

open Marksman.Config
open Marksman.Misc
open Marksman.Paths
open Marksman.Names
open Marksman.Folder

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

    let withoutFolder (keyPath: FolderId) (workspace: Workspace) : Workspace =
        { workspace with folders = Map.remove keyPath workspace.folders }

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

    let docCount (workspace: Workspace) : int =
        workspace.folders.Values |> Seq.sumBy Folder.docCount

    let folderCount (workspace: Workspace) : int = workspace.folders.Count
