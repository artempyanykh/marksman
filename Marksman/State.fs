﻿module Marksman.State

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types

open FSharpPlus.GenericBuilders

open Marksman.Diag
open Marksman.Workspace
open Marksman.Paths
open Marksman.Config
open Newtonsoft.Json.Linq

type InitOptions = { preferredTextSyncKind: Option<TextSync> }

module InitOptions =
    let empty = { preferredTextSyncKind = None }

    let ofJson (json: JToken) : InitOptions =
        match json.SelectToken("preferredTextSyncKind") with
        | null -> empty
        | kindNum ->
            let kind =
                try
                    Some(int64 kindNum)
                with _ ->
                    None

            let kind =
                kind
                |> Option.bind (function
                    | x when x = int64 TextDocumentSyncKind.Full -> Some Full
                    | x when x = int64 TextDocumentSyncKind.Incremental -> Some Incremental
                    | _ -> None)

            { preferredTextSyncKind = kind }

type ClientDescription =
    { info: ClientInfo option
      caps: ClientCapabilities
      opts: InitOptions }

    member this.IsVSCode: bool =
        this.info |> Option.exists (fun x -> x.Name = "Visual Studio Code")

    member this.IsEmacs: bool =
        this.info |> Option.exists (fun x -> x.Name = "emacs")

    member this.SupportsDocumentEdit: bool =
        let docChange =
            monad' {
                let! ws = this.caps.Workspace
                let! edit = ws.WorkspaceEdit
                return! edit.DocumentChanges
            }

        docChange = Some true

    member this.SupportsStatus: bool =
        match this.caps.Experimental with
        | None -> false
        | Some exp -> exp.Value<bool>("statusNotification")

    member this.SupportsHierarchy: bool =
        monad' {
            let! textDoc = this.caps.TextDocument
            let! docSymbol = textDoc.DocumentSymbol
            return! docSymbol.HierarchicalDocumentSymbolSupport
        }
        |> Option.defaultValue false

    member this.SupportsPrepareRename: bool =
        monad' {
            let! textDoc = this.caps.TextDocument
            let! rename = textDoc.Rename
            return! rename.PrepareSupport
        }
        |> Option.defaultValue false

    member this.PreferredTextSyncKind: Option<TextSync> =
        this.opts.preferredTextSyncKind

module ClientDescription =
    let ofParams (par: InitializeParams) : ClientDescription =
        let caps =
            par.Capabilities
            |> Option.defaultValue { Workspace = None; TextDocument = None; Experimental = None }

        let opts =
            par.InitializationOptions
            |> Option.map InitOptions.ofJson
            |> Option.defaultValue InitOptions.empty

        { info = par.ClientInfo; caps = caps; opts = opts }

    let empty =
        { info = None
          caps = { Workspace = None; TextDocument = None; Experimental = None }
          opts = InitOptions.empty }

type State =
    private
        { client: ClientDescription
          workspace: Workspace
          revision: int }

    member this.Diag() : WorkspaceDiag = WorkspaceDiag.mk this.workspace

module State =
    let private logger = LogProvider.getLoggerByName "State"

    let mk (client: ClientDescription) (ws: Workspace) =
        { client = client; workspace = ws; revision = 0 }

    let client s = s.client

    let clientCaps s = s.client.caps

    let workspace s = s.workspace

    let userConfigOrDefault s =
        Workspace.userConfig s.workspace |> Option.defaultValue Config.Default

    let revision s = s.revision

    let diag (s: State) = s.Diag()

    let tryFindFolderEnclosing (uri: PathUri) (state: State) : option<Folder> =
        Workspace.tryFindFolderEnclosing uri state.workspace

    let findFolderEnclosing (uri: PathUri) (state: State) : Folder =
        tryFindFolderEnclosing uri state
        |> Option.defaultWith (fun _ -> failwith $"Expected folder not found: {uri}")

    let tryFindFolderAndDoc (uri: PathUri) (state: State) : option<Folder * Doc> =
        tryFindFolderEnclosing uri state
        |> Option.bind (fun folder ->
            Folder.tryFindDocByPath uri folder
            |> Option.map (fun doc -> folder, doc))

    let tryFindDoc (uri: PathUri) (state: State) : option<Doc> =
        match tryFindFolderAndDoc uri state with
        | None -> None
        | Some (_, doc) -> Some doc

    let updateFoldersFromLsp
        (added: WorkspaceFolder[])
        (removed: WorkspaceFolder[])
        (state: State)
        : State =
        logger.trace (
            Log.setMessage "Updating workspace folders"
            >> Log.addContext "numAdded" added.Length
            >> Log.addContext "numRemoved" removed.Length
        )

        let removedUris =
            removed
            |> Array.map (fun f -> PathUri.ofString f.Uri |> FolderId.ofPath)

        let userConfig = workspace state |> Workspace.userConfig

        let addedFolders =
            seq {
                for f in added do
                    let rootUri = RootPath.ofString f.Uri

                    let folder = Folder.tryLoad userConfig f.Name rootUri

                    match folder with
                    | Some folder -> yield folder
                    | _ -> ()
            }

        let newWorkspace =
            Workspace.withoutFolders removedUris state.workspace
            |> Workspace.withFolders addedFolders

        { client = state.client
          workspace = newWorkspace
          revision = state.revision + 1 }

    let updateFolder (newFolder: Folder) (state: State) : State =
        let newWs = Workspace.withFolder newFolder state.workspace
        { state with workspace = newWs; revision = state.revision + 1 }

    let removeFolder (keyPath: FolderId) (state: State) : State =
        let newWs = Workspace.withoutFolder keyPath state.workspace
        { state with workspace = newWs; revision = state.revision + 1 }
