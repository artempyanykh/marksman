module Marksman.State

open Ionide.LanguageServerProtocol.Logging
open Ionide.LanguageServerProtocol.Types

open FSharpPlus.GenericBuilders

open Marksman.Diag
open Marksman.Workspace
open Marksman.Misc

type ClientDescription =
    { info: ClientInfo option
      caps: ClientCapabilities }
    member this.IsVSCode: bool =
        this.info |> Option.exists (fun x -> x.Name = "Visual Studio Code")

    member this.IsEmacs: bool =
        this.info |> Option.exists (fun x -> x.Name = "emacs")

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

module ClientDescription =
    let ofParams (par: InitializeParams) : ClientDescription =
        let caps =
            par.Capabilities
            |> Option.defaultValue
                { Workspace = None
                  TextDocument = None
                  Experimental = None
                  InlayHint = None }

        { info = par.ClientInfo; caps = caps }


type State =
    private
        { client: ClientDescription
          workspace: Workspace
          revision: int }
    member this.Diag: WorkspaceDiag = WorkspaceDiag.mk this.workspace

module State =
    let private logger = LogProvider.getLoggerByName "State"

    let mk (client: ClientDescription) (ws: Workspace) =
        { client = client; workspace = ws; revision = 0 }

    let client s = s.client

    let workspace s = s.workspace

    let revision s = s.revision

    let diag (s: State) = s.Diag

    let tryFindFolderEnclosing (uri: PathUri) (state: State) : option<Folder> =
        Workspace.tryFindFolderEnclosing uri state.workspace

    let findFolderEnclosing (uri: PathUri) (state: State) : Folder =
        tryFindFolderEnclosing uri state
        |> Option.defaultWith (fun _ -> failwith $"Expected folder now found: {uri}")

    let tryFindDocument (uri: PathUri) (state: State) : option<Doc> =
        tryFindFolderEnclosing uri state
        |> Option.map (Folder.tryFindDoc uri)
        |> Option.flatten

    let updateFoldersFromLsp
        (added: WorkspaceFolder [])
        (removed: WorkspaceFolder [])
        (state: State)
        : State =
        logger.trace (
            Log.setMessage "Updating workspace folders"
            >> Log.addContext "numAdded" added.Length
            >> Log.addContext "numRemoved" removed.Length
        )

        let removedUris = removed |> Array.map (fun f -> PathUri.fromString f.Uri)

        let addedFolders =
            seq {
                for f in added do
                    let rootUri = PathUri.fromString f.Uri

                    let folder = Folder.tryLoad f.Name rootUri

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

    let updateDocument (newDocument: Doc) (state: State) : State =
        let folder = findFolderEnclosing newDocument.path state

        let newDocs = folder.docs |> Map.add newDocument.path newDocument

        let newFolder = { folder with docs = newDocs }

        let newWs = Workspace.withFolder newFolder state.workspace

        { client = state.client
          workspace = newWs
          revision = state.revision + 1 }


    let removeDocument (path: PathUri) (state: State) : State =
        let folder = findFolderEnclosing path state

        let newFolder = Folder.removeDoc path folder

        let newWs = Workspace.withFolder newFolder state.workspace

        { client = state.client
          workspace = newWs
          revision = state.revision + 1 }
