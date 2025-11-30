module Marksman.Fatality

open System.Reflection
open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.State
open Marksman.Workspace

let abort (stateOpt: Option<State>) (ex: exn) =
    let marksmanAssembly = typeof<State>.Assembly

    let marksmanVersion =
        marksmanAssembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
        |> Option.ofObj
        |> Option.map (fun attr -> attr.InformationalVersion)
        |> Option.defaultValue (marksmanAssembly.GetName().Version.ToString())

    let clientDebugOut ({ Name = name; Version = versionOpt }: ClientInfo) =
        eprintf $"Client: {name}"
        Option.iter (fun v -> eprintf $"{v}") versionOpt
        eprintfn ""

    let stateDebugOut (state: State) =
        Option.iter clientDebugOut (State.client state).info
        let workspace = State.workspace state
        let stateRevision = State.revision state

        let userConfigString =
            if Option.isSome (Workspace.userConfig workspace) then
                "present"
            else
                "absent"

        eprintfn "Workspace:"
        eprintfn $"  Revision      : {stateRevision}"
        eprintfn $"  Folder count  : {Workspace.folderCount workspace}"
        eprintfn $"  Document count: {Workspace.docCount workspace}"
        eprintfn $"  User config   : {userConfigString}"

    eprintfn "---------------------------------------------------------------------------"
    eprintfn "Marksman encountered a fatal error"
    eprintfn "Please, report the error at https://github.com/artempyanykh/marksman/issues"
    eprintfn "---------------------------------------------------------------------------"
    eprintfn $"Marksman version: {marksmanVersion}"
    eprintfn $"OS: {System.Runtime.InteropServices.RuntimeInformation.OSDescription}"
    eprintfn $"Arch: {System.Runtime.InteropServices.RuntimeInformation.OSArchitecture}"
    Option.iter stateDebugOut stateOpt
    eprintfn "---------------------------------------------------------------------------"
    ex.Message.Lines() |> Array.iter (fun l -> eprintfn $"{l}")
    eprintfn $"{ex.StackTrace.ToString()}"
    exit 1
