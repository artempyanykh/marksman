module Marksman.DiagnosticUpdateBenchmarks

open BenchmarkDotNet.Attributes
open Marksman.Config
open Marksman.Diag
open Marksman.Doc
open Marksman.Folder
open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Server
open Marksman.State
open Marksman.Workspace

/// Measures diagnostic calculation after parsing and folder updates have finished.
[<MemoryDiagnoser>]
type DiagnosticUpdates() =
    let mutable previous: (State * WorkspaceDiag) option = None
    let mutable current: State option = None

    [<Params(100, 1000)>]
    member val FolderSize = 100 with get, set

    [<Params("Initial", "ProseEdit", "LinkEdit", "TargetHeadingEdit")>]
    member val Scenario = "Initial" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let root = (if isWindows then "C:\\docs" else "/docs") |> AbsPath.ofSystem
        let folderId = root |> AbsPath.toUri |> UriWith.mkRoot
        let config = { Config.Default with coreIncrementalReferences = Some true }

        let mkDoc path lines =
            let id = DocId(UriWith.mkRooted folderId (LocalPath.ofSystem path))
            let text = String.concat "\n" lines |> Text.mkText
            Doc.mk (ParserSettings.OfConfig config) id None text

        let path i = $"group{i % 10}/doc{i}.md"

        let lines i = [
            yield $"# Title {i}"
            yield "## Section"
            yield ""
            for offset in 1..8 do
                let target = (i + offset) % this.FolderSize
                if offset % 2 = 0 then
                    yield $"[[doc{target}#Section]]"
                else
                    yield $"[[Title {target}#Section]]"
            yield "[[missing#Absent]]"
            yield ""
            yield "Some prose."
        ]

        let docs = Array.init this.FolderSize (fun i -> mkDoc (path i) (lines i))
        let before = Folder.multiFile "benchmark" folderId docs (Some config)

        let after =
            let changedLines =
                match this.Scenario with
                | "Initial" -> lines 0
                | "ProseEdit" -> lines 0 @ [ "More prose." ]
                | "LinkEdit" -> lines 0 @ [ "[[another-missing]]" ]
                | "TargetHeadingEdit" ->
                    lines 0
                    |> List.map (fun line -> if line = "## Section" then "## Renamed" else line)
                | scenario -> invalidArg "Scenario" scenario

            if this.Scenario = "Initial" then
                before
            else
                Folder.withDoc (mkDoc (path 0) changedLines) before

        let state folder =
            Workspace.ofFolders None [ folder ] |> State.mk ClientDescription.empty

        previous <-
            if this.Scenario = "Initial" then
                None
            else
                let beforeState = state before
                let diagnostics, _ = WorkspaceDiag.calculate None (State.workspace beforeState)
                Some(beforeState, diagnostics)

        current <- Some(state after)

    [<Benchmark>]
    member _.Calculate() =
        calcDiagnosticsUpdate previous current.Value

    [<Benchmark>]
    member _.FindAffectedDocuments() =
        match previous with
        | None -> Map.empty
        | Some(before, _) ->
            WorkspaceDiag.affectedDocuments (State.workspace before) (State.workspace current.Value)
