module Marksman.ConnectionUpdateBenchmarks

open BenchmarkDotNet.Attributes
open Marksman.Conn
open Marksman.Config
open Marksman.Doc
open Marksman.Folder
open Marksman.Misc
open Marksman.Names
open Marksman.Paths

/// Documents are parsed in setup. Each invocation edits the same immutable input
/// folder, so warmup and measurement never turn an edit into a no-op.
[<MemoryDiagnoser>]
type ConnectionUpdates() =
    let mutable runScenario: unit -> Folder =
        fun () -> failwith "Run setup first"

    let mutable connectionVersions: (Conn * Conn) option = None

    [<Params(100, 1000)>]
    member val FolderSize = 100 with get, set

    [<Params("ProseEdit",
             "LinkEdit",
             "HeadingEdit",
             "RenameTitle",
             "RenameUnlinkedTitle",
             "AddAmbiguousPath",
             "RemoveDocument",
             "ReorderTitles",
             "HeadingWithBrokenLinks",
             "FullRebuild")>]
    member val Scenario = "ProseEdit" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let root = (if isWindows then "C:\\docs" else "/docs") |> AbsPath.ofSystem
        let folderId = root |> AbsPath.toUri |> UriWith.mkRoot

        let config = {
            Config.Default with
                coreIncrementalReferences = Some true
                coreParanoid = Some false
        }

        let mkDoc path lines =
            let id = DocId(UriWith.mkRooted folderId (LocalPath.ofSystem path))
            let text = String.concat "\n" lines |> Text.mkText
            Doc.mk (ParserSettings.OfConfig config) id None text

        let path i = $"group{i % 10}/doc{i}.md"

        let lines i = [
            yield $"# Title {i}"
            if i = 0 then
                yield "# Alternate"
            yield "## Section"
            yield ""
            for offset in 1..8 do
                let target = (i + offset) % this.FolderSize

                if offset % 2 = 0 then
                    yield $"[[doc{target}#Section]]"
                else
                    yield $"[[Title {target}#Section]]"
            if this.Scenario = "HeadingWithBrokenLinks" then
                yield "[[missing#Absent]]"
            yield ""
            yield "Some prose."
        ]

        let docs = Array.init this.FolderSize (fun i -> mkDoc (path i) (lines i))

        let original =
            if this.Scenario = "RenameUnlinkedTitle" then
                let unlinked = mkDoc "unlinked.md" [ "# Unlinked" ]
                Folder.multiFile "benchmark" folderId (Seq.append docs [ unlinked ]) (Some config)
            else
                Folder.multiFile "benchmark" folderId docs (Some config)

        let replaceFirstDocument lines =
            let updated = mkDoc (path 0) lines
            fun () -> Folder.withDoc updated original

        runScenario <-
            match this.Scenario with
            | "ProseEdit" -> replaceFirstDocument (lines 0 @ [ "More prose." ])
            | "LinkEdit" -> replaceFirstDocument (lines 0 @ [ "[[doc42#Section]]" ])
            | "HeadingEdit"
            | "HeadingWithBrokenLinks" -> replaceFirstDocument (lines 0 @ [ "## New section" ])
            | "RenameTitle" ->
                replaceFirstDocument (
                    lines 0
                    |> List.map (fun line -> if line = "# Title 0" then "# Renamed" else line)
                )
            | "RenameUnlinkedTitle" ->
                let updated = mkDoc "unlinked.md" [ "# Renamed unlinked" ]
                fun () -> Folder.withDoc updated original
            | "AddAmbiguousPath" ->
                let added = mkDoc "doc0.md" [ "# Different title"; "## Section" ]
                fun () -> Folder.withDoc added original
            | "RemoveDocument" -> fun () -> Folder.withoutDoc docs[0].Id original |> Option.get
            | "ReorderTitles" ->
                replaceFirstDocument ("# Alternate" :: "# Title 0" :: (lines 0 |> List.skip 2))
            | "FullRebuild" -> fun () -> Folder.multiFile "benchmark" folderId docs (Some config)
            | scenario -> invalidArg "Scenario" scenario

        connectionVersions <- Some(Folder.conn original, Folder.conn (runScenario ()))

    [<Benchmark>]
    member _.Update() = runScenario ()

    [<Benchmark>]
    member _.CompareReferenceResolutions() =
        let before, after = connectionVersions.Value
        Query.documentsWithChangedReferenceResolutions before after
