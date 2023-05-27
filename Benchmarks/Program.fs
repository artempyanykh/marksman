module Marksman.Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Paths
open Marksman.Workspace
open Marksman.Index
open Marksman.Refs
open Marksman.Cst

type ReferenceResolution() =
    let mkFolder size : Folder =
        let folderRoot =
            (if isWindows then "C:\\docs" else "/docs") |> AbsPath.ofSystem

        let folderId =
            let uri = folderRoot |> AbsPath.toUri
            UriWith.mkRoot uri

        let docs =
            Array.init size (fun i ->
                let docId = UriWith.mkRooted folderId (LocalPath.ofSystem $"doc{i}.md")
                let links = List.init size (fun toDoc -> $"[[doc{toDoc}.md]]")
                let contentLines = $"# Doc {i}" :: links
                let content = String.concat "\n" contentLines
                let text = Text.mkText content
                let doc = Doc.mk docId None text
                doc.Id.data.path, doc)

        let docs = Map.ofArray docs
        Folder.multiFile "docs" folderId docs None

    [<Params(10, 50, 250)>]
    member val public FolderSize: int = 10 with get, set

    member val public Folder: Folder = mkFolder 10 with get, set

    [<GlobalSetup>]
    member this.setup() = this.Folder <- mkFolder this.FolderSize

    [<Benchmark>]
    member this.gotoDefTime() =
        let doc =
            Folder.tryFindDocByRelPath (RelPath "doc1.md") this.Folder
            |> Option.get

        let link =
            Doc.index doc |> Index.linkAtPos (Position.Mk(1, 3)) |> Option.get

        let uref = Uref.ofElement [| "md" |] doc.Id link |> Option.get
        let refs = Dest.tryResolveUref uref doc this.Folder |> Seq.toArray
        refs |> ignore

    [<Benchmark>]
    member this.findRefsTime() =
        let doc =
            Folder.tryFindDocByRelPath (RelPath "doc1.md") this.Folder
            |> Option.get

        let header =
            Cst.elementAtPos (Position.Mk(0, 3)) (Doc.cst doc) |> Option.get

        let refs = Dest.findElementRefs true this.Folder doc header |> Seq.toArray
        refs |> ignore


[<EntryPoint>]
let main _ =
    // Uncomment to debug the benchmark routines
    // ReferenceResolution().gotoDefTime()
    // ReferenceResolution().findRefsTime()
    BenchmarkRunner.Run<ReferenceResolution>() |> ignore
    0

// BenchmarkDotNet=v0.13.5, OS=macOS Ventura 13.3.1 (22E261) [Darwin 22.4.0]
// Apple M1 Pro, 1 CPU, 10 logical and 10 physical cores
// .NET SDK=7.0.302
//   [Host]     : .NET 7.0.5 (7.0.523.17405), Arm64 RyuJIT AdvSIMD DEBUG
//   DefaultJob : .NET 7.0.5 (7.0.523.17405), Arm64 RyuJIT AdvSIMD
//
//
// |       Method | FolderSize |             Mean |        Error |       StdDev |
// |------------- |----------- |-----------------:|-------------:|-------------:|
// |  gotoDefTime |         10 |         12.69 us |     0.021 us |     0.019 us |
// | findRefsTime |         10 |      1,392.42 us |     2.681 us |     2.377 us |
// |  gotoDefTime |         50 |         62.92 us |     0.078 us |     0.073 us |
// | findRefsTime |         50 |    164,905.21 us |   229.081 us |   214.283 us |
// |  gotoDefTime |        250 |        319.72 us |     0.575 us |     0.510 us |
// | findRefsTime |        250 | 20,683,207.26 us | 6,810.020 us | 6,036.905 us |
