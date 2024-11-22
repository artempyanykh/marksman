module Marksman.Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Config
open Marksman.Names
open Marksman.Paths
open Marksman.Cst
open Marksman.Doc
open Marksman.Index
open Marksman.Refs
open Marksman.Folder

type ReferenceResolution() =
    let mkFolder size : Folder =
        let folderRoot =
            (if isWindows then "C:\\docs" else "/docs") |> AbsPath.ofSystem

        let folderId =
            let uri = folderRoot |> AbsPath.toUri
            UriWith.mkRoot uri

        let docs =
            Array.init size (fun i ->
                let docId =
                    DocId(UriWith.mkRooted folderId (LocalPath.ofSystem $"doc{i}.md"))

                let links = List.init size (fun toDoc -> $"[[doc{toDoc}.md]]")
                let contentLines = $"# Doc {i}" :: links
                let content = String.concat "\n" contentLines
                let text = Text.mkText content
                Doc.mk ParserSettings.Default docId None text)

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

        let refs = Dest.tryResolveElement this.Folder doc link |> Seq.toArray
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
// $ dotnet run -c Release --project Benchmarks/Benchmarks.fsproj
// BenchmarkDotNet=v0.13.5, OS=macOS Ventura 13.3.1 (22E261) [Darwin 22.4.0]
// BenchmarkDotNet v0.14.0, macOS Sonoma 14.6.1 (23G93) [Darwin 23.6.0]
// Apple M1 Pro, 1 CPU, 10 logical and 10 physical cores
// .NET SDK 8.0.402
//   [Host]     : .NET 8.0.8 (8.0.824.36612), Arm64 RyuJIT AdvSIMD DEBUG
//   DefaultJob : .NET 8.0.8 (8.0.824.36612), Arm64 RyuJIT AdvSIMD
//
//
// | Method       | FolderSize | Mean       | Error     | StdDev     |
// |------------- |----------- |-----------:|----------:|-----------:|
// | gotoDefTime  | 10         |   1.683 us | 0.0171 us |  0.0160 us |
// | findRefsTime | 10         |  10.410 us | 0.1307 us |  0.1223 us |
// | gotoDefTime  | 50         |   2.428 us | 0.0377 us |  0.0334 us |
// | findRefsTime | 50         |  58.423 us | 1.0584 us |  0.9901 us |
// | gotoDefTime  | 250        |   3.247 us | 0.0379 us |  0.0355 us |
// | findRefsTime | 250        | 388.914 us | 7.6202 us | 11.1697 us |
