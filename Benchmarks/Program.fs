module Marksman.Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
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
                Doc.mk Config.defaultMarkdownExtensions docId None text)

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

// BenchmarkDotNet=v0.13.5, OS=macOS Ventura 13.3.1 (22E261) [Darwin 22.4.0]
// Apple M1 Pro, 1 CPU, 10 logical and 10 physical cores
// .NET SDK=7.0.302
//   [Host]     : .NET 7.0.5 (7.0.523.17405), Arm64 RyuJIT AdvSIMD DEBUG [AttachedDebugger]
//   DefaultJob : .NET 7.0.5 (7.0.523.17405), Arm64 RyuJIT AdvSIMD
//
//
// |       Method | FolderSize |           Mean |       Error |      StdDev |
// |------------- |----------- |---------------:|------------:|------------:|
// |  gotoDefTime |         10 |       1.180 us |   0.0034 us |   0.0032 us |
// | findRefsTime |         10 |     147.967 us |   0.7200 us |   0.6735 us |
// |  gotoDefTime |         50 |       1.294 us |   0.0013 us |   0.0011 us |
// | findRefsTime |         50 |   4,040.372 us |   4.7052 us |   4.4013 us |
// |  gotoDefTime |        250 |       1.433 us |   0.0022 us |   0.0019 us |
// | findRefsTime |        250 | 115,387.334 us | 118.0649 us | 104.6614 us |
