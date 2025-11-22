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
// $ make bench
// BenchmarkDotNet v0.14.0, macOS Sequoia 15.6.1 (24G90) [Darwin 24.6.0]
// Apple M1 Pro, 1 CPU, 10 logical and 10 physical cores
// .NET SDK 9.0.100
//   [Host]     : .NET 9.0.0 (9.0.24.52809), Arm64 RyuJIT AdvSIMD DEBUG
//   DefaultJob : .NET 9.0.0 (9.0.24.52809), Arm64 RyuJIT AdvSIMD


// | Method       | FolderSize | Mean       | Error     | StdDev    |
// |------------- |----------- |-----------:|----------:|----------:|
// | gotoDefTime  | 10         |   1.565 us | 0.0179 us | 0.0150 us |
// | findRefsTime | 10         |   9.587 us | 0.1863 us | 0.2788 us |
// | gotoDefTime  | 50         |   2.455 us | 0.0243 us | 0.0203 us |
// | findRefsTime | 50         |  50.685 us | 0.9987 us | 0.8854 us |
// | gotoDefTime  | 250        |   2.925 us | 0.0109 us | 0.0097 us |
// | findRefsTime | 250        | 339.520 us | 6.7119 us | 5.9500 us |
