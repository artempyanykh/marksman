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
// BenchmarkDotNet v0.15.6, Linux Arch Linux
// 13th Gen Intel Core i7-1370P 0.40GHz, 1 CPU, 20 logical and 14 physical cores
// .NET SDK 10.0.100
//   [Host]     : .NET 10.0.0 (10.0.0, 42.42.42.42424), X64 RyuJIT x86-64-v3 DEBUG
//   DefaultJob : .NET 10.0.0 (10.0.0, 42.42.42.42424), X64 RyuJIT x86-64-v3
//
//
// | Method       | FolderSize | Mean       | Error     | StdDev    |
// |------------- |----------- |-----------:|----------:|----------:|
// | gotoDefTime  | 10         |   2.916 us | 0.0300 us | 0.0266 us |
// | findRefsTime | 10         |  17.125 us | 0.2131 us | 0.1993 us |
// | gotoDefTime  | 50         |   4.011 us | 0.0248 us | 0.0207 us |
// | findRefsTime | 50         |  84.928 us | 0.6689 us | 0.5222 us |
// | gotoDefTime  | 250        |   5.158 us | 0.0143 us | 0.0127 us |
// | findRefsTime | 250        | 641.349 us | 5.2700 us | 4.9296 us |
