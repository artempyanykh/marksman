module Marksman.Helpers

open System.Runtime.InteropServices
open Snapper
open Marksman.Misc
open Marksman.Workspace

let pathToUri path = $"file://{path}"

let dummyRoot =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        "c:/"
    else
        "/"

let dummyRootUri = pathToUri dummyRoot

let dummyRootPath pathComps = dummyRoot + (pathComps |> String.concat "/")

let checkInlineSnapshot (fmt: 'a -> string) (things: seq<'a>) (snapshot: seq<string>) =
    let lines = Seq.map (fun x -> (fmt x).Lines()) things |> Seq.concat

    lines.ShouldMatchInlineSnapshot(snapshot)

type FakeDoc =
    class
        static member Mk(content: string, ?path: string) : Doc =
            let text = Text.mkText content
            let path = defaultArg path "fake.md"
            let pathComp = path.TrimStart('/').Split("/") |> List.ofArray
            let path = dummyRootPath pathComp
            let pathUri = pathToUri path
            let rootUri = dummyRoot |> pathToUri
            Doc.mk (PathUri.fromString pathUri) (PathUri.fromString rootUri) None text

        static member Mk(contentLines: array<string>, ?path: string) : Doc =
            let content = String.concat System.Environment.NewLine contentLines
            FakeDoc.Mk (content, ?path = path)
    end

type FakeFolder =
    class
        static member Mk(docs: seq<Doc>) : Folder =
            let docsMap = docs |> Seq.map (fun d -> d.path, d) |> Map.ofSeq

            { name = "dummy"
              root = PathUri.fromString dummyRootUri
              docs = docsMap }
    end
