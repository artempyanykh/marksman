module Marksman.Helpers

open System.Runtime.InteropServices
open Snapper
open Marksman.Misc
open Marksman.Workspace
open Marksman.CodeActions

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

let applyDocumentAction (doc: Doc) (action: DocumentAction): string =
    let (before, after) = doc.text.Cutout(action.edit)

    before + action.newText + after

let stripMargin (str: string) = 
    let lines = str.Lines()
    let modified = lines |> Array.map(fun line -> 
        System.Text.RegularExpressions.Regex.Replace(line, "^[\s]*\|", "")
    )
    String.concat System.Environment.NewLine modified

let stripMarginTrim (str: string) = 
    stripMargin (str.Trim())

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
