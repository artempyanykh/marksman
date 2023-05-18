module Marksman.Helpers

open System.Runtime.InteropServices
open Snapper
open Marksman.Misc
open Marksman.Paths
open Marksman.Workspace
open Marksman.CodeActions

let pathToUri (path: string) = $"file://{path}"

let dummyRoot =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        "c:/"
    else
        "/"

let dummyRootUri = pathToUri dummyRoot

let dummyRootPath pathComps = dummyRoot + (pathComps |> String.concat "/")

let pathComps (path: string) = path.TrimStart('/').Split("/") |> List.ofArray

let checkInlineSnapshot (fmt: 'a -> string) (things: seq<'a>) (snapshot: seq<string>) =
    let lines = Seq.map (fun x -> (fmt x).Lines()) things |> Seq.concat

    lines.ShouldMatchInlineSnapshot(snapshot)

let applyDocumentAction (doc: Doc) (action: DocumentAction) : string =
    let before, after = (Doc.text doc).Cutout(action.edit)

    before + action.newText + after

let stripMargin (str: string) =
    let lines = str.Lines()

    lines
    |> Array.map (fun line -> System.Text.RegularExpressions.Regex.Replace(line, "^[\s]*\|", ""))
    |> concatLines

let stripMarginTrim (str: string) = stripMargin (str.Trim())

type FakeDoc =
    class
        static member Mk(content: string, ?path: string, ?root: string) : Doc =
            let text = Text.mkText content
            let path = defaultArg path "fake.md"
            let pathUri = pathToUri (dummyRootPath (pathComps path))
            let root = Option.map pathComps root |> Option.defaultValue []
            let rootUri = dummyRootPath root |> pathToUri

            Doc.mk (PathUri.ofString pathUri) (RootPath.ofString rootUri) None text

        static member Mk(contentLines: array<string>, ?path: string) : Doc =
            let content = String.concat System.Environment.NewLine contentLines
            FakeDoc.Mk(content, ?path = path)
    end

type FakeFolder =
    class
        static member Mk(docs: seq<Doc>, ?config: Config.Config) : Folder =
            let docsMap = docs |> Seq.map (fun d -> Doc.path d, d) |> Map.ofSeq

            Folder.multiFile "dummy" (RootPath.ofString dummyRootUri) docsMap config
    end
