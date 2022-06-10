module Marksman.CodeActions

open Ionide.LanguageServerProtocol.Types

open FSharpPlus.GenericBuilders
open Marksman.Toc
open Marksman.Workspace
open Marksman.Text
open Marksman.Misc

open type System.Environment

type DocumentAction = { name: string; newText: string; edit: Range }

let documentEdit range text documentUri : WorkspaceEdit =
    let textEdit = { NewText = text; Range = range }

    let mp = Map.ofList [ documentUri, [| textEdit |] ]

    { Changes = Some mp; DocumentChanges = None }

let tableOfContents (document: Doc) : DocumentAction option =
    monad' {
        let! toc = Toc.TableOfContents.mk document.index

        let rendered = TableOfContents.render toc
        let existing = TableOfContents.detect document.text
        let insertion = TableOfContents.insertionPoint document


        let name =
            match existing with
            | None -> "Create a Table of Contents"
            | _ -> "Update the Table of Contents"

        let insertionPoint =
            match existing with
            | Some (range) -> Replacing range
            | None -> insertion

        let text =
            match insertion with
            | Replacing _ -> rendered
            | After _ -> NewLine + rendered

        let editRange =
            match insertionPoint with
            | Replacing range -> range
            | After range ->
                let lineAfterLast = range.End.Line + 1
                Range.Mk(lineAfterLast, 0, lineAfterLast, 0)

        { name = name; newText = text; edit = editRange }
    }
