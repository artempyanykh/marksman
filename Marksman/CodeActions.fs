module Marksman.CodeActions

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open Marksman.Toc
open Marksman.Workspace
open Marksman.Misc

open type System.Environment

let private logger = LogProvider.getLoggerByName "CodeActions"

type DocumentAction = { name: string; newText: string; edit: Range }

let documentEdit range text documentUri : WorkspaceEdit =
    let textEdit = { NewText = text; Range = range }

    let workspaceChanges = Map.ofList [ documentUri, [| textEdit |] ]

    { Changes = Some workspaceChanges; DocumentChanges = None }

let tableOfContents (document: Doc) : DocumentAction option =
    match TableOfContents.mk document.index with
    | Some toc ->
        let rendered = TableOfContents.render toc
        let existingRange = TableOfContents.detect document.text

        let existingText =
            existingRange
            |> Option.map document.text.Substring
            |> Option.map (fun x -> x.Trim())

        if existingText = Some rendered then
            None
        else
            let name =
                match existingRange with
                | None -> "Create a Table of Contents"
                | _ -> "Update the Table of Contents"

            let insertionPoint =
                match existingRange with
                | Some range -> Replacing range
                | None -> TableOfContents.insertionPoint document

            logger.trace (
                Log.setMessage "Determining table of contents insertion point"
                >> Log.addContext "insertionPoint" insertionPoint
                >> Log.addContext "existing" existingRange
                >> Log.addContext "text" rendered
            )

            let isEmpty lineNumber = document.text.LineContent(lineNumber).IsWhitespace()

            let emptyLine = NewLine + NewLine
            let lineBreak = NewLine

            let editRange, newLinesBefore, newLinesAfter =
                match insertionPoint with
                | DocumentBeginning ->
                    let after =
                        if isEmpty Text.documentBeginning.Start.Line then "" else emptyLine

                    Text.documentBeginning, "", after

                | Replacing range ->
                    let before =
                        if range.Start.Line <= 0 || isEmpty (range.Start.Line - 1) then
                            ""
                        else
                            emptyLine

                    let after = if isEmpty (range.End.Line + 1) then "" else emptyLine

                    range, before, after

                | After range ->
                    let lineAfterLast = range.End.Line + 1
                    let newRange = Range.Mk(lineAfterLast, 0, lineAfterLast, 0)

                    let before = if isEmpty range.End.Line then "" else lineBreak
                    let after = if isEmpty (lineAfterLast) then lineBreak else emptyLine

                    newRange, before, after


            let text = $"{newLinesBefore}{rendered}{newLinesAfter}"

            Some { name = name; newText = text; edit = editRange }

    | _ -> None
