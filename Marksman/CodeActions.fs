module Marksman.CodeActions

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open FSharpPlus.GenericBuilders
open Marksman.Toc
open Marksman.Workspace
open Marksman.Text
open Marksman.Misc

open type System.Console

open type System.Environment

let private logger = LogProvider.getLoggerByName "CodeActions"

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

        let name =
            match existing with
            | None -> "Create a Table of Contents"
            | _ -> "Update the Table of Contents"

        let insertionPoint =
            match existing with
            | Some (range) -> Replacing range
            | None -> TableOfContents.insertionPoint document

        logger.trace (
            Log.setMessage ("Determining table of contents insertion point")
            >> Log.addContext "insertionPoint" insertionPoint
            >> Log.addContext "existing" existing
            >> Log.addContext "text" rendered
        )

        let isEmpty lineNumber = document.text.LineContent(lineNumber).Trim().Length.Equals(0)

        let emptyLine = NewLine + NewLine
        let lineBreak = NewLine

        let (editRange, newLinesBefore, newLinesAfter) =
            match insertionPoint with
            | DocumentBeginning ->
                let after =
                    if isEmpty Text.documentBeginning.Start.Line then "" else emptyLine

                Text.documentBeginning, "", after

            | Replacing range ->
                let before = if isEmpty (range.Start.Line - 1) then "" else emptyLine
                let after = if isEmpty (range.End.Line + 1) then "" else emptyLine

                range, before, after

            | After range ->
                let lineAfterLast = range.End.Line + 1
                let newRange = Range.Mk(lineAfterLast, 0, lineAfterLast, 0)

                let before = if isEmpty range.End.Line then "" else lineBreak
                let after = if isEmpty (lineAfterLast) then lineBreak else emptyLine

                newRange, before, after


        let text = $"{newLinesBefore}{rendered}{newLinesAfter}"

        { name = name; newText = text; edit = editRange }
    }