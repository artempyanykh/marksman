module Marksman.CodeActions

open type System.Environment

open FSharpPlus
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open Marksman.Misc
open Marksman.Paths
open Marksman.Names
open Marksman.Cst
open Marksman.Doc
open Marksman.Index
open Marksman.Folder
open Marksman.Refs
open Marksman.Toc
open Marksman.Structure
open Marksman.Syms

let private logger = LogProvider.getLoggerByName "CodeActions"

type DocumentAction = { name: string; newText: string; edit: Range }

let documentEdit range text documentUri : WorkspaceEdit =
    let textEdit = { NewText = text; Range = range }

    let workspaceChanges = Map.ofList [ documentUri, [| textEdit |] ]

    { Changes = Some workspaceChanges; DocumentChanges = None }

type CreateFileAction = { name: string; newFileUri: DocumentUri }

let createFile newFileUri : WorkspaceEdit =
    let documentChanges = [| DocumentChange.createFile newFileUri |]

    { Changes = None; DocumentChanges = Some documentChanges }

let tableOfContentsInner (doc: Doc) : DocumentAction option =
    match TableOfContents.mk (Doc.index doc) with
    | Some toc ->
        let rendered = TableOfContents.render toc
        let existingRange = TableOfContents.detect (Doc.text doc)

        let isSame =
            existingRange
            |> Option.map (Doc.text doc).Substring
            |> Option.map (TableOfContents.isSame rendered)
            |> Option.defaultValue false

        if isSame then
            None
        else
            let name =
                match existingRange with
                | None -> "Create a Table of Contents"
                | _ -> "Update the Table of Contents"

            let insertionPoint =
                match existingRange with
                | Some range -> Replacing range
                | None -> TableOfContents.insertionPoint doc

            logger.trace (
                Log.setMessage "Determining table of contents insertion point"
                >> Log.addContext "insertionPoint" insertionPoint
                >> Log.addContext "existing" existingRange
                >> Log.addContext "text" rendered
            )

            let isEmpty lineNumber = (Doc.text doc).LineContent(lineNumber).IsWhitespace()

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
                    let after = if isEmpty lineAfterLast then lineBreak else emptyLine

                    newRange, before, after


            let text = $"{newLinesBefore}{rendered}{newLinesAfter}"

            Some { name = name; newText = text; edit = editRange }

    | _ -> None

let tableOfContents
    (_range: Range)
    (_context: CodeActionContext)
    (doc: Doc)
    : DocumentAction option =
    tableOfContentsInner doc

let createMissingFile
    (range: Range)
    (_context: CodeActionContext)
    (doc: Doc)
    (folder: Folder)
    : CreateFileAction option =
    let configuredExts = (Folder.configuredMarkdownExts folder)

    let pos = range.Start

    monad' {
        let! atPos = Doc.index doc |> Index.linkAtPos pos

        // Extract a potential xref to another doc. We should extract only the doc part to ensure
        // that empty list of references means that the file doesn't exist
        let! symAtPos = doc.Structure |> Structure.tryFindSymbolForConcrete atPos

        let! docAtPos =
            match symAtPos with
            | Sym.Ref(CrossRef r) -> Some(r.Doc)
            | _ -> None

        let docRefAtPos = Sym.Ref(CrossRef(CrossDoc docAtPos))
        let refs = Dest.tryResolveSym folder doc docRefAtPos

        // Early return if the file exists
        do! guard (Seq.isEmpty refs)

        let! internPath = InternName.tryAsPath { name = docAtPos; src = doc.Id }

        let relPath =
            InternPath.toRel internPath
            |> RelPath.toSystem
            |> ensureMarkdownExt configuredExts
            |> RelPath

        let path = RootPath.append (Folder.rootPath folder) relPath
        let filename = AbsPath.filename path
        let uri = AbsPath.toUri path

        // create the file
        { name = $"Create `{filename}`"; newFileUri = uri }
    }

let linkToReference (range: Range) (context: CodeActionContext) (doc: Doc) : CodeAction option =
    let getExistingRefAction (link: Node<MdLink>, linkDef: Node<MdLinkDef>) : CodeAction option =
        match link.data with
        | MdLink.IL(text, _, _) ->
            Some {
                Title = $"Replace link with reference `{linkDef.data.label.text}`"
                Kind = Some CodeActionKind.RefactorRewrite
                Command = None
                Data = None
                Diagnostics = None
                Disabled = None
                IsPreferred = None
                Edit =
                    Some {
                        DocumentChanges = None
                        Changes =
                            Some
                                Map[Doc.uri doc,
                                    [|
                                        {
                                            Range = Node.range link
                                            NewText = $"[{text.text}][{linkDef.data.label.text}]"
                                        }
                                    |]]
                    }
            }
        | _ -> None

    let getNonExistingRefAction (link: Node<MdLink>) : CodeAction option =
        match link.data with
        | MdLink.IL(text, url, title) ->
            let label =
                text.text
                |> String.toLower
                |> String.replace " " "-"
                |> String.replace "_" "-"
                |> String.replace "." "-"

            let refText = $"[{label}]: {url.Value.text}"

            (* a new line at the end of the doc's text *)
            let refRange =
                let text = Doc.text doc
                let line = text.lineMap.NumLines + 1
                Range.Mk(line, 0, line + 1, refText.Length)

            Some {
                Data = None
                Disabled = None
                IsPreferred = None
                Command = None
                Title = $"Convert link to new reference `{label}`"
                Kind = Some CodeActionKind.RefactorRewrite
                Diagnostics = None
                Edit =
                    Some {
                        DocumentChanges = None
                        Changes =
                            Some
                                Map[Doc.uri doc,
                                    [|
                                        {
                                            Range = Node.range link
                                            NewText = $"[{text.text}][{label}]"
                                        }
                                        { Range = refRange; NewText = refText }
                                    |]]
                    }
            }
        | _ -> None

    let hasUrl (url: UrlEncodedNode) (x: Node<MdLinkDef>) = url.text.Equals(x.data.url.text)

    let getAction (link: Node<MdLink>) : CodeAction option =
        match link.data with
        | MdLink.IL(_, Some(url), _) ->
            let linkDef = doc.Index.linkDefs |> Seq.tryFind (hasUrl url)

            match linkDef with
            | Some(def) -> getExistingRefAction (link, def)
            | None -> getNonExistingRefAction link
        | _ -> None

    let isInRange (range: Range) (node: Node<MdLink>) =
        range.Start.Line >= node.range.Start.Line
        && range.End.Line <= node.range.End.Line

    let isInlineLink (node: Node<MdLink>) =
        match node.data with
        | MdLink.IL(_, _, _) -> true
        | _ -> false

    (* get the markdown link at the given range *)
    doc.Index.mdLinks
    |> Seq.filter (isInRange range)
    |> Seq.tryFind isInlineLink
    |> Option.bind getAction
