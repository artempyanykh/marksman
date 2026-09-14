module Marksman.Diag

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Conn
open Marksman.Folder
open Marksman.Syms
open Marksman.Workspace

module Lsp = Ionide.LanguageServerProtocol.Types

open Marksman.Cst
open Marksman.Index
open Marksman.Refs

type Entry =
    | AmbiguousLink of Element * Syms.Ref * array<Dest>
    | BrokenLink of Element * Syms.Ref
    | NonBreakableWhitespace of Lsp.Range

let code: Entry -> string =
    function
    | AmbiguousLink _ -> "1"
    | BrokenLink _ -> "2"
    | NonBreakableWhitespace _ -> "3"

let checkNonBreakingWhitespace (doc: Doc) =
    let nonBreakingWhitespace = "\u00a0"

    let headings = [ 1..7 ] |> List.map (fun n -> (String.replicate n "#"))

    [ 0 .. (Doc.text doc).lineMap.NumLines ]
    |> List.collect (fun x ->
        let line = (Doc.text doc).LineContent x

        let headingLike =
            List.tryFind (fun (h: string) -> line.StartsWith(h + nonBreakingWhitespace)) headings

        match headingLike with
        | None -> []
        | Some heading ->
            let whitespaceRange: Lsp.Range = {
                Start = { Line = x; Character = heading.Length }
                End = { Line = x; Character = heading.Length + 1 }
            }

            [ NonBreakableWhitespace(whitespaceRange) ])

let checkLink (folder: Folder) (doc: Doc) (linkEl: Element) : seq<Entry> =
    let exts = Folder.configuredMarkdownExts folder

    let ref =
        doc.Structure
        |> Structure.Structure.tryFindSymbolForConcrete linkEl
        |> Option.bind Syms.Sym.asRef

    match ref with
    | None -> []
    | Some ref ->
        let refs = Dest.tryResolveElement folder doc linkEl |> Array.ofSeq

        if Folder.isSingleFile folder && Syms.Ref.isCross ref then
            []
        else if refs.Length = 1 then
            []
        else if refs.Length = 0 then
            match linkEl with
            // Inline shortcut links often are a part of regular text.
            // Raising diagnostics on them would be noisy.
            | ML { data = MdLink.RS _ } -> []
            | ML { data = MdLink.IL(_, url, _) } ->
                match url with
                | Some { data = url } ->
                    // Inline links to docs that don't look like a markdown file should not
                    // produce diagnostics
                    if Misc.isMarkdownFile exts (UrlEncoded.decode url) then
                        [ BrokenLink(linkEl, ref) ]
                    else
                        []
                | _ -> [ BrokenLink(linkEl, ref) ]
            | _ -> [ BrokenLink(linkEl, ref) ]
        else
            [ AmbiguousLink(linkEl, ref, refs) ]

let checkLinks (folder: Folder) (doc: Doc) : seq<Entry> =
    let links = Doc.index >> Index.links <| doc
    links |> Seq.collect (checkLink folder doc)

let checkDoc (folder: Folder) (doc: Doc) : list<Entry> =
    seq {
        yield! checkLinks folder doc
        yield! checkNonBreakingWhitespace doc
    }
    |> List.ofSeq

let destToHuman (ref: Dest) : string =
    match ref with
    | Dest.Doc { doc = doc } -> $"document {Doc.name doc}"
    | Dest.Heading(docLink, { data = heading }) ->
        $"heading {Heading.name heading} in the document {Doc.name (DocLink.doc docLink)}"
    | Dest.LinkDef(_, { data = ld }) -> $"link definition {MdLinkDef.name ld}"
    | Dest.Tag(doc, { data = tag }) -> $"tag {tag.name} in the document {Doc.name doc}"

let docToHuman (name: string) : string = $"document '{name}'"

let refToHuman (ref: Syms.Ref) : string =
    match ref with
    | Syms.CrossRef(Syms.CrossDoc docName) -> docToHuman docName
    | Syms.CrossRef(Syms.CrossSection(docName, sectionName)) ->
        $"heading '{Slug.toString sectionName}' in {docToHuman docName}"
    | Syms.IntraRef(Syms.IntraSection heading) -> $"heading '{Slug.toString heading}'"
    | Syms.IntraRef(Syms.IntraLinkDef ld) -> $"link definition with the label '{ld}'"

let diagToLsp (diag: Entry) : Lsp.Diagnostic =
    match diag with
    | AmbiguousLink(el, ref, dests) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | T _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let mkRelated dest : DiagnosticRelatedInformation =
            let loc = Dest.location dest
            let msg = $"Duplicate definition of {refToHuman ref}"
            { Location = loc; Message = msg }

        let related = dests |> Array.map mkRelated

        {
            Range = Element.range el
            Severity = Some severity
            Code = Some(code diag)
            CodeDescription = None
            Source = Some "Marksman"
            Message = $"Ambiguous link to {refToHuman ref}"
            RelatedInformation = Some related
            Tags = None
            Data = None
        }
    | BrokenLink(el, ref) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | T _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let msg = $"Link to non-existent {refToHuman ref}"

        {
            Range = Element.range el
            Severity = Some severity
            Code = Some(code diag)
            CodeDescription = None
            Source = Some "Marksman"
            Message = msg
            RelatedInformation = None
            Tags = None
            Data = None
        }

    | NonBreakableWhitespace dup -> {
        Range = dup
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "Marksman"
        Message =
            "Non-breaking whitespace used instead of regular whitespace. This line won't be interpreted as a heading"
        RelatedInformation = None
        Tags = None
        Data = None
      }

type FolderDiag = Map<DocId, array<Lsp.Diagnostic>>

type WorkspaceDiag = Map<FolderId, FolderDiag>

module WorkspaceDiag =
    let private incomingReferenceDocuments folder docIds =
        docIds
        |> Seq.collect (fun docId ->
            let doc = Folder.findDocById docId folder

            Doc.syms doc
            |> Seq.choose Sym.asDef
            |> Seq.collect (fun definition ->
                Conn.Query.resolve (Scope.Doc docId, Sym.Def definition) (Folder.conn folder)))
        |> Seq.choose (function
            | Scope.Doc source, Sym.Ref _ -> Some source
            | _ -> None)
        |> Set.ofSeq

    /// Documents whose diagnostics may differ between two immutable workspace
    /// states. The comparison runs at publication time, so edits
    /// coalesced by the diagnostics agent need no separate change log.
    let affectedDocuments (before: Workspace) (after: Workspace) : Map<FolderId, Set<DocId>> =
        let previousFolders =
            Workspace.folders before
            |> Seq.map (fun folder -> Folder.id folder, folder)
            |> Map.ofSeq

        let currentFolders =
            Workspace.folders after
            |> Seq.map (fun folder -> Folder.id folder, folder)
            |> Map.ofSeq

        let folderIds =
            Set.union (Map.keys previousFolders |> Set.ofSeq) (Map.keys currentFolders |> Set.ofSeq)

        folderIds
        |> Seq.choose (fun folderId ->
            let previous = Map.tryFind folderId previousFolders
            let current = Map.tryFind folderId currentFolders

            let affected =
                match previous, current with
                | Some oldFolder, Some newFolder when obj.ReferenceEquals(oldFolder, newFolder) ->
                    Set.empty
                | Some oldFolder, Some newFolder when
                    Folder.config oldFolder <> Folder.config newFolder
                    || Folder.isSingleFile oldFolder <> Folder.isSingleFile newFolder
                    ->
                    Set.union
                        (Folder.docs oldFolder |> Seq.map Doc.id |> Set.ofSeq)
                        (Folder.docs newFolder |> Seq.map Doc.id |> Set.ofSeq)
                | Some oldFolder, Some newFolder ->
                    let docs = Folder.docsDifference oldFolder newFolder

                    let oldTargets = docs.changed + docs.removed
                    let newTargets = docs.changed + docs.added
                    let oldConn = Folder.conn oldFolder
                    let newConn = Folder.conn newFolder

                    let changedResolutions =
                        if obj.ReferenceEquals(oldConn, newConn) then
                            Set.empty
                        else
                            Conn.Query.documentsWithChangedReferenceResolutions oldConn newConn

                    docs.added
                    + docs.removed
                    + docs.changed
                    + docs.reopened
                    + changedResolutions
                    + incomingReferenceDocuments oldFolder oldTargets
                    + incomingReferenceDocuments newFolder newTargets
                | Some folder, None
                | None, Some folder -> Folder.docs folder |> Seq.map Doc.id |> Set.ofSeq
                | None, None -> Set.empty

            if Set.isEmpty affected then None else Some(folderId, affected))
        |> Map.ofSeq

    /// Calculate diagnostics for the current workspace. Without prior results,
    /// every document is recalculated; otherwise only affected documents are.
    let calculate
        (previous: option<Workspace * WorkspaceDiag>)
        (current: Workspace)
        : WorkspaceDiag * Map<FolderId, Set<DocId>> =
        let affected =
            match previous with
            | None ->
                Workspace.folders current
                |> Seq.map (fun folder ->
                    Folder.id folder, Folder.docs folder |> Seq.map Doc.id |> Set.ofSeq)
                |> Map.ofSeq
            | Some(previousWorkspace, _) -> affectedDocuments previousWorkspace current

        let previousDiagnostics =
            previous |> Option.map snd |> Option.defaultValue Map.empty

        let diagnostics =
            Workspace.folders current
            |> Seq.map (fun folder ->
                let folderId = Folder.id folder

                let previousFolder =
                    Map.tryFind folderId previousDiagnostics
                    |> Option.defaultValue Map.empty

                let affectedIds =
                    Map.tryFind folderId affected |> Option.defaultValue Set.empty

                let updated =
                    affectedIds
                    |> Set.fold
                        (fun entries docId ->
                            let docPath = UriWith.rootedRelToAbs docId.Raw

                            match Folder.tryFindDocByPath docPath.data folder with
                            | None -> Map.remove docId entries
                            | Some doc ->
                                let diags =
                                    checkDoc folder doc |> List.map diagToLsp |> Array.ofList

                                Map.add docId diags entries)
                        previousFolder

                folderId, updated)
            |> Map.ofSeq

        diagnostics, affected
