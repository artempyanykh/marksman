module Marksman.Diag

open Ionide.LanguageServerProtocol.Types
open Marksman.Workspace

module Lsp = Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Cst
open Marksman.Index
open Marksman.Refs

type Entry =
    | AmbiguousLink of Element * Uref * array<Dest>
    | BrokenLink of Element * Uref
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
            let whitespaceRange: Lsp.Range =
                { Start = { Line = x; Character = heading.Length }
                  End = { Line = x; Character = heading.Length + 1 } }

            [ NonBreakableWhitespace(whitespaceRange) ])

let isCrossFileLink uref =
    match uref with
    | Uref.Doc _ -> true
    | Uref.Heading(doc = Some _) -> true
    | Uref.Heading(doc = None) -> false
    | Uref.LinkDef _ -> false

let checkLink (folder: Folder) (doc: Doc) (link: Element) : seq<Entry> =
    let uref = Uref.ofElement link

    match uref with
    | None -> []
    | Some uref ->
        let refs = Dest.tryResolveUref uref doc folder |> Array.ofSeq

        if Folder.isSingleFile folder && isCrossFileLink uref then
            []
        else if refs.Length = 1 then
            []
        else if refs.Length = 0 then
            match link with
            // Inline shortcut links often are a part of regular text.
            // Raising diagnostics on them would be noisy.
            | ML { data = MdLink.RS _ } -> []
            | _ -> [ BrokenLink(link, uref) ]
        else
            [ AmbiguousLink(link, uref, refs) ]

let checkLinks (folder: Folder) (doc: Doc) : seq<Entry> =
    let links = Doc.index >> Index.links <| doc
    links |> Seq.collect (checkLink folder doc)

let checkFolder (folder: Folder) : seq<PathUri * list<Entry>> =
    seq {
        for doc in Folder.docs folder do
            let docDiag =
                seq {
                    yield! checkLinks folder doc
                    yield! checkNonBreakingWhitespace doc
                }
                |> List.ofSeq

            Doc.path doc, docDiag
    }

let refToHuman (ref: Dest) : string =
    match ref with
    | Dest.Doc doc -> $"document {Doc.name doc}"
    | Dest.Heading (doc, { data = heading }) ->
        $"heading {Heading.name heading} in the document {Doc.name doc}"
    | Dest.LinkDef (_, { data = ld }) -> $"link definition {MdLinkDef.name ld}"

let docRefToHuman: DocRef -> string =
    function
    | DocRef.Title title -> $"document with the title '{title}'"
    | DocRef.Url url -> $"document at '{url}'"

let urefToHuman (uref: Uref) : string =
    match uref with
    | Uref.Doc docRef -> docRefToHuman docRef
    | Uref.Heading (docRef, heading) ->
        match docRef with
        | None -> $"heading '{Node.text heading}'"
        | Some docRef -> $"heading '{Node.text heading}' in {docRefToHuman docRef}"
    | Uref.LinkDef ld -> $"link definition with the label '{Node.text ld}'"

let diagToLsp (diag: Entry) : Lsp.Diagnostic =
    match diag with
    | AmbiguousLink (el, uref, refs) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let mkRelated ref : DiagnosticRelatedInformation =
            let loc = Dest.location ref
            let msg = $"Duplicate definition of {refToHuman ref}"
            { Location = loc; Message = msg }

        let related = refs |> Array.map mkRelated

        { Range = Element.range el
          Severity = Some severity
          Code = Some(code diag)
          CodeDescription = None
          Source = Some "Marksman"
          Message = $"Ambiguous link to {urefToHuman uref}"
          RelatedInformation = Some related
          Tags = None
          Data = None }
    | BrokenLink (el, uref) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let msg = $"Link to non-existent {urefToHuman uref}"

        { Range = Element.range el
          Severity = Some severity
          Code = Some(code diag)
          CodeDescription = None
          Source = Some "Marksman"
          Message = msg
          RelatedInformation = None
          Tags = None
          Data = None }

    | NonBreakableWhitespace dup ->
        { Range = dup
          Severity = Some Lsp.DiagnosticSeverity.Warning
          Code = Some(code diag)
          CodeDescription = None
          Source = Some "Marksman"
          Message =
            "Non-breaking whitespace used instead of regular whitespace. This line won't be interpreted as a heading"
          RelatedInformation = None
          Tags = None
          Data = None }

type FolderDiag = array<PathUri * array<Lsp.Diagnostic>>

module FolderDiag =
    let mk (folder: Folder) : FolderDiag =
        checkFolder folder
        |> Seq.map (fun (uri, diags) ->
            let lspDiags = List.map diagToLsp diags |> Array.ofList

            uri, lspDiags)
        |> Array.ofSeq

type WorkspaceDiag = Map<FolderId, FolderDiag>

module WorkspaceDiag =
    let mk (ws: Workspace) : WorkspaceDiag =
        Workspace.folders ws
        |> Seq.map (fun folder -> (Folder.id folder), FolderDiag.mk folder)
        |> Map.ofSeq

    let empty = Map.empty
