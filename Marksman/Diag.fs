module Marksman.Diag

open Ionide.LanguageServerProtocol.Types

open Marksman.Names
open Marksman.Doc
open Marksman.Folder
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
            let whitespaceRange: Lsp.Range =
                { Start = { Line = x; Character = heading.Length }
                  End = { Line = x; Character = heading.Length + 1 } }

            [ NonBreakableWhitespace(whitespaceRange) ])

let isCrossFileLink sym =
    match sym with
    | Syms.SectionRef (Some _, _) -> true
    | Syms.SectionRef _
    | Syms.LinkDefRef _ -> false


let checkLink (folder: Folder) (doc: Doc) (link: Element) : seq<Entry> =
    let exts = Folder.configuredMarkdownExts folder

    let sym =
        doc.Structure
        |> Structure.Structure.tryFindSymbolForConcrete link
        |> Option.bind Syms.Sym.asRef

    match sym with
    | None -> []
    | Some sym ->
        let refs = Dest.tryResolveElement folder doc link |> Array.ofSeq

        if Folder.isSingleFile folder && isCrossFileLink sym then
            []
        else if refs.Length = 1 then
            []
        else if refs.Length = 0 then
            match link with
            // Inline shortcut links often are a part of regular text.
            // Raising diagnostics on them would be noisy.
            | ML { data = MdLink.RS _ } -> []
            | ML { data = MdLink.IL (_, url, _) } ->
                match url with
                | Some { data = url } ->
                    // Inline links to docs that don't look like a markdown file should not
                    // produce diagnostics
                    if Misc.isMarkdownFile exts (UrlEncoded.decode url) then
                        [ BrokenLink(link, sym) ]
                    else
                        []
                | _ -> [ BrokenLink(link, sym) ]
            | _ -> [ BrokenLink(link, sym) ]
        else
            [ AmbiguousLink(link, sym, refs) ]

let checkLinks (folder: Folder) (doc: Doc) : seq<Entry> =
    let links = Doc.index >> Index.links <| doc
    links |> Seq.collect (checkLink folder doc)

let checkFolder (folder: Folder) : seq<DocId * list<Entry>> =
    seq {
        for doc in Folder.docs folder do
            let docDiag =
                seq {
                    yield! checkLinks folder doc
                    yield! checkNonBreakingWhitespace doc
                }
                |> List.ofSeq

            Doc.id doc, docDiag
    }

let destToHuman (ref: Dest) : string =
    match ref with
    | Dest.Doc { doc = doc } -> $"document {Doc.name doc}"
    | Dest.Heading (docLink, { data = heading }) ->
        $"heading {Heading.name heading} in the document {Doc.name (DocLink.doc docLink)}"
    | Dest.LinkDef (_, { data = ld }) -> $"link definition {MdLinkDef.name ld}"
    | Dest.Tag (doc, { data = tag }) -> $"tag {tag.name} in the document {Doc.name doc}"

let docToHuman (name: string) : string = $"document '{name}'"

let refToHuman (ref: Syms.Ref) : string =
    match ref with
    | Syms.SectionRef (None, None) -> failwith $"Internal error: malformed SectionRef: {ref}"
    | Syms.SectionRef (Some docName, None) -> docToHuman docName
    | Syms.SectionRef (docName, Some heading) ->
        match docName with
        | None -> $"heading '{heading}'"
        | Some docName -> $"heading '{heading}' in {docToHuman docName}"
    | Syms.LinkDefRef ld -> $"link definition with the label '{ld}'"

let diagToLsp (diag: Entry) : Lsp.Diagnostic =
    match diag with
    | AmbiguousLink (el, ref, dests) ->
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

        { Range = Element.range el
          Severity = Some severity
          Code = Some(code diag)
          CodeDescription = None
          Source = Some "Marksman"
          Message = $"Ambiguous link to {refToHuman ref}"
          RelatedInformation = Some related
          Tags = None
          Data = None }
    | BrokenLink (el, ref) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | T _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let msg = $"Link to non-existent {refToHuman ref}"

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

type FolderDiag = array<DocId * array<Lsp.Diagnostic>>

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
