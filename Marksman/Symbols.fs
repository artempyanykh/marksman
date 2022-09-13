module Marksman.Symbols

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Cst
open Marksman.Workspace
open Marksman.Index

let headingToSymbolInfo (docUri: PathUri) (h: Node<Heading>) : SymbolInformation =
    let name = Heading.name h.data
    let name = $"H{h.data.level}: {name}"
    let kind = SymbolKind.String

    let location = { Uri = docUri.DocumentUri; Range = h.range }

    let sym =
        { Name = name
          Kind = kind
          Location = location
          ContainerName = None }

    sym

let rec headingToDocumentSymbol (isEmacs: bool) (h: Node<Heading>) : DocumentSymbol =
    let name = Heading.name h.data
    let kind = SymbolKind.String
    let range = h.data.scope
    let selectionRange = h.range

    let children =
        h.data.children
        |> Element.pickHeadings
        |> Array.map (headingToDocumentSymbol isEmacs)

    let children =
        if Array.isEmpty children then
            None
        else if isEmacs then
            // Emacs' imenu with consult/counsel/etc. doesn't allow selecting intermediate
            // nodes that have children. As a workaround we add a '.' this node.
            let thisHeading =
                { Name = "."
                  Detail = None
                  Kind = kind
                  Range = selectionRange
                  SelectionRange = selectionRange
                  Children = None }

            Some(Array.append [| thisHeading |] children)
        else
            Some children

    { Name = name
      Detail = None
      Kind = kind
      Range = range
      SelectionRange = selectionRange
      Children = children }

let docSymbols
    (hierarchy: bool)
    (isEmacs: bool)
    (doc: Doc)
    : U2<array<SymbolInformation>, array<DocumentSymbol>> =
    if hierarchy then
        let topLevelHeadings =
            Doc.cst doc |> Seq.collect (Element.asHeading >> Option.toList)

        topLevelHeadings
        |> Seq.map (headingToDocumentSymbol isEmacs)
        |> Array.ofSeq
        |> Second
    else
        let allHeadings = Doc.index >> Index.headings <| doc

        allHeadings
        |> Seq.map (headingToSymbolInfo (Doc.path doc))
        |> Array.ofSeq
        |> First

let workspaceSymbols (query: string) (ws: Workspace) : array<SymbolInformation> =
    seq {
        for folder in Workspace.folders ws do
            for doc in Folder.docs folder do
                let headings = Doc.index doc |> Index.headings

                let matchingHeadings =
                    headings
                    |> Seq.filter (fun { data = h } -> query.IsSubSequenceOf(Heading.name h))

                let matchingSymbols =
                    matchingHeadings |> Seq.map (headingToSymbolInfo (Doc.path doc))

                yield! matchingSymbols
    }
    |> Array.ofSeq
