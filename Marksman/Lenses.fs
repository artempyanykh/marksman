module Marksman.Lenses

open Ionide.LanguageServerProtocol.Types
module LspServer = Ionide.LanguageServerProtocol.Server

open Marksman.Doc
open Marksman.Folder
open Marksman.State
open Marksman.Refs

let findReferencesLens = "marksman.findReferences"

// fsharplint:disable-next-line
type FindReferencesData = { Uri: DocumentUri; Position: Position; Locations: Location[] }

let private humanRefCount cnt = if cnt = 1 then "1 reference" else $"{cnt} references"

let buildReferenceLens (client: ClientDescription) (folder: Folder) (doc: Doc) (el: Cst.Element) =
    let refs = Dest.findElementRefs false folder doc el |> Array.ofSeq
    let refCount = Array.length refs

    if refCount > 0 then
        let data =
            if client.SupportsLensFindReferences then
                let locations = [| for doc, el in refs -> { Uri = Doc.uri doc; Range = el.Range } |]

                let data = {
                    Uri = Doc.uri doc
                    Position = el.Range.Start
                    Locations = locations
                }

                Some [| LspServer.serialize data |]
            else
                None

        let command = {
            Title = humanRefCount refCount
            Command = findReferencesLens
            Arguments = data
        }


        Some { Range = el.Range; Command = Some command; Data = None }
    else
        None

let forDoc (client: ClientDescription) (folder: Folder) (doc: Doc) =
    let headingLenses =
        doc.Index.headings
        |> Seq.map Cst.H
        |> Seq.choose (buildReferenceLens client folder doc)

    let linkDefLenses =
        doc.Index.linkDefs
        |> Seq.map Cst.MLD
        |> Seq.choose (buildReferenceLens client folder doc)

    Seq.append headingLenses linkDefLenses |> Array.ofSeq
