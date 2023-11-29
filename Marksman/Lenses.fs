module Marksman.Lenses

open Ionide.LanguageServerProtocol.Types

open Marksman.Doc
open Marksman.Folder
open Marksman.Refs

let findReferencesLens = "marksman.findReferences"

let private humanRefCount cnt = if cnt = 1 then "1 reference" else $"{cnt} references"

let forDoc (folder: Folder) (doc: Doc) =
    seq {
        for h in doc.Index.headings do
            let refs = Dest.findElementRefs false folder doc (Cst.H h)
            let refCount = Seq.length refs

            if refCount > 0 then
                let command =
                    { Title = humanRefCount refCount
                      Command = findReferencesLens
                      Arguments = None }

                yield { Range = h.range; Command = Some command; Data = None }
    }
    |> Array.ofSeq
