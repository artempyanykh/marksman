module Marksman.Comp

open Ionide.LanguageServerProtocol.Types
open Parser
open Marksman.Domain
open Marksman.Misc

type Completable =
    | WikiLink
    | MarkdownLink
    | PrefixWiki
    | PrefixMarkdown
    | EmptySquareBrackets
    | EmptyRoundBrackets

let findCandidates (pos: Position) (docUri: PathUri) (folder: Folder) : array<CompletionItem> = [||]
//    let doc = Folder.tryFindDocument docUri folder
//
//    match doc with
//    | None -> [||]
//    | Some doc ->
//        let curDocName = doc.name
//
//        let isAtPoint =
//            function
//            | CP cp -> cp.range.End = pos
//            | WL x -> x.range.Start <= pos && pos < x.range.End
//            | _ -> false
//
//        let atPoint =
//            Document.elementsAll doc |> Seq.tryFind isAtPoint
//
//        match atPoint with
//        | None -> [||]
//        | Some atPoint ->
//            let wantedDoc, wantedHeading =
//                match atPoint with
//                | WL ref ->
//                    let destDoc =
//                        WikiLink.destDoc ref.dest
//                        // Absence of explicit doc means completion inside the current doc
//                        |> Option.defaultValue curDocName
//                        |> Some
//
//                    destDoc, WikiLink.destHeading ref.dest
//                | CP cp -> CompletionPoint.destNote cp, None
//                | _ -> None, None
//
//            // Now we have 2 modes of completion to tackle
//            match wantedDoc, wantedHeading with
//            // Plain doc name completion
//            | Some wantedDoc, None ->
//                let docs =
//                    Map.values folder.documents
//                    |> Seq.map (fun doc -> doc, Document.title doc, doc.name)
//
//                let isMatchingDoc (_, title: option<Heading>, name) =
//                    let titleMatch =
//                        title
//                        |> Option.map (fun t -> wantedDoc.IsSubSequenceOf(t.text))
//                        |> Option.defaultValue false
//
//                    let nameMatch =
//                        wantedDoc.IsSubSequenceOf(name)
//
//                    let notCurDoc = name <> curDocName
//
//                    (titleMatch || nameMatch) && notCurDoc
//
//                let matchingDocs =
//                    docs |> Seq.filter isMatchingDoc
//
//                let toCompletionItem (_doc, title, name) =
//                    let label =
//                        Option.map Heading.text title
//                        |> Option.defaultValue name
//
//                    let detail =
//                        if Option.isSome title then
//                            Some name
//                        else
//                            None
//
//                    let dest = WikiLink.Doc name
//
//                    let newText = WikiLink.fmt dest
//
//                    let textEdit =
//                        { Range = Element.range atPoint
//                          NewText = newText }
//
//                    { CompletionItem.Create(label) with
//                        Detail = detail
//                        TextEdit = Some textEdit
//                        FilterText = Some newText }
//
//                matchingDocs
//                |> Seq.map toCompletionItem
//                |> Array.ofSeq
//            // Heading completion inside an already specified doc
//            | Some wantedDoc, Some wantedHeading ->
//                let targetDoc =
//                    Map.values folder.documents
//                    |> Seq.tryFind (fun doc -> doc.name = wantedDoc)
//
//                match targetDoc with
//                | None -> [||]
//                | Some targetDoc ->
//                    let matchingHeadings =
//                        seq {
//                            for el in Document.elementsAll targetDoc do
//                                match Element.asHeading el with
//                                | Some h when
//                                    h.level <> 1
//                                    && wantedHeading.IsSubSequenceOf(Heading.title h)
//                                    ->
//                                    yield h
//                                | _ -> ()
//                        }
//
//                    let toCompletionItem heading =
//                        // TODO: consider using slug for heading
//                        let label = Heading.title heading
//
//                        let destDoc =
//                            if wantedDoc = curDocName then
//                                None
//                            else
//                                Some wantedDoc
//
//                        let dest = WikiLink.Heading(destDoc, label)
//                        let newText = WikiLink.fmt dest
//
//                        let textEdit =
//                            { Range = Element.range atPoint
//                              NewText = newText }
//
//                        { CompletionItem.Create(label) with
//                            TextEdit = Some textEdit
//                            FilterText = Some newText }
//
//                    matchingHeadings
//                    |> Seq.map toCompletionItem
//                    |> Array.ofSeq
//            | _ -> [||]
