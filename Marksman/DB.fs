module Marksman.DB

open Misc
open Parser
open Workspace

type Dictionary<'K, 'V> = System.Collections.Generic.Dictionary<'K, 'V>

type DocDB =
    { path: PathUri
      titles: list<Node<Heading>>
      headings: Map<Slug, list<Node<Heading>>>
      wikiLinks: array<Node<WikiLink>> }

module DocDB =
    let ofDoc (doc: Doc) : DocDB =
        let titles = ResizeArray()

        let headings = Dictionary<Slug, ResizeArray<Node<Heading>>>()

        let wikiLinks = ResizeArray()

        for el in Doc.elementsAll doc do
            match el with
            | H hn ->
                let slug = Heading.slug hn.data

                if not (headings.ContainsKey(slug)) then
                    headings[slug] <- ResizeArray()

                headings.[slug].Add(hn)

                if Heading.isTitle hn.data then titles.Add(hn)
            | WL ref -> wikiLinks.Add(ref)
            | _ -> ()

        let titles = titles |> List.ofSeq

        let headings =
            seq {
                for KeyValue (slug, headings) in headings do
                    yield slug, headings |> List.ofSeq
            }
            |> Map.ofSeq

        let wikiLinks = wikiLinks.ToArray()

        { path = doc.path
          titles = titles
          headings = headings
          wikiLinks = wikiLinks }

    let path db = db.path

    let titles db = db.titles

    let wikiLinks db = db.wikiLinks |> List.ofArray

    let headingsBySlug db = db.headings
    let tryFindHeadingBySlug slug db = db.headings |> Map.tryFind slug |> Option.bind List.tryHead

type FolderDB = F of Map<Doc, DocDB>

module FolderDB =
    let ofFolder (folder: Folder) : FolderDB =
        seq {
            for KeyValue (_, doc) in folder.docs do
                let docDB = DocDB.ofDoc doc
                yield doc, docDB
        }
        |> Map.ofSeq
        |> F

    let data (F data) = data

    let tryFindDocBySlug (slug: Slug) (fdb: FolderDB) : option<DocDB> =
        data fdb
        |> Map.tryPick (fun doc db -> if Doc.slug doc = slug then Some db else None)

    let findDocBySlug slug fdb =
        tryFindDocBySlug slug fdb
        |> Option.defaultWith (fun () -> failwith $"No DocDB with slug: {slug}")

    let tryFindDocByRelPath (relPath: string) (fdb: FolderDB) : option<DocDB> =
        data fdb
        |> Map.tryPick (fun doc db ->
            if doc.relPath.AbsPathUrlEncode() = relPath.AbsPathUrlEncode() then
                Some db
            else
                None)

    let docsBySlug fdb = data fdb |> Map.toSeq |> Seq.map (fun (doc, db) -> Doc.slug doc, db)

    let withDoc doc fdb =
        // TODO: assert that doc is inside a folder
        let docDB = DocDB.ofDoc doc
        let newData = Map.add doc docDB (data fdb)
        F newData

    let withoutDoc path fdb =
        let newData = Map.remove path (data fdb)
        F newData

type WorkspaceDB = WS of Map<PathUri, FolderDB>

module WorkspaceDB =
    let ofWorkspace (ws: Workspace) : WorkspaceDB =
        seq { for KeyValue (path, folder) in ws.folders -> path, FolderDB.ofFolder folder }
        |> Map.ofSeq
        |> WS

    let data (WS data) = data

    let tryFindByPath (path: PathUri) (ws: WorkspaceDB) : option<FolderDB> =
        data ws |> Map.tryFind path

    let findByPath (path: PathUri) (ws: WorkspaceDB) : FolderDB =
        tryFindByPath path ws
        |> Option.defaultWith (fun () -> failwith $"No DB for ${path.LocalPath}")

    let folders wdb = data wdb |> Map.toSeq
    
    let withDoc (doc: Doc) wdb =
        folders wdb |> Seq.tryFind (fun (path, fdb) -> )
            
