module Marksman.Index

open Ionide.LanguageServerProtocol.Types
open Marksman.Misc
open Marksman.Cst

type Dictionary<'K, 'V> = System.Collections.Generic.Dictionary<'K, 'V>

type Index =
    { titles: array<Node<Heading>>
      headingsBySlug: Map<Slug, list<Node<Heading>>>
      wikiLinks: array<Node<WikiLink>>
      mdLinks: array<Node<MdLink>>
      linkDefs: array<Node<MdLinkDef>> }

module Index =
    let ofCst (cst: Cst) : Index =
        let titles = ResizeArray()
        let headings = Dictionary<Slug, ResizeArray<Node<Heading>>>()
        let wikiLinks = ResizeArray()
        let mdLinks = ResizeArray()
        let linkDefs = ResizeArray()

        for el in Cst.elementsAll cst do
            match el with
            | H hn ->
                let slug = Heading.slug hn.data

                if not (headings.ContainsKey(slug)) then
                    headings[slug] <- ResizeArray()

                headings.[slug].Add(hn)

                if Heading.isTitle hn.data then titles.Add(hn)
            | WL wl -> wikiLinks.Add(wl)
            | ML ml -> mdLinks.Add(ml)
            | MLD linkDef -> linkDefs.Add(linkDef)

        let headingsBySlug =
            seq {
                for KeyValue (slug, headings) in headings do
                    yield slug, headings |> List.ofSeq
            }
            |> Map.ofSeq

        let titles = titles.ToArray()
        let wikiLinks = wikiLinks.ToArray()
        let mdLinks = mdLinks.ToArray()
        let linkDefs = linkDefs.ToArray()

        { titles = titles
          headingsBySlug = headingsBySlug
          wikiLinks = wikiLinks
          mdLinks = mdLinks
          linkDefs = linkDefs }

    let titles index = index.titles

    let title = titles >> Array.tryHead

    let wikiLinks index = index.wikiLinks

    let mdLinks index = index.mdLinks

    let links index =
        Seq.append (wikiLinks index |> Seq.map WL) (mdLinks index |> Seq.map ML)

    let linkDefs index = index.linkDefs

    let tryFindLinkDef label index =
        index.linkDefs
        |> Array.tryFind (fun { data = ld } -> ld.label.text = label)

    let headingsBySlug index = index.headingsBySlug

    let headings index = index.headingsBySlug |> Map.values |> Seq.concat

    let tryFindHeadingBySlug slug index =
        index.headingsBySlug |> Map.tryFind slug |> Option.bind List.tryHead

    let linkAtPos (pos: Position) index =
        let matching el =
            let range = Node.range el
            range.Start <= pos && pos < range.End

        let fromWiki () = Array.tryFind matching index.wikiLinks |> Option.map WL
        let fromMd () = Array.tryFind matching index.mdLinks |> Option.map ML

        fromWiki () |> Option.orElseWith fromMd
