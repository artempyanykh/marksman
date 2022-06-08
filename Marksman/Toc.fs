module Marksman.Toc

open Marksman.Misc
open Marksman.Index
open Marksman.Cst

type Title = string
type EntryLevel = int

type Entry = { Level: EntryLevel; Title: Title; Link: Slug }

module Entry =
    let renderLink entry =
        let offset = String.replicate (entry.Level - 1) " "
        let slug = entry.Link |> Slug.toString
        $"{offset}- [{entry.Title}](#{slug})"

    let fromHeading (heading: Heading) : Entry =
        let slug = Heading.slug heading
        { Level = heading.level; Link = slug; Title = heading.title.text }

type TableOfContents = { Entries: Entry[] }

module TableOfContents =
    let mk (index: Index) : TableOfContents option =
        let headings = index.titles |> Array.map (fun x -> x.data)

        if index.titles.Length.Equals 0 then
            None
        else
            Some { Entries = Array.map Entry.fromHeading headings }

    let render (toc: TableOfContents) = Array.map Entry.renderLink toc.Entries |> String.concat "\n"
