module Marksman.Toc

open Marksman.Misc
open Marksman.Index
open Marksman.Cst
open Marksman.Text
open Ionide.LanguageServerProtocol.Types

[<Literal>]
let Marker = """<!--toc-->"""

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
        let headings = index.headings |> Array.map (fun x -> x.data)

        if index.headings.Length.Equals 0 then
            None
        else
            Some { Entries = Array.map Entry.fromHeading headings }

    let render (toc: TableOfContents) =
        Marker
        + "\n"
        + (Array.map Entry.renderLink toc.Entries |> String.concat "\n")


    type State =
        | BeforeMarker
        | Collecting of Range


    let detect (text: Text) : Range option =
        let lines = text.lineMap
        let maxIndex = lines.NumLines

        let rec go i st =
            if i.Equals(maxIndex) then
                st
            else
                let lineRange = text.LineContentRange(i)
                let lineContent = text.LineContent(i)

                match st with
                // if we found the marker, start collecting text from here
                | BeforeMarker when lineContent.Equals(Marker) -> go (i + 1) (Collecting lineRange)
                // if we are in collecting mode, just expand the selection
                | Collecting (range) ->
                    let expandToThisLine =
                        Collecting { End = lineRange.End; Start = range.Start }

                    if lineContent.Trim().Length.Equals(0) then
                        expandToThisLine
                    else
                        go (i + 1) expandToThisLine // in any other case, just move to next line
                | BeforeMarker -> go (i + 1) BeforeMarker

        match (go 0 BeforeMarker) with
        // marker was never found
        | BeforeMarker -> None
        | Collecting (range) -> Some range
