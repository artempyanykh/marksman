module Marksman.Toc

open Marksman.Misc
open Marksman.Index
open Marksman.Cst
open Marksman.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

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
    let logger = LogProvider.getLoggerByName "TocAgent"

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
        + "\n"


    type State =
        | BeforeMarker
        | Collecting of Range * int


    let detect (text: Text) : Range option =
        let lines = text.lineMap
        let maxIndex = lines.NumLines

        let rec go i st =
            if i.Equals(maxIndex) then
                st
            else
                let lineRange = text.LineContentRange(i)
                let lineContent = text.LineContent(i)
                let lineIsEmpty = lineContent.Trim().Length.Equals(0)
                let lineIsMarker = lineContent.Equals(Marker)
                let expandToThisLine (range: Range) = { End = lineRange.End; Start = range.Start }

                logger.info (Log.setMessage $"State: {st}, line: {lineContent.Trim()}")

                match st with
                // if we found the marker, start collecting text from here
                | BeforeMarker when lineIsMarker -> go (i + 1) (Collecting(lineRange, 0))
                // if we are in collecting mode, just expand the selection
                | Collecting (range, numEmptyLines) ->
                    let toThisLine = expandToThisLine range

                    if lineIsEmpty then
                        if numEmptyLines.Equals(0) then
                            go (i + 1) (Collecting(toThisLine, 1))
                        else
                            Collecting(toThisLine, 2)
                    else
                        go (i + 1) (Collecting(expandToThisLine range, 0))
                | BeforeMarker -> go (i + 1) BeforeMarker

        match (go 0 BeforeMarker) with
        // marker was never found
        | Collecting (range, 2) -> Some range
        | other ->
            logger.error (Log.setMessage $"Got {other} instead")
            None
