module Marksman.Toc

open Marksman.Misc
open Marksman.Index
open Marksman.Cst
open Marksman.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Logging

open type System.Environment

[<Literal>]
let StartMarker = """<!--toc:start-->"""

[<Literal>]
let EndMarker = """<!--toc:end-->"""

[<Literal>]
let EmptyLine = ""

type Title = string
type EntryLevel = int

type Entry = { level: EntryLevel; title: Title; link: Slug }

module Entry =
    let Mk (level: EntryLevel, title: Title) =
        { level = level; title = title; link = Slug.ofString title }

    let renderLink entry minLevel =
        let offset = String.replicate (entry.level - minLevel) "  "
        let slug = entry.link |> Slug.toString
        $"{offset}- [{entry.title}](#{slug})"

    let fromHeading (heading: Heading) : Entry =
        let slug = Heading.slug heading
        { level = heading.level; link = slug; title = heading.title.text }

type InsertionPoint =
    | After of Range
    | Replacing of Range
    | DocumentBeginning

type TableOfContents = { entries: array<Entry> }

module TableOfContents =

    let logger = LogProvider.getLoggerByName "TocAgent"

    let mk (index: Marksman.Index.Index) : TableOfContents option =
        let headings = index.headings |> Array.map (fun x -> x.data)

        if Array.isEmpty index.headings then
            None
        else
            Some { entries = Array.map Entry.fromHeading headings }

    let insertionPoint (doc: Workspace.Doc) : InsertionPoint =
        match (Array.toList doc.index.titles) with
        // if there's only a single title
        | [ singleTitle ] -> After singleTitle.range
        | _ ->
            match doc.index.yamlFrontMatter with
            | None -> DocumentBeginning
            | Some yml -> After yml.range


    let render (toc: TableOfContents) =
        let offset =
            if Array.isEmpty toc.entries then
                1
            else
                (Array.minBy (fun x -> x.level) toc.entries).level

        let tocLinks = Array.map (fun x -> Entry.renderLink x offset) toc.entries
        let startMarkerLines = [| StartMarker |]
        let endMarkerLines = [| EndMarker |]

        let lines = Array.concat [| startMarkerLines; tocLinks; endMarkerLines |]

        String.concat NewLine lines

    type State =
        | BeforeMarker
        | Collecting of Range
        | Collected of Range

    let detect (text: Text) : Range option =
        let lines = text.lineMap
        let maxIndex = lines.NumLines

        let rec go i st =
            if i.Equals(maxIndex) then
                st
            else
                let lineRange = text.LineContentRange(i)
                let lineContent = text.LineContent(i)
                let isStartMarker = lineContent.Trim().Equals(StartMarker)
                let isEndMarker = lineContent.Trim().Equals(EndMarker)
                let expandToThisLine (range: Range) = { range with End = lineRange.End }

                match st with
                // if we found the marker, start collecting text from here
                | BeforeMarker ->
                    if isStartMarker then
                        go (i + 1) (Collecting lineRange)
                    else
                        go (i + 1) BeforeMarker
                // if we are in collecting mode, just expand the selection
                | Collecting (range) ->
                    let toThisLine = expandToThisLine range

                    if isEndMarker then
                        Collected toThisLine
                    else
                        go (i + 1) (Collecting toThisLine)
                | _ -> st


        match (go 0 BeforeMarker) with
        // marker was never found
        | Collected range -> Some range
        | BeforeMarker -> None
        | other ->
            logger.warn (
                Log.setMessage $"TOC detection failed - end marker was not found"
                >> Log.addContext "finalState" other
            )

            None
