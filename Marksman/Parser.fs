module Marksman.Parser

open FSharp.Formatting.Markdown
open Ionide.LanguageServerProtocol.Types

open Misc

type NoteName = string

type Heading =
    { level: int
      text: string
      scope: Range
      range: Range }

[<RequireQualifiedAccess>]
type XDest =
    | Note of NoteName
    | Heading of note: NoteName * heading: string

type XRef =
    { text: string
      note: NoteName
      dest: XDest
      range: Range }

type Element =
    | H of Heading
    | X of XRef

type Elements = List<Element>

let rec scrapeParagraphs (pars: MarkdownParagraphs) : Elements = []

let rec scrapeDocument (text: string) : Elements =
    let parsed = Markdown.Parse(text)
    scrapeParagraphs parsed.Paragraphs
