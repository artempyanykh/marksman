module Marksman.BibTeX

open System

type CitationKey = CitationKey of string

let private skipWhile predicate (content: string) start =
    let mutable offset = start

    while offset < content.Length && predicate content[offset] do
        offset <- offset + 1

    offset

// Only entry headers are needed for completion. Skip balanced entry bodies
// so apparent entries in titles, macros, and comments cannot become keys.
let parseKeys (content: string) : seq<CitationKey> =
    let isEntryType c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

    let isKey c =
        match c with
        | ','
        | '='
        | '{'
        | '}'
        | '('
        | ')'
        | '"'
        | '#'
        | '%' -> false
        | _ -> not (Char.IsWhiteSpace(c))

    let skipWhitespace = skipWhile Char.IsWhiteSpace content
    let isComma offset = offset < content.Length && content[offset] = ','

    seq {
        let mutable offset = 0

        while offset < content.Length do
            match content[offset] with
            | '%' ->
                let newline = content.IndexOf('\n', offset)
                offset <- if newline < 0 then content.Length else newline + 1
            | '@' ->
                let typeStart = offset + 1
                let typeEnd = skipWhile isEntryType content typeStart
                let typeLength = typeEnd - typeStart
                let opening = skipWhitespace typeEnd

                if
                    typeLength = 0
                    || opening = content.Length
                    || (content[opening] <> '{' && content[opening] <> '(')
                then
                    offset <- offset + 1
                else
                    offset <- opening + 1

                    let entryType = content.Substring(typeStart, typeLength)

                    let entryType = entryType.ToLowerInvariant()

                    let raw =
                        match entryType with
                        | "comment"
                        | "preamble" -> true
                        | _ -> false

                    if not raw && entryType <> "string" then
                        let start = skipWhitespace offset
                        let end_ = skipWhile isKey content start
                        let comma = skipWhitespace end_

                        if start < end_ && isComma comma then
                            let key = content.Substring(start, end_ - start)
                            yield CitationKey key

                    let closing = if content[opening] = '{' then '}' else ')'

                    let mutable braces = 0
                    let mutable quoted = false
                    let mutable finished = false

                    while offset < content.Length && not finished do
                        let atTopLevel = braces = 0 && not quoted

                        match content[offset] with
                        | '%' when atTopLevel && not raw ->
                            match content.IndexOf('\n', offset) with
                            | -1 -> offset <- content.Length
                            | newline -> offset <- newline
                        | '\\' -> offset <- offset + 1
                        | '{' -> braces <- braces + 1
                        | '}' when braces > 0 -> braces <- braces - 1
                        | '"' when braces = 0 && not raw -> quoted <- not quoted
                        | c when c = closing && atTopLevel -> finished <- true
                        | _ -> ()

                        offset <- offset + 1
            | _ -> offset <- offset + 1
    }
