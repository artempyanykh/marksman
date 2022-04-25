module Marksman.Parser

open System
open FSharp.Formatting.Markdown
open Ionide.LanguageServerProtocol.Types

open Text
open Misc

type NoteName = string

[<RequireQualifiedAccess>]
type XDest =
    | Note of NoteName
    | Heading of note: NoteName * heading: string

module XDest =
    let fmt =
        function
        | XDest.Note name -> $"[[{name}]]"
        | XDest.Heading (note, heading) -> $"[[{note}|{heading}]]"

type XRef =
    { text: string
      dest: XDest
      range: Range }

module XRef =
    let fmt x =
        let lines =
            [ $"X: {XDest.fmt x.dest}; {x.range}"
              $"  text: {x.text}" ]

        String.Join(Environment.NewLine, lines)

type Element =
    | H of Heading
    | X of XRef

and Heading =
    { level: int
      text: string
      range: Range
      scope: Range
      children: array<Element> }

let rec private fmtElement =
    function
    | H h -> fmtHeading h
    | X x -> XRef.fmt x

and private fmtHeading h =
    let l1 =
        $"H{h.level}: range={h.range.DebuggerDisplay}; scope={h.scope.DebuggerDisplay}"

    let l2 = $"  text=`{h.text}`"

    let rest =
        Array.map (indentFmt fmtElement) h.children

    String.Join(Environment.NewLine, Array.concat [ [| l1; l2 |]; rest ])

module Heading =
    let fmt = fmtHeading

module Element =
    let range =
        function
        | H h -> h.range
        | X x -> x.range

    let fmt = fmtElement

let mdRangeToRange (mdRange: MarkdownRange) : Range =
    // It seems that MarkdownRange has lines starting with 1
    assert (mdRange.StartLine > 0)

    let startPos =
        { Line = mdRange.StartLine - 1
          Character = mdRange.StartColumn }

    let endPos =
        { Line = mdRange.EndLine - 1
          Character = mdRange.EndColumn }

    { Start = startPos; End = endPos }

let rec scrapeSpan (text: Text) (span: MarkdownSpan) : seq<Element> =
    seq {
        match span with
        | DirectLink (spans, url, titleOpt, range) -> ()
        | IndirectLink (spans, original, key, range) -> ()
        | MarkdownPatterns.SpanLeaf _ -> ()
        | MarkdownPatterns.SpanNode (_, spans) ->
            for span in spans do
                yield! scrapeSpan text span
    }

let rec scrapeParagraph (text: Text) (par: MarkdownParagraph) : seq<Element> =
    seq {
        match par with
        | Heading (level, spans, mdRange) ->
            let mdRange =
                mdRange
                |> Option.orElseWith (fun _ -> failwith $"No range for {par}")
                |> Option.get

            let range = mdRangeToRange mdRange
            let headingText = text.Substring range

            let children =
                Seq.collect (scrapeSpan text) spans |> Array.ofSeq

            yield
                Element.H
                    { level = level
                      text = headingText
                      scope = range
                      range = range
                      children = children }
        | MarkdownPatterns.ParagraphLeaf _ -> ()
        | MarkdownPatterns.ParagraphSpans (_, spans) ->
            for span in spans do
                yield! scrapeSpan text span
        | MarkdownPatterns.ParagraphNested (_, parsList) ->
            for pars in parsList do
                for par in pars do
                    yield! scrapeParagraph text par
    }

let rec reconstructHierarchy (text: Text) (flat: seq<Element>) : seq<Element> =
    seq {
        let mutable headStack: list<Heading> = []
        let mutable accChildren: list<Element> = []

        let mutable accStandalone: list<Element> =
            []

        let rec unwindHeadStack newHead : unit =
            match headStack with
            | [] ->
                accStandalone <-
                    List.concat [ accChildren
                                  accStandalone ]

                accChildren <- []
                headStack <- [ newHead ]
            | curHead :: rest ->
                // #
                // ##
                //  e1, e2
                // ###       |
                //  e3, e4   |
                // ##        |
                //  e5
                let curHeadChildren =
                    Array.concat [ curHead.children
                                   Array.ofList accChildren ]

                let curHead =
                    { curHead with children = curHeadChildren }

                accChildren <- []

                // Unwind further until we find a parent heading or none at all
                if curHead.level >= newHead.level then
                    let newScope =
                        { Start = curHead.scope.Start
                          End = newHead.scope.Start }

                    let curHead =
                        { curHead with scope = newScope }

                    accChildren <- [ H curHead ]
                    headStack <- rest
                    unwindHeadStack newHead
                else // cur.level < new.level; should stack the child
                    headStack <- newHead :: curHead :: rest

        for el in flat do
            match el with
            | X _ ->
                match headStack with
                | _ :: _ -> accChildren <- el :: accChildren
                | [] -> yield el
            | H newHead -> unwindHeadStack newHead

        let guardHead =
            { level = -1
              text = ""
              scope = text.EndRange()
              range = text.EndRange()
              children = [||] }

        unwindHeadStack guardHead

        for child in accChildren do
            yield child

        for child in accStandalone do
            yield child
    }

let rec sortElements (elements: array<Element>) : unit =
    for el in elements do
        match el with
        | H h -> sortElements h.children
        | _ -> ()

    let elementStart el =
        let start = (Element.range el).Start
        start.Line + start.Character

    Array.sortInPlaceBy elementStart elements

let rec scrapeText (text: Text) : array<Element> =
    let parsed = Markdown.Parse(text.content)

    let flatElements =
        Seq.collect (scrapeParagraph text) parsed.Paragraphs

    let hierarchicalElements =
        reconstructHierarchy text flatElements

    let elements =
        Array.ofSeq hierarchicalElements

    sortElements elements
    elements
