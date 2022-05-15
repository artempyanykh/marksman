module Marksman.Text

open System
open System.IO
open System.Text
open Ionide.LanguageServerProtocol.Types

open Misc

type LineRange = int * int

type LineMap =
    | LineMap of array<LineRange>
    member this.Map =
        let (LineMap arr) = this
        arr

    member this.NumLines = this.Map.Length - 1

    member this.TryFindPosition(offset: int) : Option<Position> =
        let rec go l h =
            if l > h then
                None
            else
                let m = l + (h - l) / 2
                let start, end_ = this.Map[m]

                if start > offset then
                    go l (m - 1)
                else if start <= offset && offset < end_ then
                    Some m
                else if start = end_ && start = offset then
                    Some m
                else if offset >= end_ then
                    go (m + 1) h
                else
                    None

        match go 0 (this.Map.Length - 1) with
        | Some lineIdx ->
            let start, _ = this.Map[lineIdx]

            Some
                { Line = lineIdx
                  Character = offset - start }
        | _ -> None

    member this.FindPosition(offset: int) : Position =
        this.TryFindPosition(offset)
        |> Option.defaultWith (fun _ -> failwith $"Couldn't find offset's position: {offset}")

    member this.TryFindOffset(pos: Position) : option<int> =
        if pos.Line >= this.Map.Length then
            None
        else
            let start, end_ = this.Map[pos.Line]
            let offset = start + pos.Character

            if offset > end_ then
                None
            else
                Some(offset)

    member this.FindOffset(pos: Position) : int =
        this.TryFindOffset(pos)
        |> Option.defaultWith (fun _ -> failwith $"Position outside of line map: {pos}")

    member this.TryFindRange(range: Range) : option<int> * option<int> =
        let start = this.TryFindOffset range.Start
        let end_ = this.TryFindOffset range.End
        start, end_

    member this.FindRange(range: Range) : LineRange =
        match this.TryFindRange range with
        | Some s, Some e -> s, e
        | None, _ -> failwith $"Range start outside of line map: {range}"
        | _, None -> failwith $"Range end outside of line map: {range}"

type Text =
    { content: string
      lineMap: LineMap }
    member this.Substring(range: Range) : string =
        let s, e = this.lineMap.FindRange range
        this.content.Substring(s, e - s)

    member this.CharAt(p: Position) : char =
        let off = this.lineMap.FindOffset(p)
        this.content[off]

    member this.CharAt(off: int) : char = this.content[off]

    member this.LineContentOffsets(line: int) : LineRange =
        let start, end_ = this.lineMap.Map[line]

        if start = end_ then
            start, end_
        else if start = end_ - 1 then
            if this.content[start] = '\n' then
                start, start
            else
                start, end_
        else if this.content[end_ - 2] = '\r'
                && this.content[end_ - 1] = '\n' then
            start, end_ - 2
        else if this.content[end_ - 1] = '\n' then
            start, end_ - 1
        else
            start, end_

    member this.LineContentRange(line: int) : Range =
        let start, end_ =
            this.LineContentOffsets(line)

        Range.Mk(line, 0, line, end_ - start)

    member this.FullRange() : Range =
        let lineNum = this.lineMap.Map.Length - 1
        let start = { Line = 0; Character = 0 }
        let end_ = { Line = lineNum; Character = 0 }
        { Start = start; End = end_ }

    member this.EndRange() : Range =
        let lineNum = this.lineMap.Map.Length - 1
        let end_ = { Line = lineNum; Character = 0 }
        { Start = end_; End = end_ }

type internal TrackingTextReader(baseReader: TextReader) =
    inherit TextReader()

    interface IDisposable with
        member this.Dispose() =
            baseReader.Dispose()
            base.Dispose()

    member val Position: int = 0 with get, set

    override this.Read() : int =
        let char = baseReader.Read()

        if char <> -1 then
            this.Position <- this.Position + 1

        char


    override this.Peek() : int = baseReader.Peek()

let mkLineMap (str: string) : LineMap =
    use reader =
        new TrackingTextReader(new StringReader(str))

    let lineMap = ResizeArray<int * int>()
    let mutable start = 0

    let processLine line =
        match line with
        | null -> false
        | _ ->
            let finish = reader.Position
            let range = start, finish
            lineMap.Add(range)
            start <- finish
            true

    while processLine (reader.ReadLine()) do
        ()

    // Add an empty new line for text insertion at the end
    lineMap.Add(start, start)

    LineMap(lineMap.ToArray())

let mkText (content: string) : Text =
    let lineMap = mkLineMap content
    { content = content; lineMap = lineMap }

let mkPosition (line, char) = { Line = line; Character = char }

let mkRange (start, end_) =
    { Start = mkPosition start
      End = mkPosition end_ }

let private applyChangeRaw (lineMap: LineMap) (content: string) (change: TextDocumentContentChangeEvent) : string =
    match change.Range, change.RangeLength with
    | Some range, Some length ->
        let start =
            range.Start |> lineMap.FindOffset

        StringBuilder(content)
            .Remove(start, length)
            .Insert(start, change.Text)
            .ToString()
    | None, None -> change.Text
    | _, _ -> failwith $"Unexpected change event structure: {change}"

let applyTextChange (changeEvents: array<TextDocumentContentChangeEvent>) (text: Text) : Text =
    let newContent =
        Array.fold (applyChangeRaw text.lineMap) text.content changeEvents

    mkText newContent

type Span = { text: Text; start: int; end_: int }

type Cursor = { span: Span; pos: int }

module Cursor =
    let char c = c.span.text.content[c.pos]

    let forward c : option<Cursor> =
        if c.span.start < c.span.end_ then
            Some { c with pos = c.pos + 1 }
        else
            None

    let char2 c =
        let char1 = char c
        let char2 = forward c |> Option.map char
        char2 |> Option.map (fun x -> char1, x)

    let charN n c =
        let rec loop acc n cursor : option<list<char>> =
            if n = 0 then
                Some acc
            else
                match cursor with
                | None -> None
                | Some cursor -> loop ((char cursor) :: acc) (n - 1) (forward cursor)

        loop [] n c |> Option.map List.rev


    let forwardN n c =
        let mutable res = forward c
        let mutable i = 1

        while i < n && res.IsSome do
            res <- Option.bind forward res

        res

    let toSpan c : Span = { c.span with start = c.pos }

module Span =
    let range span : Range =
        let start = span.text.lineMap.FindPosition(span.start)

        let stop = span.text.lineMap.FindPosition(span.end_)

        Range.Mk(start.Line, start.Character, stop.Line, stop.Character)

    let toCursor span : option<Cursor> =
        if span.start < span.end_ then
            Some { span = span; pos = span.start }
        else
            None

    let forward span : option<Span> =
        if span.start < span.end_ then
            Some { span with start = span.start + 1 }
        else
            None

    let startChar = toCursor >> (Option.map Cursor.char)

type Line = { text: Text; line: int }

module Line =
    let toSpan line : Span =
        let start, end_ = line.text.LineContentOffsets(line.line)

        { text = line.text; start = start; end_ = end_ }

    let toCursor = toSpan >> Span.toCursor

    let startChar = toSpan >> Span.startChar
