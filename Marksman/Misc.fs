module Marksman.Misc

open System
open System.Text
open Ionide.LanguageServerProtocol.Types

let todo what = failwith $"{what} not implemented"

let flip (f: 'a -> 'b -> 'c) : 'b -> 'a -> 'c = fun b a -> f a b

let lineEndings = [| "\r\n"; "\n" |]

type String with
    member this.Lines() : array<string> = this.Split(lineEndings, StringSplitOptions.None)

    member this.EndsWithNewline() : bool = Array.exists<string> this.EndsWith lineEndings

    member this.IsSubSequenceOf(other: string) : bool =
        let rec isMatching thisIdx otherIdx =
            if thisIdx >= this.Length then
                true
            else if otherIdx >= other.Length then
                false
            else
                match this[thisIdx], other[otherIdx] with
                | thisChar, otherChar when Char.ToLower(thisChar) = Char.ToLower(otherChar) ->
                    isMatching (thisIdx + 1) (otherIdx + 1)
                | _ -> isMatching thisIdx (otherIdx + 1)

        isMatching 0 0

    member this.Slug() : string =
        let mutable sb = StringBuilder()
        let mutable sepSeen = false
        let mutable chunkState = 0 // 0 no text chunk, 1 chunk in progress, 2 finished

        for char in this.ToCharArray() do
            let isPunct = Char.IsPunctuation(char) || Char.IsSymbol(char)

            let isSep = Char.IsWhiteSpace(char) || char = '-'

            let isToOut = not isPunct && not isSep

            if isSep then sepSeen <- true

            if isToOut then
                if sepSeen && chunkState = 2 then
                    sb <- sb.Append('-')
                    sepSeen <- false

                chunkState <- 1
                sb <- sb.Append(Char.ToLower(char))
            else if chunkState = 1 then
                chunkState <- 2

        sb.ToString()

let indentFmt (fmtA: 'A -> string) (a: 'A) =
    let reprA = fmtA a

    let indentedLines = reprA.Lines() |> Array.map (fun x -> "  " + x)

    String.Join(Environment.NewLine, indentedLines)

[<CustomEquality; CustomComparison>]
type PathUri =
    private
        { uri: string
          localPath: string }

    member this.DocumentUri: string = this.uri

    member this.LocalPath: string = this.localPath

    override this.Equals(obj) =
        match obj with
        | :? PathUri as other -> this.LocalPath.Equals(other.LocalPath)
        | _ -> false


    override this.GetHashCode() = this.LocalPath.GetHashCode()

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? PathUri as other -> this.LocalPath.CompareTo(other.LocalPath)
            | _ -> failwith "Incompatible Type"


// https://github.dev/fsharp/FsAutoComplete/blob/d90597c2e073b7e88390f2b1933b031ff8a9a009/src/FsAutoComplete.Core/Utils.fs#L635
let localPathToUriString (filePath: string) : string =
    let uri = StringBuilder(filePath.Length)

    for c in filePath do
        if (c >= 'a' && c <= 'z')
           || (c >= 'A' && c <= 'Z')
           || (c >= '0' && c <= '9')
           || c = '+'
           || c = '/'
           || c = '.'
           || c = '-'
           || c = '_'
           || c = '~'
           || c > '\xFF' then
            uri.Append(c) |> ignore
        // handle windows path separator chars.
        // we _would_ use Path.DirectorySeparator/AltDirectorySeparator, but those vary per-platform and we want this
        // logic to work cross-platform (for tests)
        else if c = '\\' then
            uri.Append('/') |> ignore
        else
            uri.Append('%') |> ignore
            uri.Append((int c).ToString("X2")) |> ignore

    if uri.Length >= 2 && uri[0] = '/' && uri[1] = '/' then // UNC path
        "file:" + uri.ToString()
    else
        "file:///" + uri.ToString().TrimStart('/')

module PathUri =
    let fromString (str: string) : PathUri =
        let unescaped = Uri.UnescapeDataString(str)
        let uri = Uri(unescaped)
        let localPath = uri.LocalPath

        let isWin =
            localPath.Length >= 2 && Char.IsLetter(localPath[0]) && localPath[1] = ':'

        let localPath =
            if isWin then
                Char.ToLower(localPath[0]).ToString() + localPath[1..]
            else
                localPath

        let escapedUri =
            if str.StartsWith("file://") then
                str
            else
                localPathToUriString localPath

        { uri = escapedUri; localPath = localPath }


type Position with
    static member Mk(line: int, char: int) : Position = { Line = line; Character = char }
    static member MkLine(line: int) : Position = Position.Mk(line, 0)
    // TODO: use text for precise loc
    member this.NextChar(n: int) : Position = { Line = this.Line; Character = this.Character + n }

    // TODO: use text for precise loc
    member this.PrevChar(n: int) : Position =
        if this.Character <= n then
            failwith $"Start of line doesn't have a previous char: ${this.DebuggerDisplay}"
        else
            { Line = this.Line; Character = this.Character - n }

type Range with
    static member Mk(startLine: int, startChar: int, endLine: int, endChar: int) : Range =
        { Start = Position.Mk(startLine, startChar)
          End = Position.Mk(endLine, endChar) }

    static member Mk(start: Position, end_: Position) : Range = { Start = start; End = end_ }

    member this.ContainsInclusive(pos: Position) : bool = this.Start <= pos && pos <= this.End
