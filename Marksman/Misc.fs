module Marksman.Misc

open System
open System.Text
open Ionide.LanguageServerProtocol.Types

let todo what = failwith $"{what} not implemented"

let flip (f: 'a -> 'b -> 'c) : 'b -> 'a -> 'c = fun b a -> f a b

let lineEndings = [| "\r\n"; "\n" |]

type String with
    member this.Lines() : array<string> =
        this.Split(lineEndings, StringSplitOptions.None)

    member this.EndsWithNewline() : bool =
        Array.exists<string> this.EndsWith lineEndings

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
            let isPunct =
                Char.IsPunctuation(char) || Char.IsSymbol(char)

            let isSep =
                Char.IsWhiteSpace(char) || char = '-'

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

    let indentedLines =
        reprA.Lines() |> Array.map (fun x -> "  " + x)

    String.Join(Environment.NewLine, indentedLines)

[<CustomEquality; CustomComparison>]
type PathUri =
    | PathUri of Uri
    member this.Uri: Uri =
        let (PathUri uri) = this
        uri

    member this.DocumentUri =
        this.Uri.OriginalString

    member this.LocalPath: string =
        let unprocessed = this.Uri.LocalPath

        let isWin =
            unprocessed.Length >= 3
            && match unprocessed[ 0..2 ].ToCharArray() with
               | [| '/'; drive; ':' |] when Char.IsLetter drive -> true
               | _ -> false

        if isWin then
            // Seems like trimming the leading slash without turning all forward slashes into back-slashes is enough to make Windows recognize paths.
            unprocessed.TrimStart('/')
        else
            unprocessed

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

module PathUri =
    let fromString (str: string) : PathUri = PathUri(Uri(str))

type Range with
    static member Mk(startLine: int, startChar: int, endLine: int, endChar: int) : Range =
        { Start =
            { Line = startLine
              Character = startChar }
          End = { Line = endLine; Character = endChar } }
