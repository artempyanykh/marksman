module Marksman.Misc

open System
open Ionide.LanguageServerProtocol.Types

let todo what = failwith $"{what} not implemented"

let lineEndings = [| "\r"; "\n"; "\r\n" |]

type String with
    member this.Lines() : array<string> =
        this.Split(lineEndings, StringSplitOptions.None)

    member this.EndsWithNewline() : bool =
        Array.exists<string> this.EndsWith lineEndings

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

    member this.AbsolutePath: string =
        this.Uri.AbsolutePath


    override this.Equals(obj) =
        match obj with
        | :? PathUri as other -> this.AbsolutePath.Equals(other.AbsolutePath)
        | _ -> false


    override this.GetHashCode() = this.AbsolutePath.GetHashCode()

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? PathUri as other -> this.AbsolutePath.CompareTo(other.AbsolutePath)
            | _ -> failwith "Incompatible Type"
