module Marksman.Paths

open System
open System.Text

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
        if
            (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c = '+'
            || c = '/'
            || c = '.'
            || c = '-'
            || c = '_'
            || c = '~'
            || c > '\xFF'
        then
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
    let ofString (str: string) : PathUri =
        let unescaped = Uri.UnescapeDataString(str)
        let uri = Uri(unescaped)
        let localPath = uri.LocalPath

        let isWin =
            localPath.Length >= 2
            && Char.IsLetter(localPath[0])
            && localPath[1] = ':'

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

type RootPath = RootPath of PathUri

module RootPath =
    let ofPath path = RootPath path
    let ofString s = ofPath (PathUri.ofString s)
    let path (RootPath p) = p
