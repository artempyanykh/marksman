module Marksman.Misc

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Text.RegularExpressions

open Ionide.LanguageServerProtocol.Types

let flip (f: 'A -> 'B -> 'C) : 'B -> 'A -> 'C = fun b a -> f a b

let lineEndings = [| "\r\n"; "\n" |]

let concatLines (lines: seq<string>) : string = String.concat Environment.NewLine lines

let mkWatchGlob (configuredExts: seq<string>) : string =
    let ext_pattern = "{" + (String.concat "," configuredExts) + "}"
    $"**/*.{ext_pattern}"

let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

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

        if this.IsEmpty() then true else isMatching 0 0

    member this.IsSubStringOf(other: string) : bool = other.Contains(this)

    member this.IsEmpty() : bool = String.IsNullOrEmpty(this)

    member this.IsWhitespace() : bool = String.IsNullOrWhiteSpace(this)

    member this.Slug() : string =
        let mutable sb = StringBuilder()
        let mutable sepSeen = false
        let mutable chunkState = 0 // 0 no text chunk, 1 chunk in progress, 2 finished

        for char in this.Trim().ToCharArray() do
            let isPunct = Char.IsPunctuation(char) || Char.IsSymbol(char)

            let isSep = Char.IsWhiteSpace(char) || char = '-'

            let isToOut = (not isPunct && not isSep)

            if isSep then
                sepSeen <- true

            if isToOut then
                if sepSeen && chunkState = 2 then
                    sb <- sb.Append('-')
                    sepSeen <- false

                chunkState <- 1

                sb <- sb.Append(Char.ToLower(char))
            else if chunkState = 1 then
                chunkState <- 2

        sb.ToString()

    member this.EncodeForWiki() : string =
        let replacement = [| "#", "%23"; "[", "%5B"; "]", "%5D"; "|", "%7C" |]

        replacement
        |> Array.fold (fun (sb: StringBuilder) -> sb.Replace) (StringBuilder(this))
        |> (fun x -> x.ToString())

    member this.EncodePathForWiki() : string =
        let parts = this.TrimStart('/').Split([| '\\'; '/' |])
        // We do decode and then encode because path components here can be raw
        // url-encoded strings coming straight from document's source.
        let encodedParts =
            parts |> Seq.map (fun s -> s.UrlDecode().EncodeForWiki())

        // NOTE: NO LEADING SLASH HERE. Without the leading slash the paths may be ambiguous
        // but this is how other tools want the paths to look like.
        // See: https://github.com/artempyanykh/marksman/issues/162
        // See: RefsTests.FileLinkTests.fileName_RelativeAsAbs
        String.Join('/', encodedParts)

    member this.UrlEncode() : string = Uri.EscapeDataString(this)

    member this.UrlDecode() : string = Uri.UnescapeDataString(this)

    member this.AbsPathUrlEncode() : string =
        let parts = this.TrimStart('/').Split([| '\\'; '/' |])
        // We do decode and then encode because path components here can be raw
        // url-encoded strings coming straight from document's source.
        let encodedParts = parts |> Seq.map (fun s -> s.UrlDecode().UrlEncode())
        "/" + String.Join('/', encodedParts)

    member this.AsUnixAbsPath() : string =
        let parts = this.TrimStart('/').Split([| '\\'; '/' |])
        let joined = String.Join('/', parts)
        if joined.StartsWith('/') then joined else "/" + joined

    member this.AbsPathUrlEncodedToRelPath() : string = this.TrimStart('/').UrlDecode()

    member this.TrimPrefix(prefix: string) : string =
        if this.StartsWith(prefix) then
            this.Substring(prefix.Length)
        else
            this

    member this.TrimSuffix(suffix: string) : string =
        if this.EndsWith(suffix) then
            this.Substring(0, this.Length - suffix.Length)
        else
            this

    member this.TrimBoth(prefix: string, suffix: string) : string =
        this.TrimPrefix(prefix).TrimSuffix(suffix)

let isEmacsBackup (path: string) =
    try
        (Path.GetFileName path).StartsWith(".#")
    with :? ArgumentException ->
        false

let isMarkdownFile (configuredExts: seq<string>) (path: string) : bool =
    if isEmacsBackup path then
        false
    else
        // GetExtension returns extension with a '.'. In config we don't have '.'s.
        let ext = (Path.GetExtension path)

        match ext with
        | null -> false
        | ext ->
            let ext = ext.TrimStart('.').ToLowerInvariant()
            Seq.contains ext configuredExts

let chopMarkdownExt (configuredExts: seq<string>) (path: string) : string =
    if isMarkdownFile configuredExts path then
        path.TrimSuffix(Path.GetExtension path)
    else
        path

let ensureMarkdownExt (configuredExts: seq<string>) (path: string) : string =
    if isMarkdownFile configuredExts path then
        path
    else
        let ext = Seq.head configuredExts
        $"{path}.{ext}"

let isPotentiallyMarkdownFile (configuredExts: seq<string>) (path: string) : bool =
    let ext = Path.GetExtension path

    match ext with
    | null
    | "" -> true
    | _ -> isMarkdownFile configuredExts path

let isPotentiallyInternalRef (configuredExts: seq<string>) (name: string) : bool =
    if Uri.IsWellFormedUriString(name, UriKind.Absolute) then
        false
    else
        isPotentiallyMarkdownFile configuredExts name

let fmtOption fmt value =
    match value with
    | Some value -> $"{fmt value}"
    | None -> "∅"

type Slug =
    | Slug of string

    member this.Raw =
        let (Slug str) = this
        str

module Slug =
    let ofString (s: string) = Slug(s.Slug())

    let toString (Slug s) = s

    let str (s: string) = s.Slug()

    let isEmpty (Slug s) = String.IsNullOrEmpty s

    let isSubSequence (sub: Slug) (sup: Slug) =
        let (Slug sub) = sub
        let (Slug sup) = sup
        sub.IsSubSequenceOf(sup)

    let isSubString (sub: Slug) (sup: Slug) =
        let (Slug sub) = sub
        let (Slug sup) = sup
        sub.IsSubStringOf(sup)

    let equalStrings (s1: string) (s2: string) = ofString s1 = ofString s2

let indentFmt (fmtA: 'A -> string) (a: 'A) =
    let reprA = fmtA a

    let indentedLines = reprA.Lines() |> Array.map (fun x -> "  " + x)

    String.Join(Environment.NewLine, indentedLines)


type Position with

    static member Mk(line: int, char: int) : Position = { Line = line; Character = char }
    static member MkLine(line: int) : Position = Position.Mk(line, 0)
    // TODO: use text for precise loc
    member this.NextChar(n: int) : Position = { Line = this.Line; Character = this.Character + n }

    // TODO: use text for precise loc
    member this.PrevChar(n: int) : Position =
        if this.Character <= n then
            failwith $"Start of line doesn't have a previous char: {this}"
        else
            { Line = this.Line; Character = this.Character - n }

type Range with

    static member Mk(startLine: int, startChar: int, endLine: int, endChar: int) : Range = {
        Start = Position.Mk(startLine, startChar)
        End = Position.Mk(endLine, endChar)
    }

    static member Mk(start: Position, end_: Position) : Range = { Start = start; End = end_ }

    member this.IsEmpty() : bool = this.Start >= this.End

    member this.ContainsInclusive(pos: Position) : bool = this.Start <= pos && pos <= this.End

[<Struct>]
[<StructuredFormatDisplay("{AsString}")>]
type LinkLabel =
    private
    | LinkLabel of string

    override this.ToString() =
        let (LinkLabel s) = this
        s

    member this.AsString = this.ToString()

module LinkLabel =
    let private consecutiveWhitespacePattern = Regex(@"\s+")

    let ofString (s: string) =
        let normCase = s.Normalize().ToLowerInvariant().Trim()
        let normWs = consecutiveWhitespacePattern.Replace(normCase, " ")
        LinkLabel normWs

    let isSubSequenceOf (LinkLabel other) (LinkLabel this) = other.IsSubSequenceOf(this)

[<StructuredFormatDisplay("{AsString}")>]
type Indented<'A> =
    | Indented of int * 'A

    override this.ToString() =
        let (Indented(indent, inner)) = this

        let lines =
            seq {
                for line in inner.ToString().Lines() do
                    String.replicate indent " " + line
            }

        concatLines lines

    member this.AsString = this.ToString()

type Difference<'A> when 'A: comparison = {
    added: Set<'A>
    removed: Set<'A>
} with

    member this.CompactFormat() =
        let lines =
            seq {
                if not (Set.isEmpty this.added) then
                    yield "Added:"

                    for x in this.added do
                        yield Indented(4, x).ToString()

                if not (Set.isEmpty this.removed) then
                    yield "Removed:"

                    for x in this.removed do
                        yield Indented(4, x).ToString()
            }

        concatLines lines

module Difference =
    let empty = { added = Set.empty; removed = Set.empty }

    let isEmpty { added = added; removed = removed } = Set.isEmpty added && Set.isEmpty removed

    let mk (before: seq<'A>) (after: seq<'A>) : Difference<'A> =
        let before = Set.ofSeq before
        let after = Set.ofSeq after

        { added = after - before; removed = before - after }

    let map f { added = added; removed = removed } = {
        added = Set.map f added
        removed = Set.map f removed
    }

type FullDifference<'A> when 'A: comparison = {
    added: Set<'A>
    removed: Set<'A>
    changed: Set<'A>
    unchanged: Set<'A>
}

let getAssemblyVersion () : string =
    let assembly = Assembly.GetExecutingAssembly()
    assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
    |> Option.ofObj
    |> Option.map (fun attr -> attr.InformationalVersion)
    |> Option.defaultValue (assembly.GetName().Version.ToString())
