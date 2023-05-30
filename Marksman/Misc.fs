module Marksman.Misc

open System
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Text.RegularExpressions

open Ionide.LanguageServerProtocol.Types

let flip (f: 'a -> 'b -> 'c) : 'b -> 'a -> 'c = fun b a -> f a b

let lineEndings = [| "\r\n"; "\n" |]

let concatLines (lines: array<string>) : string = String.concat Environment.NewLine lines

let mkWatchGlob (configuredExts: seq<string>) : string =
    let ext_pattern = "{" + (String.concat "," (configuredExts)) + "}"
    $"**/*.{ext_pattern}"

let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

let isMarkdownFile (configuredExts: seq<string>) (path: string) : bool =
    let isEmacsBackup =
        try
            (Path.GetFileName path).StartsWith(".#")
        with :? ArgumentException ->
            false

    if isEmacsBackup then
        false
    else
        // GetExtension returns extension with a '.'. In config we don't have '.'s.
        let ext = (Path.GetExtension path)

        match ext with
        | null -> false
        | ext ->
            let ext = ext.TrimStart('.').ToLowerInvariant()
            Seq.contains ext configuredExts

let isPotentiallyMarkdownFile (configuredExts: seq<string>) (path: string) : bool =
    let ext = Path.GetExtension path

    match ext with
    | null
    | "" -> true
    | _ -> isMarkdownFile configuredExts path

let fmtOption fmt value =
    match value with
    | Some value -> $"{fmt value}"
    | None -> "∅"

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

        for char in this.ToCharArray() do
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

type Slug = Slug of string

module Slug =
    let ofString (s: string) = Slug(s.Slug())

    let toString (Slug s) = s

    let str (s: string) = s.Slug()

    let isSubSequence (sub: Slug) (sup: Slug) =
        let (Slug sub) = sub
        let (Slug sup) = sup
        sub.IsSubSequenceOf(sup)

    let isSubString (sub: Slug) (sup: Slug) =
        let (Slug sub) = sub
        let (Slug sup) = sup
        sub.IsSubStringOf(sup)

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

    static member Mk(startLine: int, startChar: int, endLine: int, endChar: int) : Range =
        { Start = Position.Mk(startLine, startChar)
          End = Position.Mk(endLine, endChar) }

    static member Mk(start: Position, end_: Position) : Range = { Start = start; End = end_ }

    member this.IsEmpty() : bool = this.Start >= this.End

    member this.ContainsInclusive(pos: Position) : bool = this.Start <= pos && pos <= this.End

type LinkLabel = private LinkLabel of string

module LinkLabel =
    let private consecutiveWhitespacePattern = Regex(@"\s+")

    let ofString (s: string) =
        let normCase = s.Normalize().ToLowerInvariant().Trim()
        let normWs = consecutiveWhitespacePattern.Replace(normCase, " ")
        LinkLabel normWs

    let isSubSequenceOf (LinkLabel other) (LinkLabel this) = other.IsSubSequenceOf(this)

type SuffixTree<'K, 'V> when 'K: comparison =
    { nodes: Map<'K, SuffixTree<'K, 'V>>; value: option<'V> }

module SuffixTree =
    let empty = { nodes = Map.empty; value = None }

    let hasChildren st = Map.isEmpty st.nodes |> not

    let hasValue st = Option.isSome st.value

    let hasData st = hasValue st || hasChildren st

    let add (key: list<'K>) (value: 'V) (st: SuffixTree<'K, 'V>) : SuffixTree<'K, 'V> =
        let rec go key st =
            match key with
            | [] -> { st with value = Some value }
            | keyHead :: keyTail ->
                let tailTree = Map.tryFind keyHead st.nodes |> Option.defaultValue empty
                let tailTree = go keyTail tailTree
                { nodes = Map.add keyHead tailTree st.nodes; value = st.value }

        go (List.rev key) st

    let remove (key: list<'K>) (st: SuffixTree<'K, 'V>) : SuffixTree<'K, 'V> =
        let rec go key st =
            match key with
            | [] -> if hasData st then Some { st with value = None } else None
            | keyHd :: keyTl ->
                let withUpdatedTail tlTree =
                    match go keyTl tlTree with
                    | Some tlTree -> Some { st with nodes = Map.add keyHd tlTree st.nodes }
                    | None ->
                        let st = { st with nodes = Map.remove keyHd st.nodes }
                        if hasData st then Some st else None

                Map.tryFind keyHd st.nodes
                |> Option.map withUpdatedTail
                |> Option.defaultValue (Some st)

        go (List.rev key) st |> Option.defaultValue empty

    let ofSeq (sx: seq<list<'K> * 'V>) : SuffixTree<'K, 'V> =
        Seq.fold (fun st (key, value) -> add key value st) empty sx

    let rec private findSubtree keyRev st =
        match keyRev with
        | [] -> st
        | keyHead :: keyTail ->
            match Map.tryFind keyHead st.nodes with
            | None -> empty
            | Some sub -> findSubtree keyTail sub

    let rec collectValues (st: SuffixTree<'K, 'V>) =
        seq {
            match st.value with
            | Some v -> yield v
            | None -> ()

            for sub in st.nodes |> Map.values do
                yield! collectValues sub
        }


    let filterMatchingValues (key: list<'K>) (st: SuffixTree<'K, 'V>) : seq<'V> =
        let key = List.rev key
        let st = findSubtree key st
        st |> collectValues
