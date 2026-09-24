module Marksman.GitIgnore

open System
open System.IO
open GlobExpressions
open Ionide.LanguageServerProtocol.Logging

type GlobPattern =
    | Include of Glob
    | Exclude of Glob

let private logger = LogProvider.getLoggerByName "GitIgnore"

let patternToGlob (pat: string) : array<Glob> =
    if String.IsNullOrWhiteSpace(pat) then
        [||]
    else
        let firstSlashIdx = pat.IndexOf('/')
        let isAbsolute = firstSlashIdx <> pat.Length - 1
        let isDir = pat[pat.Length - 1] = '/'
        let pat = if pat.StartsWith("/") then pat.Substring(1) else pat
        let pat = if isAbsolute then pat else "**/" + pat

        let opts = GlobOptions.Compiled

        try
            if isDir then
                [|
                    Glob(pat + "**", opts)
                    Glob(pat.Substring(0, pat.Length - 1), opts)
                |]
            else
                [| Glob(pat, opts) |]
        with :? GlobPatternException ->
            logger.warn (Log.setMessage "Unsupported glob pattern" >> Log.addContext "pat" pat)
            [||]

let mkGlobPattern (pat: string) : array<GlobPattern> =
    if pat.StartsWith("#") then
        [||]
    else if pat.StartsWith("!") then
        let pat = pat.Substring(1)
        let isDir = pat.EndsWith("/")
        if isDir then
            let dirPat = if pat.StartsWith("/") then pat.Substring(1, pat.Length - 2) else pat.Substring(0, pat.Length - 1)
            let dirPat = if pat.IndexOf('/') = pat.Length - 1 then "**/" + dirPat else dirPat
            try
                [| Include (GlobExpressions.Glob(dirPat, GlobExpressions.GlobOptions.Compiled)) |]
            with :? GlobExpressions.GlobPatternException ->
                logger.warn (Log.setMessage "Unsupported glob pattern" >> Log.addContext "pat" pat)
                [||]
        else
            patternToGlob pat |> Array.map Include
    else
        patternToGlob pat |> Array.map Exclude

type GlobMatcher = { root: string; patterns: array<GlobPattern> }

module GlobMatcher =

    let mk (root: string) (lines: array<string>) : GlobMatcher =
        let patterns = lines |> Array.collect mkGlobPattern

        { root = root; patterns = patterns }

    let mkDefault (root: string) : GlobMatcher = mk root [| ".git"; ".hg" |]

    let ignores (matcher: GlobMatcher) (path: string) : bool =
        let relPath = Path.GetRelativePath(matcher.root, path)
        let mutable lastMatch : option<bool> = None
        for pat in matcher.patterns do
            match pat with
            | Include glob -> if glob.IsMatch(relPath) then lastMatch <- Some false
            | Exclude glob -> if glob.IsMatch(relPath) then lastMatch <- Some true
        match lastMatch with
        | Some r -> r
        | None -> false

    let ignoresAny (matchers: seq<GlobMatcher>) (path: string) : bool =
        Seq.exists (fun m -> ignores m path) matchers
