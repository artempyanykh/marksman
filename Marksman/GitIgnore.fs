module Marksman.GitGlob

type MsMatcher = Microsoft.Extensions.FileSystemGlobbing.Matcher

type Glob =
    | Include of string
    | Exclude of string

let patternToMsPattern (pat: string) : string =
    let firstSlashIdx = pat.IndexOf('/')
    let isAbsolute = firstSlashIdx <> pat.Length - 1
    let isDir = pat[pat.Length - 1] = '/'
    let pat = if isAbsolute then pat else "**/" + pat
    let pat = if isDir then pat.Substring(0, pat.Length - 2) else pat
    pat


let patternToGlob (pat: string) : option<Glob> =
    if pat.StartsWith("#") then
        None
    else if pat.StartsWith("!") then
        let pat = pat.Substring(1)
        Include(patternToMsPattern pat) |> Some
    else
        Exclude(patternToMsPattern pat) |> Some

type GlobMatcher = { root: string; matcher: MsMatcher }

module GlobMatcher =
    open Microsoft.Extensions.FileSystemGlobbing

    let mk (root: string) (lines: array<string>) : GlobMatcher =
        let globs =
            lines
            |> Array.collect (fun line -> patternToGlob line |> Option.toArray)

        let matcher = MsMatcher().AddInclude("**/*")
        matcher.AddExcludePatterns([| ".git"; ".hg" |])

        for glob in globs do
            match glob with
            | Exclude pat -> matcher.AddExclude(pat) |> ignore
            | Include pat -> matcher.AddInclude(pat) |> ignore

        { root = root; matcher = matcher }
