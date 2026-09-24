module Marksman.GitIgnoreTest

open System.Runtime.InteropServices
open Xunit

open Marksman.GitIgnore

[<Fact>]
let patternToGlob_Empty () = Assert.Equal<GlobExpressions.Glob>([||], patternToGlob "")

[<Fact>]
let absGlob_Unix () =
    if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
        let root = "/Users/john/notes"
        let glob = GlobMatcher.mk root [| "/node_modules" |]
        let ignored = "/Users/john/notes/node_modules"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "/Users/john/notes/real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False

[<Fact>]
let relGlob_Unix_1 () =
    if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
        let root = "/Users/john/notes"
        let glob = GlobMatcher.mk root [| "node_modules/" |]
        let ignored = "/Users/john/notes/node_modules"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "/Users/john/notes/node_modules/"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "/Users/john/notes/node_modules/foo.md"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "/Users/john/notes/real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False

[<Fact>]
let relGlob_Unix_2 () =
    if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
        let root = "/Users/john/notes"
        let glob = GlobMatcher.mk root [| "node_modules/" |]
        let ignored = "/Users/john/notes/sub/node_modules"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "/Users/john/notes/sub/node_modules/"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "/Users/john/notes/sub/node_modules/foo.md"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "/Users/john/notes/sub/real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False

[<Fact>]
let relGlob_Unix_3 () =
    if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
        let root = "/Users/john/notes"
        let glob = GlobMatcher.mk root [| "a/b" |]
        let ignored = "/Users/john/notes/a/b"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "/Users/john/notes/a/real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False

[<Fact>]
let absGlob_Win () =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        let root = "C:\\notes"
        let glob = GlobMatcher.mk root [| "/node_modules" |]
        let ignored = "C:\\notes\\node_modules"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "C:\\notes\\real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False


[<Fact>]
let relGlob_Win_1 () =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        let root = "C:\\notes"
        let glob = GlobMatcher.mk root [| "node_modules/" |]

        let ignored = "C:\\notes\\node_modules"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "C:\\notes\\node_modules\\"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "C:\\notes\\node_modules\\foo.md"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "C:\\notes\\real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False

[<Fact>]
let relGlob_Win_2 () =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        let root = "C:\\notes"
        let glob = GlobMatcher.mk root [| "node_modules/" |]

        let ignored = "C:\\notes\\sub\\node_modules"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "C:\\notes\\sub\\node_modules\\"
        GlobMatcher.ignores glob ignored |> Assert.True

        let ignored = "C:\\notes\\sub\\node_modules\\foo.md"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "C:\\notes\\sub\\real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False

[<Fact>]
let relGlob_Win_3 () =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        let root = "C:\\notes"
        let glob = GlobMatcher.mk root [| "a/b" |]
        let ignored = "C:\\notes\\a\\b"
        GlobMatcher.ignores glob ignored |> Assert.True

        let notIgnored = "C:\\notes\\a\\real.md"
        GlobMatcher.ignores glob notIgnored |> Assert.False

[<Fact>]
let issue_218 () =
    if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
        let root = "/Users/john/notes"
        let glob = GlobMatcher.mk root [| "*.foo[o,p]" |]
        // FN because the pattern is not supported
        GlobMatcher.ignores glob "zip.foop" |> Assert.False
        // FN because the pattern is not supported
        GlobMatcher.ignores glob "zap.fooo" |> Assert.False
        // TN
        GlobMatcher.ignores glob "zap.foos" |> Assert.False

[<Fact>]
let issue_428 () =
    let root = "/Users/john/notes"
    let patterns =
        [| "a/**"
           "!a/b/"
           "!a/b/c/"
           "!a/b/c/**"
           "d/**" |]
    let glob = GlobMatcher.mk root patterns
    let ignored_1 = "/Users/john/notes/a/private/file.md"
    let ignored_2 = "/Users/john/notes/a/file.md"
    let ignored_3 = "/Users/john/notes/a/b/file.md"
    let ignored_4 = "/Users/john/notes/d/file.md"
    let notIgnored = "/Users/john/notes/a/b/c/file.md"
    Assert.True(GlobMatcher.ignores glob ignored_1)
    Assert.True(GlobMatcher.ignores glob ignored_2)
    Assert.True(GlobMatcher.ignores glob ignored_3)
    Assert.True(GlobMatcher.ignores glob ignored_4)
    Assert.False(GlobMatcher.ignores glob notIgnored)
