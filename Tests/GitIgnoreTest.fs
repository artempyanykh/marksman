module Marksman.GitIgnoreTest

open System.Runtime.InteropServices
open Xunit

open Marksman.GitIgnore

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
