module Marksman.WorkspaceTest

open System.Runtime.InteropServices
open Ionide.LanguageServerProtocol.Types

open Xunit

open Marksman.Misc
open Marksman.Workspace

open Marksman.Helpers

module FolderTests =
    module ShouldBeIgnoredTests =
        [<Fact>]
        let absGlob_Unix () =
            if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
                let glob = buildGlobs [| "/node_modules" |]
                let root = "/Users/john/notes"
                let ignored = "/Users/john/notes/node_modules"
                shouldBeIgnored glob root ignored |> Assert.True

                let notIgnored = "/Users/john/notes/real.md"
                shouldBeIgnored glob root notIgnored |> Assert.False

        [<Fact>]
        let relGlob_Unix () =
            if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
                let glob = buildGlobs [| "a/b" |]
                let root = "/Users/john/notes"
                let ignored = "/Users/john/notes/a/b"
                shouldBeIgnored glob root ignored |> Assert.True

                let notIgnored = "/Users/john/notes/a/real.md"
                shouldBeIgnored glob root notIgnored |> Assert.False

        [<Fact>]
        let absGlob_Win () =
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                let glob = buildGlobs [| "/node_modules" |]
                let root = "C:\\notes"
                let ignored = "C:\\notes\\node_modules"
                shouldBeIgnored glob root ignored |> Assert.True

                let notIgnored = "C:\\notes\\real.md"
                shouldBeIgnored glob root notIgnored |> Assert.False

        [<Fact>]
        let relGlob_Win () =
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                let glob = buildGlobs [| "a/b" |]
                let root = "C:\\notes"
                let ignored = "C:\\notes\\a\\b"
                shouldBeIgnored glob root ignored |> Assert.True

                let notIgnored = "C:\\notes\\a\\real.md"
                shouldBeIgnored glob root notIgnored |> Assert.False


module DocTest =
    [<Fact>]
    let applyLspChange () =
        let dummyPath = (dummyRootPath [ "dummy.md" ])

        let empty =
            Doc.mk
                (PathUri.fromString dummyPath)
                (PathUri.fromString dummyRoot)
                None
                (Text.mkText "")

        let insertChange =
            { TextDocument = { Uri = pathToUri dummyPath; Version = Some 1 }
              ContentChanges =
                [| { Range = Some(Range.Mk(0, 0, 0, 0))
                     RangeLength = Some 0
                     Text = "[" } |] }

        let updated = Doc.applyLspChange insertChange empty
        Assert.Equal("[", (Doc.text updated).content)
