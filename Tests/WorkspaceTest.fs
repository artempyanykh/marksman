module Marksman.WorkspaceTest

open GlobExpressions
open Ionide.LanguageServerProtocol.Types

open Xunit

open Marksman.Misc
open Marksman.Workspace

open Marksman.Helpers

module FolderTests =
    module ShouldBeIgnoredTests =
        [<Fact>]
        let absGlob_Unix () =
            let glob = Glob("/node_modules")
            let root = "/Users/john/notes"
            let ignored = "/Users/john/notes/node_modules"
            Folder.shouldBeIgnored [|glob|] root ignored |> Assert.True
            
            let notIgnored = "/Users/john/notes/real.md"
            Folder.shouldBeIgnored [|glob|] root notIgnored |> Assert.False
            
        [<Fact>]
        let absGlob_Win () =
            let glob = Glob("/node_modules")
            let root = "C:\\notes"
            let ignored = "C:\\notes\\node_modules"
            Folder.shouldBeIgnored [|glob|] root ignored |> Assert.True
            
            let notIgnored = "C:\\notes\\real.md"
            Folder.shouldBeIgnored [|glob|] root notIgnored |> Assert.False
        

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
        Assert.Equal("[", updated.text.content)
