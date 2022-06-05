module Marksman.WorkspaceTest

open Ionide.LanguageServerProtocol.Types

open Xunit

open Marksman.Misc
open Marksman.Workspace

open Marksman.Helpers

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
