module Marksman.WorkspaceText

open Ionide.LanguageServerProtocol.Types

open Xunit

open Marksman.Misc
open Marksman.Workspace

module DocTest =
    [<Fact>]
    let applyLspChange () =
        let empty =
            Doc.mk (PathUri.fromString "/dummy.md") (PathUri.fromString "/") None (Text.mkText "")

        let insertChange =
            { TextDocument = { Uri = "file:///dummy.md"; Version = Some 1 }
              ContentChanges =
                [| { Range = Some(Range.Mk(0, 0, 0, 0))
                     RangeLength = Some 0
                     Text = "[" } |] }

        let updated = Doc.applyLspChange insertChange empty
        Assert.Equal("[", updated.text.content)
