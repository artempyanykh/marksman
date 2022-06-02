module Marksman.WorkspaceTest

open System.Runtime.InteropServices
open Ionide.LanguageServerProtocol.Types

open Xunit

open Marksman.Misc
open Marksman.Workspace

module DocTest =
    let dummyRoot =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            "c:/"
        else
            "/"

    let mkDummyPath components = dummyRoot + (components |> String.concat "/")

    [<Fact>]
    let applyLspChange () =
        let dummyPath = (mkDummyPath [ "dummy.md" ])

        let empty =
            Doc.mk
                (PathUri.fromString dummyPath)
                (PathUri.fromString dummyRoot)
                None
                (Text.mkText "")

        let insertChange =
            { TextDocument = { Uri = $"file://{dummyPath}"; Version = Some 1 }
              ContentChanges =
                [| { Range = Some(Range.Mk(0, 0, 0, 0))
                     RangeLength = Some 0
                     Text = "[" } |] }

        let updated = Doc.applyLspChange insertChange empty
        Assert.Equal("[", updated.text.content)
