module Marksman.PathsTests

open Xunit

open Marksman.Paths

module PathUriTests =
    [<Fact>]
    let testWinPathFromUri () =
        let uri = "file:///e%3A/notes"
        let puri = PathUri.ofString uri

        Assert.Equal("e:\\notes", puri.LocalPath)

    [<Fact>]
    let testWinPathFromPath () =
        let puri = PathUri.ofString "E:\\notes (precious)"

        Assert.Equal("e:\\notes (precious)", puri.LocalPath)

    [<Fact>]
    let testWinDocUriFromUri () =
        let uri = "file:///e%3A/notes"
        let puri = PathUri.ofString uri
        Assert.Equal(uri, puri.DocumentUri)

    [<Fact>]
    let testWinDocUriFromPath () =
        let path = "E:\\notes"
        let uri = "file:///e%3A/notes"
        let puri = PathUri.ofString path
        Assert.Equal(uri, puri.DocumentUri)

