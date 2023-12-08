module Marksman.PathsTests

open Xunit

open Marksman.Misc
open Marksman.Paths

module LocalPathTests =
    [<Fact>]
    let testWinPath () =
        let p = LocalPath.ofSystem "C:/Program Data"
        Assert.Equal("C:/Program Data", LocalPath.toSystem p)
        Assert.Equal<string>([| "C:"; "Program Data" |], LocalPath.components p)

        let p = LocalPath.ofSystem "C:\\Program Data\\notes.txt"
        Assert.Equal("C:\\Program Data\\notes.txt", LocalPath.toSystem p)
        Assert.Equal<string>([| "C:"; "Program Data"; "notes.txt" |], LocalPath.components p)

    [<Fact>]
    let testUnixPath () =
        let p = LocalPath.ofSystem "/"
        Assert.Equal("/", LocalPath.toSystem p)
        Assert.Equal<string>([||], LocalPath.components p)

        let p = LocalPath.ofSystem "/home/user"
        Assert.Equal("/home/user", LocalPath.toSystem p)
        Assert.Equal<string>([| "home"; "user" |], LocalPath.components p)

        let p = LocalPath.ofSystem "/home/user/"
        Assert.Equal("/home/user/", LocalPath.toSystem p)
        Assert.Equal<string>([| "home"; "user" |], LocalPath.components p)

        let p = LocalPath.ofSystem "/home/user/data/../notes/notes.txt"
        Assert.Equal("/home/user/data/../notes/notes.txt", LocalPath.toSystem p)

        Assert.Equal<string>(
            [| "home"; "user"; "data"; ".."; "notes"; "notes.txt" |],
            LocalPath.components p
        )

    [<Fact>]
    let testNormalize () =
        let p = LocalPath.ofSystem "/home/user/data/../notes/notes.txt"
        let np = LocalPath.normalize p
        Assert.Equal("/home/user/notes/notes.txt", LocalPath.toSystem np)

        let p = LocalPath.ofSystem "/../../notes/notes.txt"
        let np = LocalPath.normalize p
        Assert.Equal("/notes/notes.txt", LocalPath.toSystem np)

        let p = LocalPath.ofSystem "./../../notes/notes.txt"
        let np = LocalPath.normalize p
        Assert.Equal("../../notes/notes.txt", LocalPath.toSystem np)

        let p = LocalPath.ofSystem "../data/../../notes/notes.txt"
        let np = LocalPath.normalize p
        Assert.Equal("../../notes/notes.txt", LocalPath.toSystem np)

        let p = LocalPath.ofSystem "C:\\data\\..\\..\\notes\\notes.txt"
        let np = LocalPath.normalize p
        Assert.Equal("C:\\notes\\notes.txt", LocalPath.toSystem np)

module PathUriTests =
    [<Fact>]
    let testWinPathFromUri () =
        let uri = "file:///e%3A/notes"
        let puri = LocalPath.ofUri uri |> LocalPath.toSystem

        Assert.Equal("e:\\notes", puri)

    [<Fact>]
    let testWinPathFromPath () =
        let puri = LocalPath.ofUri "E:\\notes (precious)" |> LocalPath.toSystem

        Assert.Equal("e:\\notes (precious)", puri)

    [<Fact>]
    let testWinDocUriFromUri () =
        let uri = "file:///e%3A/notes"
        let puri = UriWith.mkAbs uri
        Assert.Equal(uri, puri.uri)

    [<Fact>]
    let testWinDocUriFromPath () =
        let path = "E:\\notes"
        let uri = "file:///E%3A/notes"
        let puri = UriWith.mkAbs (systemPathToUriString path)
        Assert.Equal(uri, puri.uri)

    [<Fact>]
    let testRootedRel_SameRootRel () =
        let uri = "file:///a/b/doc.md"
        let root = UriWith.mkRoot uri
        let id = UriWith.mkRooted root (LocalPath.ofUri uri)
        Assert.Equal("file:///a/b/doc.md", id.uri.ToString())

        Helpers.checkInlineSnapshot
            (fun x -> x.ToString())
            (id.data.ToString().Lines())
            [ "{ root = RootPath (AbsPath \"/a/b/doc.md\")"; "  path = None }" ]

    [<Fact>]
    let testAccented_issue274 () =
        let path = "/activité.md"
        let encoded = systemPathToUriString path
        Assert.Equal("file:///activit%C3%A9.md", encoded)
