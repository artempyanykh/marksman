module Marksman.WorkspaceTest

open Ionide.LanguageServerProtocol.Types

open Xunit

open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Workspace
open Marksman.Config

open Marksman.Helpers

module FolderTest =
    [<Fact>]
    let rooPath_singleFile () =
        let d1 = FakeDoc.Mk(content = "", path = "a/b/d1.md", root = "a")
        let f1 = Folder.singleFile d1 None

        Assert.Equal((dummyRootPath [ "a" ] |> mkFolderId).data, Folder.rootPath f1)

    [<Fact>]
    let updateDoc () =
        let d1 = FakeDoc.Mk(content = "# Title 1", path = "doc1.md")
        let d2 = FakeDoc.Mk(content = "# Title 2", path = "doc2.md")
        let f = FakeFolder.Mk([ d1; d2 ])

        Assert.Equal(1, Folder.filterDocsBySlug (Slug.ofString "Title 1") f |> Seq.length)
        Assert.Equal(0, Folder.filterDocsBySlug (Slug.ofString "Title 0") f |> Seq.length)

        Assert.Equal(1, Folder.filterDocsByInternPath (Approx(RelPath "doc1")) f |> Seq.length)
        Assert.Equal(0, Folder.filterDocsByInternPath (Approx(RelPath "doc0")) f |> Seq.length)

        // Updating a title of the doc; by slug lookup should reflect
        let d1 = FakeDoc.Mk(content = "# Title 2", path = "doc1.md")
        let f = Folder.withDoc d1 f

        Assert.Equal(0, Folder.filterDocsBySlug (Slug.ofString "Title 1") f |> Seq.length)
        Assert.Equal(2, Folder.filterDocsBySlug (Slug.ofString "Title 2") f |> Seq.length)

        Assert.Equal(1, Folder.filterDocsByInternPath (Approx(RelPath "doc1")) f |> Seq.length)
        Assert.Equal(1, Folder.filterDocsByInternPath (Approx(RelPath "doc2")) f |> Seq.length)

        // Removing a doc; both by slug and by path should reflect
        let f = Folder.withoutDoc d1.Id f |> Option.get

        Assert.Equal(0, Folder.filterDocsBySlug (Slug.ofString "Title 1") f |> Seq.length)
        Assert.Equal(1, Folder.filterDocsBySlug (Slug.ofString "Title 2") f |> Seq.length)

        Assert.Equal(0, Folder.filterDocsByInternPath (Approx(RelPath "doc1")) f |> Seq.length)
        Assert.Equal(1, Folder.filterDocsByInternPath (Approx(RelPath "doc2")) f |> Seq.length)

module DocTest =
    [<Fact>]
    let applyLspChange () =
        let dummyPath =
            (dummyRootPath [ "dummy.md" ]) |> mkDocId (mkFolderId dummyRoot)

        let empty = Doc.mk dummyPath None (Text.mkText "")

        let insertChange =
            { TextDocument = { Uri = RootedRelPath.toSystem dummyPath.data; Version = Some 1 }
              ContentChanges =
                [| { Range = Some(Range.Mk(0, 0, 0, 0))
                     RangeLength = Some 0
                     Text = "[" } |] }

        let updated = Doc.applyLspChange insertChange empty
        Assert.Equal("[", (Doc.text updated).content)

    [<Fact(Skip = "Uri and # don't mix well")>]
    let pathFromRoot_SpecialChars () =
        let doc = FakeDoc.Mk(path = "blah#blah.md", contentLines = [||])
        Assert.Equal("blah#blah.md", Doc.pathFromRoot doc |> RelPath.toSystem)

    [<Fact>]
    let fromLsp_singleFile () =
        let par =
            { Uri = "file:///a/b/doc.md"
              LanguageId = "md"
              Version = 1
              Text = "text" }

        let singletonRoot = UriWith.mkRoot par.Uri
        let doc = Doc.fromLsp singletonRoot par
        Assert.Equal("file:///a/b/doc.md", (Doc.uri doc).ToString())
        Assert.Equal("AbsPath \"/a/b/doc.md\"", (Doc.path doc).ToString())

module WorkspaceTest =
    [<Fact>]
    let folderFind_singleFile () =
        let f1 =
            let d = FakeDoc.Mk(content = "", path = "a/b/d1.md", root = "a")
            Folder.singleFile d None

        let f2 =
            let d = FakeDoc.Mk(content = "", path = "a/b/d2.md", root = "a")
            Folder.singleFile d None

        let ws = Workspace.ofFolders None [ f1; f2 ]

        Assert.Equal(
            Some f1,
            Workspace.tryFindFolderEnclosing
                (AbsPath.ofUri (dummyRootPath [ "a"; "b"; "d1.md" ] |> pathToUri))
                ws
        )

        Assert.Equal(
            None,
            Workspace.tryFindFolderEnclosing
                (AbsPath.ofUri (dummyRootPath [ "a"; "d1.md" ] |> pathToUri))
                ws
        )

    [<Fact>]
    let folderAdded_evictSingleFile () =
        let d1 = FakeDoc.Mk(content = "", path = "a/b/d1.md", root = "a/b")
        let f1 = Folder.singleFile d1 None
        let d2 = FakeDoc.Mk(content = "", path = "c/d2.md", root = "c")
        let f2 = Folder.singleFile d2 None

        let ws = Workspace.ofFolders None [ f1; f2 ]

        let f0Path = dummyRootPath [ "a" ] |> mkFolderId
        let f0 = Folder.multiFile "f0" f0Path Seq.empty None

        let updWs = Workspace.withFolder f0 ws

        let ids = Workspace.folders updWs |> Seq.map Folder.id |> List.ofSeq

        Assert.Equal<FolderId>([ Folder.id f0; Folder.id f2 ], ids)

    [<Fact>]
    let folderConfig_noUserConfig () =
        let fConfig = Some { Config.Default with caTocEnable = Some false }
        let fPath = dummyRootPath [ "a" ] |> mkFolderId

        // Multi-file
        let f = (Folder.multiFile "f0" fPath Seq.empty fConfig)
        let ws = Workspace.ofFolders None [ f ]
        let f = (Workspace.folders ws) |> Seq.head
        let updatedConfig = Folder.config f

        Assert.Equal(fConfig, updatedConfig)

        // Single-file
        let f = (Folder.singleFile (FakeDoc.Mk("")) fConfig)
        let ws = Workspace.ofFolders None [ f ]
        let f = (Workspace.folders ws) |> Seq.head
        let updatedConfig = Folder.config f
        Assert.Equal(fConfig, updatedConfig)

    [<Fact>]
    let folderConfig_userConfig () =
        let wsConfig = Some { Config.Default with caTocEnable = Some false }
        let fPath = dummyRootPath [ "a" ] |> mkFolderId

        // Multi-file
        let f = (Folder.multiFile "f0" fPath Seq.empty None)
        let ws = Workspace.ofFolders wsConfig [ f ]
        let f = (Workspace.folders ws) |> Seq.head
        let updatedConfig = Folder.config f

        Assert.Equal(wsConfig, updatedConfig)

        // Single-file
        let f = (Folder.singleFile (FakeDoc.Mk("")) None)
        let ws = Workspace.ofFolders wsConfig [ f ]
        let f = (Workspace.folders ws) |> Seq.head
        let updatedConfig = Folder.config f
        Assert.Equal(wsConfig, updatedConfig)

    [<Fact>]
    let folderConfig_userConfig_folderAdd () =
        let wsConfig = Some { Config.Default with caTocEnable = Some false }
        let fPath = dummyRootPath [ "a" ] |> mkFolderId

        // Multi-file
        let ws = Workspace.ofFolders wsConfig []
        let f = (Folder.multiFile "f0" fPath Seq.empty None)
        let ws = Workspace.withFolder f ws
        let f = (Workspace.folders ws) |> Seq.head
        let updatedConfig = Folder.config f

        Assert.Equal(wsConfig, updatedConfig)

        // Single-file
        let ws = Workspace.ofFolders wsConfig []
        let f = (Folder.singleFile (FakeDoc.Mk("")) None)
        let ws = Workspace.withFolder f ws
        let f = (Workspace.folders ws) |> Seq.head
        let updatedConfig = Folder.config f
        Assert.Equal(wsConfig, updatedConfig)
