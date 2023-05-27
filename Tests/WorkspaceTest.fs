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

module DocTest =
    [<Fact>]
    let applyLspChange () =
        let dummyPath = (dummyRootPath [ "dummy.md" ]) |> mkDocId (mkFolderId dummyRoot)

        let empty =
            Doc.mk dummyPath None (Text.mkText "")

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

module WorkspaceTest =
    [<Fact>]
    let folderAdded_evictSingleFile () =
        let d1 = FakeDoc.Mk(content = "", path = "a/b/d1.md", root = "a/b")
        let f1 = Folder.singleFile d1 None
        let d2 = FakeDoc.Mk(content = "", path = "c/d2.md", root = "c")
        let f2 = Folder.singleFile d2 None

        let ws = Workspace.ofFolders None [ f1; f2 ]

        let f0Path = dummyRootPath [ "a" ] |> mkFolderId
        let f0 = Folder.multiFile "f0" f0Path Map.empty None

        let updWs = Workspace.withFolder f0 ws

        let ids = Workspace.folders updWs |> Seq.map Folder.id |> List.ofSeq

        Assert.Equal<FolderId>([ Folder.id f0; Folder.id f2 ], ids)

    [<Fact>]
    let folderConfig_noUserConfig () =
        let fConfig = Some { Config.Default with caTocEnable = Some false }
        let fPath = dummyRootPath [ "a" ] |> mkFolderId

        // Multi-file
        let f = (Folder.multiFile "f0" fPath Map.empty fConfig)
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
        let f = (Folder.multiFile "f0" fPath Map.empty None)
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
        let f = (Folder.multiFile "f0" fPath Map.empty None)
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
