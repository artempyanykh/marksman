module Marksman.WorkspaceTest

open Ionide.LanguageServerProtocol.Types

open Xunit

open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Folder
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

        // Adding a new doc; both by slug and by path should reflect
        let d3 = FakeDoc.Mk(content = "# Title 3", path = "doc3.md")
        let f = Folder.withDoc d3 f
        Assert.Equal(1, Folder.filterDocsByInternPath (Approx(RelPath "doc3")) f |> Seq.length)
        Assert.Equal(0, Folder.filterDocsByInternPath (Approx(RelPath "doc4")) f |> Seq.length)

        Assert.Equal(1, Folder.filterDocsBySlug (Slug.ofString "Title 3") f |> Seq.length)
        Assert.Equal(0, Folder.filterDocsBySlug (Slug.ofString "Title 4") f |> Seq.length)


module DocTest =
    [<Fact>]
    let applyLspChange () =
        let dummyPath =
            (dummyRootPath [ "dummy.md" ]) |> mkDocId (mkFolderId dummyRoot)

        let empty = Doc.mk ParserSettings.Default dummyPath None (Text.mkText "")

        let insertChange = {
            TextDocument = { Uri = RootedRelPath.toSystem dummyPath.Path; Version = 1 }
            ContentChanges = [|
                {
                    Range = Some(Range.Mk(0, 0, 0, 0))
                    RangeLength = Some 0
                    Text = "["
                }
            |]
        }

        let updated = Doc.applyLspChange ParserSettings.Default insertChange empty

        Assert.Equal("[", (Doc.text updated).content)

    [<Fact(Skip = "Uri and # don't mix well")>]
    let pathFromRoot_SpecialChars () =
        let doc = FakeDoc.Mk(path = "blah#blah.md", contentLines = [||])
        Assert.Equal("blah#blah.md", Doc.pathFromRoot doc |> RelPath.toSystem)

    [<Fact>]
    let fromLsp_singleFile () =
        let par = {
            Uri = "file:///a/b/doc.md"
            LanguageId = "md"
            Version = 1
            Text = "text"
        }

        let singletonRoot = UriWith.mkRoot par.Uri
        let doc = Doc.fromLsp ParserSettings.Default singletonRoot par
        Assert.Equal("file:///a/b/doc.md", (Doc.uri doc).ToString())
        Assert.Equal("AbsPath \"/a/b/doc.md\"", (Doc.path doc).ToString())

module ExtraFolderTest =
    // Helper to build a workspace with manually designated extra folders.
    // We achieve this by using Workspace.withFolder for extra folders and
    // directly calling the workspace helpers.
    let mkPrimaryAndExtraFolders () =
        let primaryDoc =
            FakeDoc.Mk(content = "[[extra-doc]]", path = "primary.md")

        let primaryFolder = FakeFolder.Mk([ primaryDoc ])

        let extraDoc = FakeDoc.Mk(content = "# Extra Doc", path = "extra-doc.md")
        let extraFolder = FakeFolder.Mk([ extraDoc ])

        primaryFolder, extraFolder, primaryDoc, extraDoc

    [<Fact>]
    let primaryFolders_withNoExtra () =
        let d = FakeDoc.Mk(content = "", path = "doc.md")
        let f = FakeFolder.Mk([ d ])
        let ws = Workspace.ofFolders None [ f ]

        // With no extra folders configured, all folders are primary
        let primaries = Workspace.primaryFolders ws |> List.ofSeq
        Assert.Equal(1, primaries.Length)
        Assert.Equal(Folder.id f, Folder.id primaries[0])

    [<Fact>]
    let isExtraFolder_noExtra () =
        let d = FakeDoc.Mk(content = "", path = "doc.md")
        let f = FakeFolder.Mk([ d ])
        let ws = Workspace.ofFolders None [ f ]

        // Nothing is marked as extra
        Assert.False(Workspace.isExtraFolder ws (Folder.id f))

    [<Fact>]
    let extraFoldersFor_noExtra () =
        let d = FakeDoc.Mk(content = "", path = "doc.md")
        let f = FakeFolder.Mk([ d ])
        let ws = Workspace.ofFolders None [ f ]

        // No extra folders configured
        let extras = Workspace.extraFoldersFor f ws |> List.ofSeq
        Assert.Equal(0, extras.Length)

    // Regression test: when an extra folder declares the primary folder as its own
    // extra_folder (mutual reference), rename in the primary must propagate to the
    // extra folder's docs. primaryFoldersReferencing must search all workspace folders,
    // not just primary folders.
    [<Fact>]
    let primaryFoldersReferencing_mutualExtraFolder () =
        // primary folder at dummyRoot
        let primaryDoc =
            FakeDoc.Mk(content = "# Meeting\nSome content.", path = "meeting.md")

        let primaryFolder = FakeFolder.Mk([ primaryDoc ])

        // people folder at dummyRoot/people — declares primary as its extra folder
        let peopleRootUri = pathToUri (dummyRootPath [ "people" ])

        let peopleDoc =
            FakeDoc.Mk(content = "# Jon Doe\n[[meeting]].", path = "jon-doe.md", root = "people")

        let peopleFolder =
            Folder.multiFile "people" (UriWith.mkRoot peopleRootUri) [ peopleDoc ] None
            |> Folder.withExtraFolderRoots [| AbsPath.ofSystem dummyRoot |]

        // Build workspace: primary is the declared workspace root; people is injected
        let ws =
            Workspace.ofFolders None [ primaryFolder ]
            |> Workspace.withFolder peopleFolder

        // people declares primary as its extra folder, so primary should appear as a
        // folder that references people (i.e. people is a "referencing folder" for primary)
        let referencingPrimary =
            Workspace.primaryFoldersReferencing (Folder.id primaryFolder) ws
            |> List.ofSeq

        Assert.Equal(1, referencingPrimary.Length)
        Assert.Equal(Folder.id peopleFolder, Folder.id referencingPrimary[0])

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
