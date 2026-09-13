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
    let docsDifferenceAcrossInterleavedPaths () =
        let a = FakeDoc.Mk(content = "A", path = "a.md")
        let c = FakeDoc.Mk(content = "C", path = "c.md")
        let changedC = FakeDoc.Mk(content = "Updated C", path = "c.md")
        let e = FakeDoc.Mk(content = "E", path = "e.md")
        let g = FakeDoc.Mk(content = "G", path = "g.md")
        let b = FakeDoc.Mk(content = "B", path = "b.md")
        let d = FakeDoc.Mk(content = "D", path = "d.md")
        let z = FakeDoc.Mk(content = "Z", path = "z.md")
        let before = FakeFolder.Mk [ a; c; e; g ]
        let after = FakeFolder.Mk [ a; b; changedC; d; g; z ]
        let difference = Folder.docsDifference before after

        Assert.Equal<Set<DocId>>(Set.ofList [ b.Id; d.Id; z.Id ], difference.added)
        Assert.Equal<Set<DocId>>(Set.singleton e.Id, difference.removed)
        Assert.Equal<Set<DocId>>(Set.singleton c.Id, difference.changed)
        Assert.Empty(difference.reopened)

    [<Fact>]
    let docsDifferenceReportsReopeningWithoutAContentChange () =
        let closed = FakeDoc.Mk(content = "A", path = "a.md")
        let reopened = Doc.mk ParserSettings.Default closed.Id (Some 1) (Doc.text closed)
        let before = FakeFolder.Mk [ closed ]
        let after = Folder.withDoc reopened before
        let difference = Folder.docsDifference before after

        Assert.Equal<Set<DocId>>(Set.singleton closed.Id, difference.reopened)
        Assert.Empty(difference.changed)

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

    [<Fact>]
    let collidingCanonicalPathsKeepBothDocuments () =
        let markdown = FakeDoc.Mk(content = "# Markdown", path = "notes.md")
        let longExtension = FakeDoc.Mk(content = "# Long extension", path = "notes.markdown")
        let config = {
            Config.Default with
                coreIncrementalReferences = Some true
                coreParanoid = Some true
        }

        let folder = FakeFolder.Mk([ markdown ], config = config) |> Folder.withDoc longExtension

        Assert.Equal(2, Folder.docCount folder)
        Assert.Equal(Some markdown, Folder.tryFindDocByPath (Doc.path markdown) folder)
        Assert.Equal(Some longExtension, Folder.tryFindDocByPath (Doc.path longExtension) folder)
        Assert.Equal(None, Folder.tryFindDocByRelPath (RelPath "notes") folder)
        Assert.Equal(Some markdown, Folder.tryFindDocByRelPath (RelPath "notes.md") folder)
        Assert.Equal(2, Folder.filterDocsByInternPath (Approx(RelPath "notes")) folder |> Seq.length)
        Assert.Equal(
            2,
            Folder.filterDocsByName (InternName.mkUnchecked markdown.Id "notes") folder
            |> Seq.length
        )

        let withoutMarkdown = Folder.withoutDoc markdown.Id folder |> Option.get
        Assert.Equal(1, Folder.docCount withoutMarkdown)
        Assert.Equal(Some longExtension, Folder.tryFindDocByPath (Doc.path longExtension) withoutMarkdown)
        Assert.Equal(1, Folder.filterDocsByInternPath (Approx(RelPath "notes")) withoutMarkdown |> Seq.length)

        let updated = Folder.withDoc (FakeDoc.Mk(content = "# Updated", path = "notes.markdown")) folder
        Assert.Equal(2, Folder.docCount updated)
        Assert.Equal(Some markdown, Folder.tryFindDocByPath (Doc.path markdown) updated)

    [<Fact>]
    let extensionChangeKeepsDocumentsThatNowShareACanonicalPath () =
        let markdown = FakeDoc.Mk(content = "# Markdown", path = "notes.md")
        let longExtension = FakeDoc.Mk(content = "# Long extension", path = "notes.markdown")
        let initialConfig = { Config.Default with coreMarkdownFileExtensions = Some [| "md" |] }
        let folder = FakeFolder.Mk([ markdown; longExtension ], config = initialConfig)
        let updatedConfig = { initialConfig with coreMarkdownFileExtensions = Some [| "md"; "markdown" |] }
        let updated = Folder.withConfig (Some updatedConfig) folder

        Assert.Equal(2, Folder.docCount updated)
        Assert.Equal(2, Folder.filterDocsByInternPath (Approx(RelPath "notes")) updated |> Seq.length)
        Assert.Equal(Some markdown, Folder.tryFindDocByPath (Doc.path markdown) updated)
        Assert.Equal(Some longExtension, Folder.tryFindDocByPath (Doc.path longExtension) updated)


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
