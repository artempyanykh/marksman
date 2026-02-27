module Marksman.StateTests

open Newtonsoft.Json.Linq
open Xunit

open Ionide.LanguageServerProtocol.Types

open Marksman
open Marksman.Paths
open Marksman.Doc
open Marksman.Folder
open Marksman.Workspace
open Marksman.State

open Marksman.Helpers

module InitOptionTests =
    [<Fact>]
    let extractEmpty () =
        let json = JToken.Parse("{}")
        Assert.Equal(InitOptions.empty, InitOptions.ofJson json)

        let json = JToken.Parse("[]")
        Assert.Equal(InitOptions.empty, InitOptions.ofJson json)

    [<Fact>]
    let extractCorrect () =
        let json = JToken.Parse("""{"preferredTextSyncKind": 1}""")
        Assert.Equal({ preferredTextSyncKind = Some Config.Full }, InitOptions.ofJson json)

        let json = JToken.Parse("""{"preferredTextSyncKind": 2}""")
        Assert.Equal({ preferredTextSyncKind = Some Config.Incremental }, InitOptions.ofJson json)

    [<Fact>]
    let extractMalformed () =
        let json = JToken.Parse("""{"preferredTextSyncKind": 42}""")
        Assert.Equal({ preferredTextSyncKind = None }, InitOptions.ofJson json)

        let json = JToken.Parse("""{"preferredTextSyncKind": "full"}""")
        Assert.Equal({ preferredTextSyncKind = None }, InitOptions.ofJson json)

module ClientDescriptionTests =
    // Helper: build a ClientDescription with specific workspace capabilities
    let private mkClientWithWatchedFiles dynamicReg =
        let wsCaps = {
            ApplyEdit = None
            WorkspaceEdit = None
            DidChangeConfiguration = None
            DidChangeWatchedFiles =
                Some {
                    DynamicRegistration = Some dynamicReg
                    RelativePatternSupport = None
                }
            Symbol = None
            ExecuteCommand = None
            WorkspaceFolders = None
            Configuration = None
            SemanticTokens = None
            InlayHint = None
            InlineValue = None
            CodeLens = None
            FileOperations = None
            Diagnostics = None
        }

        let caps = { ClientDescription.empty.caps with Workspace = Some wsCaps }
        { ClientDescription.empty with caps = caps }

    [<Fact>]
    let supportsDidChangeWatchedFiles_true () =
        let client = mkClientWithWatchedFiles true
        Assert.True(client.SupportsDidChangeWatchedFiles)

    [<Fact>]
    let supportsDidChangeWatchedFiles_false () =
        let client = mkClientWithWatchedFiles false
        Assert.False(client.SupportsDidChangeWatchedFiles)

    [<Fact>]
    let supportsDidChangeWatchedFiles_absent () =
        Assert.False(ClientDescription.empty.SupportsDidChangeWatchedFiles)


// Tests for the state mutations that WorkspaceDidChangeWatchedFiles relies on.
// The handler does: find folder enclosing URI → reload/remove doc → update state.
// We test this pipeline directly via State helpers rather than through the server.
module WatchedFilesStateTests =
    // Use non-overlapping roots: primary at /primary, extra at /extra
    let private primaryRootUri = pathToUri (dummyRootPath [ "primary" ])
    let private extraRootUri = pathToUri (dummyRootPath [ "extra" ])

    let private mkPrimaryDoc path content =
        FakeDoc.Mk(content, path = "primary/" + path, root = "primary")

    let private mkExtraDoc path content =
        FakeDoc.Mk(content, path = "extra/" + path, root = "extra")

    let private mkPrimaryFolder docs =
        Folder.multiFile "primary" (UriWith.mkRoot primaryRootUri) docs None

    let private mkExtraFolder docs =
        Folder.multiFile "extra" (UriWith.mkRoot extraRootUri) docs None

    [<Fact>]
    // Simulates WorkspaceDidChangeWatchedFiles (Changed): update a doc already in the
    // extra folder — the new content should be visible through State lookups.
    let watchedFileChange_updatesDocInExtraFolder () =
        let extraDoc = mkExtraDoc "note.md" "# Original Title"
        let extraFolder = mkExtraFolder [ extraDoc ]

        let primaryDoc = mkPrimaryDoc "primary.md" "[[original-title]]"
        let primaryFolder = mkPrimaryFolder [ primaryDoc ]
        let ws = Workspace.ofFolders None [ primaryFolder; extraFolder ]
        let state = State.mk ClientDescription.empty ws

        let noteUri =
            UriWith.mkAbs (pathToUri (dummyRootPath [ "extra"; "note.md" ]))

        let origDoc = State.tryFindDoc noteUri state
        Assert.True(Option.isSome origDoc, "original doc should be found in state")
        Assert.Equal("# Original Title", (Option.get origDoc |> Doc.text).content)

        // Simulate the handler: reload the doc with updated content
        let updatedDoc = mkExtraDoc "note.md" "# Updated Title"
        let updatedFolder = Folder.withDoc updatedDoc extraFolder
        let newState = State.updateFolder updatedFolder state

        let reloadedDoc = State.tryFindDoc noteUri newState
        Assert.True(Option.isSome reloadedDoc, "updated doc should be found in new state")
        Assert.Equal("# Updated Title", (Option.get reloadedDoc |> Doc.text).content)

    [<Fact>]
    // Simulates WorkspaceDidChangeWatchedFiles (Deleted): remove a doc from the extra
    // folder — it should no longer be found via State lookups.
    let watchedFileDelete_removesDocFromExtraFolder () =
        let extraDoc = mkExtraDoc "note.md" "# A Note"
        let extraFolder = mkExtraFolder [ extraDoc ]

        let primaryDoc = mkPrimaryDoc "primary.md" ""
        let primaryFolder = mkPrimaryFolder [ primaryDoc ]
        let ws = Workspace.ofFolders None [ primaryFolder; extraFolder ]
        let state = State.mk ClientDescription.empty ws

        let noteUri =
            UriWith.mkAbs (pathToUri (dummyRootPath [ "extra"; "note.md" ]))

        Assert.True(
            Option.isSome (State.tryFindDoc noteUri state),
            "doc should exist before delete"
        )

        // Simulate the handler: remove the doc
        let newFolder =
            Folder.withoutDoc (Doc.id extraDoc) extraFolder |> Option.get

        let newState = State.updateFolder newFolder state

        Assert.True(
            Option.isNone (State.tryFindDoc noteUri newState),
            "doc should be gone after delete"
        )


module StateTests =
    [<Fact>]
    let folderFind_singleFile () =
        let d1 = FakeDoc.Mk(content = "", path = "a/b/d1.md", root = "a")
        let f1 = Folder.singleFile d1 None

        let d2 = FakeDoc.Mk(content = "", path = "a/b/d2.md", root = "a")
        let f2 = Folder.singleFile d2 None

        let ws = Workspace.ofFolders None [ f1; f2 ]
        let state = State.mk ClientDescription.empty ws

        let d1Uri = dummyRootPath [ "a"; "b"; "d1.md" ] |> pathToUri
        Assert.Equal(Some(f1, d1), State.tryFindFolderAndDoc (UriWith.mkAbs d1Uri) state)
