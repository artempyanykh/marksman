module Marsman.ServerTests

open Newtonsoft.Json.Linq
open Xunit

open Marksman
open Marksman.Paths
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

module StateTests =
    [<Fact>]
    let folderFind_singleFile () =
        let d1 = FakeDoc.Mk(content = "", path = "a/b/d1.md", root = "a")
        let f1 = Folder.singleFile d1 None

        let d2 = FakeDoc.Mk(content = "", path = "a/b/d2.md", root = "a")
        let f2 = Folder.singleFile d2 None

        let ws = Workspace.ofFolders None [ f1; f2 ]
        let state = State.mk ClientDescription.empty ws

        Assert.Equal(
            Some(f1, d1),
            State.tryFindFolderAndDoc (UriWith.mkAbs "file:///a/b/d1.md") state
        )
