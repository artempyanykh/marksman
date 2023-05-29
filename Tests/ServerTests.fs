module Marksman.ServerTests

open Xunit

open Marksman.Config
open Marksman.Paths
open Marksman.Server
open Marksman.State
open Marksman.Workspace

open Marksman.Helpers

module ServerUtilTests =
    [<Fact>]
    let textSync_UserConfigEmptyWS () =
        let c = { Config.Default with coreTextSync = Some Incremental }
        let clientDesc = ClientDescription.empty
        let ws = Workspace.ofFolders (Some c) []
        Assert.Equal(("userConfig", Incremental), ServerUtil.calcTextSync (Some c) ws clientDesc)

    [<Fact>]
    let textSync_NoConfigEmptyWS () =
        let clientDesc = ClientDescription.empty
        let ws = Workspace.ofFolders None []
        Assert.Equal(("default", Full), ServerUtil.calcTextSync None ws clientDesc)

    [<Fact>]
    let textSync_NoConfigEmptyWS_PreferIncr () =
        let clientDesc =
            { ClientDescription.empty with opts = { preferredTextSyncKind = Some Incremental } }

        let ws = Workspace.ofFolders None []
        Assert.Equal(("clientOption", Incremental), ServerUtil.calcTextSync None ws clientDesc)

    [<Fact>]
    let textSync_NoConfigNonEmptyWS_PreferIncr () =
        let clientDesc =
            { ClientDescription.empty with opts = { preferredTextSyncKind = Some Incremental } }

        let folder =
            Folder.multiFile "test" (dummyRootPath [ "test" ] |> mkFolderId) Seq.empty None

        let ws = Workspace.ofFolders None [ folder ]
        Assert.Equal(("clientOption", Incremental), ServerUtil.calcTextSync None ws clientDesc)

    [<Fact>]
    let textSync_NonEmptyWS_PreferIncrButConfigTakesPrecedence () =
        let clientDesc =
            { ClientDescription.empty with opts = { preferredTextSyncKind = Some Incremental } }

        let folder =
            Folder.multiFile
                "test"
                (dummyRootPath [ "test" ] |> mkFolderId)
                Seq.empty
                (Some { Config.Empty with coreTextSync = Some Full })

        let ws = Workspace.ofFolders None [ folder ]
        Assert.Equal(("workspaceConfig", Full), ServerUtil.calcTextSync None ws clientDesc)
