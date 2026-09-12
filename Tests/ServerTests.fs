module Marksman.ServerTests

open Xunit
open Ionide.LanguageServerProtocol.Types

open Marksman.Config
open Marksman.Server
open Marksman.State
open Marksman.Doc
open Marksman.Folder
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
        let clientDesc = {
            ClientDescription.empty with
                opts = { preferredTextSyncKind = Some Incremental }
        }

        let ws = Workspace.ofFolders None []
        Assert.Equal(("clientOption", Incremental), ServerUtil.calcTextSync None ws clientDesc)

    [<Fact>]
    let textSync_NoConfigNonEmptyWS_PreferIncr () =
        let clientDesc = {
            ClientDescription.empty with
                opts = { preferredTextSyncKind = Some Incremental }
        }

        let folder =
            Folder.multiFile "test" (dummyRootPath [ "test" ] |> mkFolderId) Seq.empty None

        let ws = Workspace.ofFolders None [ folder ]
        Assert.Equal(("clientOption", Incremental), ServerUtil.calcTextSync None ws clientDesc)

    [<Fact>]
    let textSync_NonEmptyWS_PreferIncrButConfigTakesPrecedence () =
        let clientDesc = {
            ClientDescription.empty with
                opts = { preferredTextSyncKind = Some Incremental }
        }

        let folder =
            Folder.multiFile
                "test"
                (dummyRootPath [ "test" ] |> mkFolderId)
                Seq.empty
                (Some { Config.Empty with coreTextSync = Some Full })

        let ws = Workspace.ofFolders None [ folder ]
        Assert.Equal(("workspaceConfig", Full), ServerUtil.calcTextSync None ws clientDesc)

module DiagnosticPublicationTests =
    let private doc path lines = FakeDoc.Mk(path = path, contentLines = Array.ofList lines)

    let private state folder =
        Workspace.ofFolders None [ folder ]
        |> State.mk ClientDescription.empty

    let private publications previous current =
        calcDiagnosticsUpdate previous current |> Array.ofSeq

    let private onlyPublication (doc: Doc) (updates: PublishDiagnosticsParams[]) =
        let update = Assert.Single updates
        Assert.Equal(Doc.uri doc, update.Uri)
        Assert.Equal(None, update.Version)
        update.Diagnostics

    [<Fact>]
    let initialCalculationPublishesOnlyDocumentsWithDiagnostics () =
        let source = doc "source.md" [ "[[missing]]"; "##\u00a0Not a heading" ]
        let clean = doc "clean.md" [ "Some prose." ]

        let diagnostics =
            state (FakeFolder.Mk [ source; clean ]) |> publications None

        let sourceDiagnostics = onlyPublication source diagnostics

        Assert.Equal(2, sourceDiagnostics.Length)
        Assert.Equal("Link to non-existent document 'missing'", sourceDiagnostics[0].Message)

        Assert.StartsWith(
            "Non-breaking whitespace used instead of regular whitespace",
            sourceDiagnostics[1].Message
        )

        Assert.Equal(Some DiagnosticSeverity.Error, sourceDiagnostics[0].Severity)
        Assert.Equal(Some DiagnosticSeverity.Warning, sourceDiagnostics[1].Severity)
        Assert.Equal(0, sourceDiagnostics[0].Range.Start.Line)
        Assert.Equal(1, sourceDiagnostics[1].Range.Start.Line)

    [<Fact>]
    let unchangedDiagnosticsProduceNoPublication () =
        let source = doc "source.md" [ "[[missing]]" ]
        let before = state (FakeFolder.Mk [ source ])
        let edited = doc "source.md" [ "[[missing]]"; "More prose." ]
        let after = state (FakeFolder.Mk [ edited ])

        Assert.Empty(publications (Some before) after)

    [<Fact>]
    let otherDocumentsCanResolveOrMakeALinkAmbiguous () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let first = doc "first.md" [ "# Target"; "## Section" ]
        let second = doc "second.md" [ "# Target"; "## Section" ]
        let missing = FakeFolder.Mk [ source ]
        let resolved = Folder.withDoc first missing
        let ambiguous = Folder.withDoc second resolved

        let broken = state missing |> publications None |> onlyPublication source
        let brokenLink = Assert.Single broken

        Assert.Equal(
            "Link to non-existent heading 'section' in document 'Target'",
            brokenLink.Message
        )

        let cleared =
            publications (Some(state missing)) (state resolved)
            |> onlyPublication source

        Assert.Empty(cleared)

        let reported =
            publications (Some(state resolved)) (state ambiguous)
            |> onlyPublication source

        let diagnostic = Assert.Single reported
        Assert.Equal("Ambiguous link to heading 'section' in document 'Target'", diagnostic.Message)
        Assert.Equal(2, diagnostic.RelatedInformation.Value.Length)

    [<Fact>]
    let movingATargetHeadingUpdatesAmbiguousLinkRelatedLocation () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let first = doc "first.md" [ "# Target"; "## Section" ]
        let second = doc "second.md" [ "# Target"; "## Section" ]
        let before = FakeFolder.Mk [ source; first; second ]
        let moved = doc "first.md" [ "# Target"; ""; "## Section" ]
        let after = Folder.withDoc moved before

        let prior = state before |> publications None |> onlyPublication source

        let latest =
            publications (Some(state before)) (state after)
            |> onlyPublication source

        let priorDiagnostic = Assert.Single prior
        let latestDiagnostic = Assert.Single latest

        let rangeInFirst (diagnostic: Diagnostic) =
            diagnostic.RelatedInformation.Value
            |> Array.find (fun related -> related.Location.Uri = Doc.uri first)
            |> fun related -> related.Location.Range

        Assert.Equal(1, (rangeInFirst priorDiagnostic).Start.Line)
        Assert.Equal(2, (rangeInFirst latestDiagnostic).Start.Line)

        Assert.Equal(
            "Ambiguous link to heading 'section' in document 'Target'",
            latestDiagnostic.Message
        )

    [<Fact>]
    let removingOneOfTwoTargetsClearsAmbiguity () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let first = doc "first.md" [ "# Target"; "## Section" ]
        let second = doc "second.md" [ "# Target"; "## Section" ]
        let before = FakeFolder.Mk [ source; first; second ]
        let after = Folder.withoutDoc second.Id before |> Option.get

        let cleared =
            publications (Some(state before)) (state after)
            |> onlyPublication source

        Assert.Empty(cleared)

    [<Fact>]
    let removingADocumentClearsItsPublishedDiagnostics () =
        let source = doc "source.md" [ "[[missing]]" ]
        let remaining = doc "remaining.md" [ "Some prose." ]
        let before = FakeFolder.Mk [ source; remaining ]
        let after = Folder.withoutDoc source.Id before |> Option.get

        let cleared =
            publications (Some(state before)) (state after)
            |> onlyPublication source

        Assert.Empty(cleared)

    [<Fact>]
    let removingAFolderClearsDiagnosticsForItsDocuments () =
        let source = doc "source.md" [ "[[missing]]" ]
        let before = state (FakeFolder.Mk [ source ])
        let after = Workspace.ofFolders None [] |> State.mk ClientDescription.empty

        let cleared = publications (Some before) after |> onlyPublication source

        Assert.Empty(cleared)

    [<Fact>]
    let reopeningADocumentResendsUnchangedDiagnostics () =
        let source = doc "source.md" [ "[[missing]]" ]
        let before = FakeFolder.Mk [ source ]

        let reopened =
            Doc.mk ParserSettings.Default source.Id (Some 1) (Doc.text source)

        let after = Folder.withDoc reopened before

        let reported =
            publications (Some(state before)) (state after)
            |> onlyPublication source

        let brokenLink = Assert.Single reported
        Assert.Equal("Link to non-existent document 'missing'", brokenLink.Message)

    [<Fact>]
    let reopeningACleanDocumentDoesNotPublishAnEmptyUpdate () =
        let source = doc "source.md" [ "Some prose." ]
        let before = FakeFolder.Mk [ source ]
        let reopened = Doc.mk ParserSettings.Default source.Id (Some 1) (Doc.text source)
        let after = Folder.withDoc reopened before

        Assert.Empty(publications (Some(state before)) (state after))

    [<Fact>]
    let publicationComparesLastPublishedStateWithLatestDebouncedState () =
        let source = doc "source.md" [ "[[Target]]" ]
        let target = doc "target.md" [ "# Target" ]
        let initial = FakeFolder.Mk [ source ]
        let intermediate = Folder.withDoc target initial
        let latest = Folder.withoutDoc target.Id intermediate |> Option.get

        Assert.Empty(publications (Some(state initial)) (state latest))
