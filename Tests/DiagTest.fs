module Marksman.DiagTest

open Xunit

open Marksman.Diag
open Marksman.Config
open Marksman.Helpers
open Marksman.Index
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Folder
open Marksman.Refs
open Marksman.Workspace

let diagToHuman (folder: Folder) : list<string * string> =
    let diagnostics, _ =
        WorkspaceDiag.calculate None (Workspace.ofFolders None [ folder ])

    seq {
        for KeyValue(id, entries) in diagnostics[Folder.id folder] do
            for entry in entries do
                yield id.Path |> RootedRelPath.relPathForced |> RelPath.toSystem, entry.Message
    }
    |> List.ofSeq

[<Fact>]
let documentIndex_1 () =
    let doc = FakeDoc.Mk "# T1\n# T2"

    let titles =
        Doc.index >> Index.titles <| doc
        |> Array.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)

[<Fact>]
let nonBreakingWhitespace () =
    let nbsp = "\u00a0"
    let doc = FakeDoc.Mk $"# T1\n##{nbsp}T2"

    match (checkNonBreakingWhitespace doc) with
    | [ NonBreakableWhitespace range ] ->
        Assert.Equal(1, range.Start.Line)
        Assert.Equal(1, range.End.Line)

        Assert.Equal(2, range.Start.Character)
        Assert.Equal(3, range.End.Character)
    | _ -> failwith "Expected NonBreakingWhitespace diagnostic"

[<Fact>]
let noDiagOnShortcutLinks () =
    let doc = FakeDoc.Mk([| "# H1"; "## H2"; "[shortcut]"; "[[#h42]]" |])
    let folder = FakeFolder.Mk([ doc ])
    let diag = diagToHuman folder

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent heading 'h42'" ], diag)

[<Fact>]
let noDiagOnRealUrls () =
    let doc =
        FakeDoc.Mk([| "# H1"; "## H2"; "[](www.bad.md)"; "[](https://www.good.md)" |])

    let folder = FakeFolder.Mk([ doc ])
    let diag = diagToHuman folder

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'www.bad.md'" ], diag)

[<Fact>]
let markdownExtensionWithoutANameDoesNotResolveToEveryDocument () =
    let source =
        FakeDoc.Mk(path = "source.md", contentLines = [| "[x](.md)" |])

    let first = FakeDoc.Mk(path = "first.md", contentLines = [| "# First" |])

    let second =
        FakeDoc.Mk(path = "second.md", contentLines = [| "# Second" |])

    let folder = FakeFolder.Mk [ source; first; second ]

    match Assert.Single(checkDoc folder source) with
    | BrokenLink _ -> ()
    | diagnostic -> failwith $"Expected a broken link, got {diagnostic}"

    Assert.Empty(Folder.filterDocsByInternPath (Approx(RelPath ".md")) folder)

[<Fact>]
let noDiagOnNonMarkdownFiles () =
    let doc =
        FakeDoc.Mk(
            [|
                "# H1"
                "## H2"
                "[](bad.md)"
                "[](another%20bad.md)"
                "[](good/folder)"
            |]
        )

    let folder = FakeFolder.Mk([ doc ])
    let diag = diagToHuman folder

    Assert.Equal<string * string>(
        [
            "fake.md", "Link to non-existent document 'bad.md'"
            "fake.md", "Link to non-existent document 'another bad.md'"
        ],
        diag
    )


[<Fact>]
let crossFileDiagOnBrokenWikiLinks () =
    let doc = FakeDoc.Mk([| "[[bad]]" |])

    let folder = FakeFolder.Mk([ doc ])
    let diag = diagToHuman folder

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'bad'" ], diag)

[<Fact>]
let noCrossFileDiagOnSingleFileFolders () =
    let doc =
        FakeDoc.Mk(
            [|
                "[](bad.md)" //
                "[[another-bad]]"
                "[bad-ref][bad-ref]"
            |]
        )

    let folder = Folder.singleFile doc None
    let diag = diagToHuman folder

    Assert.Equal<string * string>(
        [
            "fake.md", "Link to non-existent link definition with the label 'bad-ref'"
        ],
        diag
    )

module AffectedDocumentTests =
    let private doc path lines = FakeDoc.Mk(path = path, contentLines = Array.ofList lines)

    let private affected before after =
        let candidates =
            WorkspaceDiag.affectedDocuments
                (Workspace.ofFolders None [ before ])
                (Workspace.ofFolders None [ after ])
            |> Map.tryFind (Folder.id before)
            |> Option.defaultValue Set.empty

        let fullDiagnostics folder =
            WorkspaceDiag.calculate None (Workspace.ofFolders None [ folder ])
            |> fst
            |> Map.find (Folder.id folder)

        let previous = fullDiagnostics before
        let current = fullDiagnostics after

        let changedDiagnostics =
            Set.union (Map.keys previous |> Set.ofSeq) (Map.keys current |> Set.ofSeq)
            |> Set.filter (fun docId -> Map.tryFind docId previous <> Map.tryFind docId current)

        Assert.True(
            Set.isSubset changedDiagnostics candidates,
            $"Documents with changed diagnostics were not selected: {changedDiagnostics - candidates}"
        )

        candidates

    [<Fact>]
    let newlyAvailableTargetAffectsItsPreviouslyBrokenSource () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let target = doc "target.md" [ "# Target"; "## Section" ]
        let before = FakeFolder.Mk [ source ]
        let after = Folder.withDoc target before

        Assert.Equal<Set<DocId>>(Set.ofList [ source.Id; target.Id ], affected before after)

    [<Fact>]
    let newMatchingTargetAffectsAPreviouslyUnambiguousSource () =
        let source = doc "source.md" [ "[[Target]]" ]
        let first = doc "one/target.md" [ "# Target" ]
        let second = doc "two/target.md" [ "# Target" ]
        let before = FakeFolder.Mk [ source; first ]
        let after = Folder.withDoc second before

        Assert.Empty(checkDoc before source)

        match Assert.Single(checkDoc after source) with
        | AmbiguousLink _ -> ()
        | diagnostic -> failwith $"Expected an ambiguous link, got {diagnostic}"

        Assert.Equal<Set<DocId>>(Set.ofList [ source.Id; second.Id ], affected before after)

    [<Fact>]
    let movingATargetHeadingAffectsIncomingReferences () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let target = doc "first.md" [ "# Target"; "## Section" ]
        let other = doc "second.md" [ "# Target"; "## Section" ]
        let moved = doc "first.md" [ "# Target"; ""; "## Section" ]
        let before = FakeFolder.Mk [ source; target; other ]
        let after = Folder.withDoc moved before

        let relatedLine folder =
            match Assert.Single(checkDoc folder source) with
            | AmbiguousLink(_, _, destinations) ->
                destinations
                |> Array.find (fun destination -> Dest.doc destination |> Doc.id = target.Id)
                |> Dest.range
                |> fun range -> range.Start.Line
            | diagnostic -> failwith $"Expected an ambiguous link, got {diagnostic}"

        Assert.Equal(1, relatedLine before)
        Assert.Equal(2, relatedLine after)
        Assert.Equal<Set<DocId>>(Set.ofList [ source.Id; target.Id ], affected before after)

    [<Fact>]
    let renamingATargetTitleAffectsReferencesToItsOldAlias () =
        let source = doc "source.md" [ "[[Target]]" ]
        let target = doc "other.md" [ "# Target" ]
        let renamed = doc "other.md" [ "# Renamed" ]
        let before = FakeFolder.Mk [ source; target ]
        let after = Folder.withDoc renamed before

        Assert.Empty(checkDoc before source)

        match Assert.Single(checkDoc after source) with
        | BrokenLink _ -> ()
        | diagnostic -> failwith $"Expected a broken link, got {diagnostic}"

        Assert.Equal<Set<DocId>>(Set.ofList [ source.Id; target.Id ], affected before after)

    [<Fact>]
    let removingATargetAffectsItsIncomingReferences () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let target = doc "target.md" [ "# Target"; "## Section" ]
        let before = FakeFolder.Mk [ source; target ]
        let after = Folder.withoutDoc target.Id before |> Option.get

        Assert.Equal<Set<DocId>>(Set.ofList [ source.Id; target.Id ], affected before after)

    [<Fact>]
    let removingOneOfTwoTargetsAffectsThePreviouslyAmbiguousSource () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let first = doc "first.md" [ "# Target"; "## Section" ]
        let second = doc "second.md" [ "# Target"; "## Section" ]
        let before = FakeFolder.Mk [ source; first; second ]
        let after = Folder.withoutDoc second.Id before |> Option.get

        match Assert.Single(checkDoc before source) with
        | AmbiguousLink _ -> ()
        | diagnostic -> failwith $"Expected an ambiguous link, got {diagnostic}"

        Assert.Empty(checkDoc after source)
        Assert.Equal<Set<DocId>>(Set.ofList [ source.Id; second.Id ], affected before after)

    [<Fact>]
    let unrelatedProseEditAffectsOnlyTheEditedDocument () =
        let source = doc "source.md" [ "[[Missing]]" ]
        let unrelated = doc "unrelated.md" [ "Some prose." ]
        let edited = doc "unrelated.md" [ "Some more prose." ]
        let before = FakeFolder.Mk [ source; unrelated ]
        let after = Folder.withDoc edited before

        Assert.Equal<Set<DocId>>(Set.singleton unrelated.Id, affected before after)

    [<Fact>]
    let unchangedSnapshotsAndRevertedEditsAffectNoDocuments () =
        let source = doc "source.md" [ "[[Missing]]" ]
        let initial = FakeFolder.Mk [ source ]
        let edited = Folder.withDoc (doc "source.md" [ "[[Target]]" ]) initial
        let reverted = Folder.withDoc source edited
        let target = doc "missing.md" [ "# Missing" ]
        let added = Folder.withDoc target initial
        let removed = Folder.withoutDoc target.Id added |> Option.get

        Assert.Empty(affected initial initial)
        Assert.Empty(affected initial reverted)
        Assert.Empty(affected initial removed)

    [<Fact>]
    let aVersionChangeAloneDoesNotAffectDiagnosticsOfAnAlreadyOpenDocument () =
        let source = doc "source.md" [ "[[Missing]]" ]

        let openAt version =
            Doc.mk ParserSettings.Default source.Id (Some version) (Doc.text source)

        let before = FakeFolder.Mk [ openAt 1 ]
        let after = Folder.withDoc (openAt 2) before

        Assert.Empty(affected before after)

    [<Fact>]
    let reopeningADocumentAffectsItEvenWhenItsTextIsUnchanged () =
        let source = doc "source.md" [ "[[Missing]]" ]

        let reopened =
            Doc.mk ParserSettings.Default source.Id (Some 1) (Doc.text source)

        let before = FakeFolder.Mk [ source ]
        let after = Folder.withDoc reopened before

        Assert.Equal<Set<DocId>>(Set.singleton source.Id, affected before after)

    [<Fact>]
    let comparisonUsesLatestStateAfterSeveralEdits () =
        let source = doc "source.md" [ "[[Target#Section]]" ]
        let initial = FakeFolder.Mk [ source ]
        let intermediate = Folder.withDoc (doc "target.md" [ "# Target" ]) initial
        let target = doc "target.md" [ "# Target"; "## Section" ]
        let latest = Folder.withDoc target intermediate

        Assert.Equal<Set<DocId>>(Set.ofList [ source.Id; target.Id ], affected initial latest)

    [<Fact>]
    let referenceComparisonHandlesAddedAndRemovedReferencesAcrossEdits () =
        let unchanged = doc "a.md" [ "[[Missing]]" ]
        let removedReference = doc "b.md" [ "[[Missing]]" ]
        let addedReference = doc "c.md" [ "Some prose." ]
        let before = FakeFolder.Mk [ unchanged; removedReference; addedReference ]

        let after =
            before
            |> Folder.withDoc (doc "b.md" [ "Some prose." ])
            |> Folder.withDoc (doc "c.md" [ "[[Missing]]" ])

        Assert.Equal<Set<DocId>>(
            Set.ofList [ removedReference.Id; addedReference.Id ],
            Marksman.Conn.Query.documentsWithChangedReferenceResolutions
                (Folder.conn before)
                (Folder.conn after)
        )

    [<Fact>]
    let folderAdditionAndRemovalAffectAllItsDocuments () =
        let first = doc "first.md" [ "[[Missing]]" ]
        let second = doc "second.md" [ "Some prose." ]
        let folder = FakeFolder.Mk [ first; second ]
        let empty = Workspace.ofFolders None []
        let populated = Workspace.ofFolders None [ folder ]
        let expected = Set.ofList [ first.Id; second.Id ]

        Assert.Equal<Set<DocId>>(
            expected,
            WorkspaceDiag.affectedDocuments empty populated
            |> Map.find (Folder.id folder)
        )

        Assert.Equal<Set<DocId>>(
            expected,
            WorkspaceDiag.affectedDocuments populated empty
            |> Map.find (Folder.id folder)
        )

    [<Fact>]
    let changingFolderConfigurationAffectsAllItsDocuments () =
        let first = doc "first.md" [ "[](missing.markdown)" ]
        let second = doc "second.md" [ "Some prose." ]
        let before = FakeFolder.Mk [ first; second ]
        let config = { Config.Default with coreMarkdownFileExtensions = Some [| "md" |] }
        let after = Folder.withConfig (Some config) before

        match Assert.Single(checkDoc before first) with
        | BrokenLink _ -> ()
        | diagnostic -> failwith $"Expected a broken link, got {diagnostic}"

        Assert.Empty(checkDoc after first)
        Assert.Equal<Set<DocId>>(Set.ofList [ first.Id; second.Id ], affected before after)

    [<Fact>]
    let editingOneFolderDoesNotAffectAnotherFolder () =
        let firstDoc =
            FakeDoc.Mk("[[Missing]]", path = "first/source.md", root = "first")

        let secondDoc =
            FakeDoc.Mk("Some prose.", path = "second/other.md", root = "second")

        let firstFolder =
            Folder.multiFile "first" (dummyRootPath [ "first" ] |> mkFolderId) [ firstDoc ] None

        let secondFolder =
            Folder.multiFile "second" (dummyRootPath [ "second" ] |> mkFolderId) [ secondDoc ] None

        let edited =
            FakeDoc.Mk("More prose.", path = "second/other.md", root = "second")

        let before = Workspace.ofFolders None [ firstFolder; secondFolder ]

        let after =
            Workspace.ofFolders None [ firstFolder; Folder.withDoc edited secondFolder ]

        let candidates = WorkspaceDiag.affectedDocuments before after

        Assert.Equal<Set<FolderId>>(
            Set.singleton (Folder.id secondFolder),
            Map.keys candidates |> Set.ofSeq
        )

        Assert.Equal<Set<DocId>>(
            Set.singleton secondDoc.Id,
            Map.find (Folder.id secondFolder) candidates
        )
