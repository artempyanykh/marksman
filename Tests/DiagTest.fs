module Marksman.DiagTest

open Xunit

open Marksman.Diag
open Marksman.Helpers
open Marksman.Index
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Folder

let entryToHuman (entry: Entry) =
    let lsp = diagToLsp entry
    lsp.Message

let diagToHuman (diag: seq<DocId * list<Entry>>) : list<string * string> =
    seq {
        for id, entries in diag do
            for e in entries do
                yield id.Path |> RootedRelPath.relPathForced |> RelPath.toSystem, entryToHuman e
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
    let diag = checkFolder folder Seq.empty |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent heading 'h42'" ], diag)

[<Fact>]
let noDiagOnRealUrls () =
    let doc =
        FakeDoc.Mk([| "# H1"; "## H2"; "[](www.bad.md)"; "[](https://www.good.md)" |])

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder Seq.empty |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'www.bad.md'" ], diag)

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
    let diag = checkFolder folder Seq.empty |> diagToHuman

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
    let diag = checkFolder folder Seq.empty |> diagToHuman

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
    let diag = checkFolder folder Seq.empty |> diagToHuman

    Assert.Equal<string * string>(
        [
            "fake.md", "Link to non-existent link definition with the label 'bad-ref'"
        ],
        diag
    )

[<Fact>]
let noBrokenLinkWhenExtraFolderHasDoc () =
    // Primary folder has a link to a doc that only exists in an extra folder
    let primaryDoc = FakeDoc.Mk([| "[[extra-doc]]" |])
    let primaryFolder = FakeFolder.Mk([ primaryDoc ])

    let extraDoc = FakeDoc.Mk([| "# Extra Doc" |], path = "extra-doc.md")
    let extraFolder = FakeFolder.Mk([ extraDoc ])

    // With extra folder: no broken link
    let diagWithExtra =
        checkFolder primaryFolder [ extraFolder ] |> diagToHuman

    Assert.Equal<string * string>([], diagWithExtra)

    // Without extra folder: broken link
    let diagWithoutExtra = checkFolder primaryFolder Seq.empty |> diagToHuman

    Assert.Equal<string * string>(
        [ "fake.md", "Link to non-existent document 'extra-doc'" ],
        diagWithoutExtra
    )

// ── Extra-folder cross-reference diagnostics ─────────────────────────────────
//
// Folders are set up with distinct roots so they can coexist as separate
// Folder objects.  FakeDoc.Mk path must be rooted under the folder root
// (e.g. path="folderA/doc-a.md", root="folderA") so the relative path
// from the root resolves to "doc-a.md".

[<Fact>]
let noBrokenLinkOnWikiCrossRef () =
    // folderA links to doc-b by slug; doc-b lives in folderB (extra folder)
    let folderAId = mkFolderId (dummyRootPath [ "folderA" ])
    let folderBId = mkFolderId (dummyRootPath [ "folderB" ])

    let docA =
        FakeDoc.Mk(content = "[[doc-b]]", path = "folderA/doc-a.md", root = "folderA")

    let docB =
        FakeDoc.Mk(content = "[[doc-a]]", path = "folderB/doc-b.md", root = "folderB")

    let folderA = Folder.multiFile "folderA" folderAId [ docA ] None
    let folderB = Folder.multiFile "folderB" folderBId [ docB ] None

    Assert.Equal<string * string>([], checkFolder folderA [ folderB ] |> diagToHuman)
    Assert.Equal<string * string>([], checkFolder folderB [ folderA ] |> diagToHuman)

[<Fact>]
let noBrokenLinkOnMarkdownCrossRef () =
    // folderA links to doc-b.md by filename; doc-b lives in folderB (extra folder)
    // Bare filename (no .. components) becomes an Approx lookup, matched by suffix
    // in the extra folder.
    let folderAId = mkFolderId (dummyRootPath [ "folderA" ])
    let folderBId = mkFolderId (dummyRootPath [ "folderB" ])

    let docA =
        FakeDoc.Mk(content = "[to B](doc-b.md)", path = "folderA/doc-a.md", root = "folderA")

    let docB =
        FakeDoc.Mk(content = "[to A](doc-a.md)", path = "folderB/doc-b.md", root = "folderB")

    let folderA = Folder.multiFile "folderA" folderAId [ docA ] None
    let folderB = Folder.multiFile "folderB" folderBId [ docB ] None

    Assert.Equal<string * string>([], checkFolder folderA [ folderB ] |> diagToHuman)
    Assert.Equal<string * string>([], checkFolder folderB [ folderA ] |> diagToHuman)

[<Fact>]
let brokenLinkDiagWhenExtraFolderLacksTarget () =
    // Only the genuinely broken link produces a diagnostic; the valid cross-ref does not
    let folderAId = mkFolderId (dummyRootPath [ "folderA" ])
    let folderBId = mkFolderId (dummyRootPath [ "folderB" ])

    let docA =
        FakeDoc.Mk(
            content = "[[doc-b]]\n[[no-such-doc]]",
            path = "folderA/doc-a.md",
            root = "folderA"
        )

    let docB =
        FakeDoc.Mk(content = "[[doc-a]]", path = "folderB/doc-b.md", root = "folderB")

    let folderA = Folder.multiFile "folderA" folderAId [ docA ] None
    let folderB = Folder.multiFile "folderB" folderBId [ docB ] None

    Assert.Equal<string * string>(
        [ "doc-a.md", "Link to non-existent document 'no-such-doc'" ],
        checkFolder folderA [ folderB ] |> diagToHuman
    )

    Assert.Equal<string * string>([], checkFolder folderB [ folderA ] |> diagToHuman)

[<Fact>]
let noBrokenLinkOnWikiCrossRefByH1Title () =
    // [[Doc B]] matches the H1 title of doc-b; doc-b also has H2 sub-sections.
    // Regression test: extra-folder resolution must use isTitle (not isHeaderOrTitle)
    // to avoid yielding H2 defs as extra destinations and triggering "Ambiguous link".
    let folderAId = mkFolderId (dummyRootPath [ "folderA" ])
    let folderBId = mkFolderId (dummyRootPath [ "folderB" ])

    let docA =
        FakeDoc.Mk(content = "[[Doc B]]", path = "folderA/doc-a.md", root = "folderA")

    let docB =
        FakeDoc.Mk(
            content = "# Doc B\n## Section One\n## Section Two",
            path = "folderB/doc-b.md",
            root = "folderB"
        )

    let folderA = Folder.multiFile "folderA" folderAId [ docA ] None
    let folderB = Folder.multiFile "folderB" folderBId [ docB ] None

    // Extra-folder resolution
    Assert.Equal<string * string>([], checkFolder folderA [ folderB ] |> diagToHuman)

    // Primary-folder resolution (Conn path) — should also be clean, baseline check
    let folderId = mkFolderId (dummyRootPath [ "folder" ])

    let docASameFolder =
        FakeDoc.Mk(content = "[[Doc B]]", path = "folder/doc-a.md", root = "folder")

    let docBSameFolder =
        FakeDoc.Mk(
            content = "# Doc B\n## Section One\n## Section Two",
            path = "folder/doc-b.md",
            root = "folder"
        )

    let sameFolder =
        Folder.multiFile "folder" folderId [ docASameFolder; docBSameFolder ] None

    Assert.Equal<string * string>([], checkFolder sameFolder Seq.empty |> diagToHuman)

[<Fact>]
let noBrokenLinkOnWikiCrossRefToSection () =
    // [[doc-b#section-one]] targets a heading in an extra-folder doc
    let folderAId = mkFolderId (dummyRootPath [ "folderA" ])
    let folderBId = mkFolderId (dummyRootPath [ "folderB" ])

    let docA =
        FakeDoc.Mk(content = "[[doc-b#section-one]]", path = "folderA/doc-a.md", root = "folderA")

    let docB =
        FakeDoc.Mk(content = "# Doc B\n## Section One", path = "folderB/doc-b.md", root = "folderB")

    let folderA = Folder.multiFile "folderA" folderAId [ docA ] None
    let folderB = Folder.multiFile "folderB" folderBId [ docB ] None

    Assert.Equal<string * string>([], checkFolder folderA [ folderB ] |> diagToHuman)

[<Fact>]
let noDiagForBrokenLinkInsideExtraFolderDoc () =
    // Broken links inside an extra-folder document must not be diagnosed from the
    // primary session — they belong to the extra folder's own LSP session.
    let folderAId = mkFolderId (dummyRootPath [ "folderA" ])
    let folderBId = mkFolderId (dummyRootPath [ "folderB" ])

    let docA =
        FakeDoc.Mk(content = "[[doc-b]]", path = "folderA/doc-a.md", root = "folderA")

    let docB =
        FakeDoc.Mk(
            content = "[[no-such-doc]]", // broken link inside extra folder
            path = "folderB/doc-b.md",
            root = "folderB"
        )

    let folderA = Folder.multiFile "folderA" folderAId [ docA ] None
    let folderB = Folder.multiFile "folderB" folderBId [ docB ] None

    // Primary session: no diagnostics — folderB's broken link is not our concern
    Assert.Equal<string * string>([], checkFolder folderA [ folderB ] |> diagToHuman)

    // Extra folder's own session would catch it (no extra folders passed)
    Assert.Equal<string * string>(
        [ "doc-b.md", "Link to non-existent document 'no-such-doc'" ],
        checkFolder folderB Seq.empty |> diagToHuman
    )

[<Fact>]
let noTransitiveExtraFolderResolution () =
    // Non-goal: A declares B as extra, B declares C as extra.
    // A link in folderA that only exists in folderC must NOT resolve — C is not
    // transitively visible from A.
    let folderAId = mkFolderId (dummyRootPath [ "folderA" ])
    let folderBId = mkFolderId (dummyRootPath [ "folderB" ])
    let folderCId = mkFolderId (dummyRootPath [ "folderC" ])

    let docA =
        FakeDoc.Mk(content = "[[doc-c]]", path = "folderA/doc-a.md", root = "folderA")

    let docB =
        FakeDoc.Mk(content = "[[doc-c]]", path = "folderB/doc-b.md", root = "folderB")

    let docC =
        FakeDoc.Mk(content = "# Doc C", path = "folderC/doc-c.md", root = "folderC")

    let folderA = Folder.multiFile "folderA" folderAId [ docA ] None
    let folderB = Folder.multiFile "folderB" folderBId [ docB ] None
    let folderC = Folder.multiFile "folderC" folderCId [ docC ] None

    // B can see C (B declares C as extra) — resolves fine
    Assert.Equal<string * string>([], checkFolder folderB [ folderC ] |> diagToHuman)

    // A can only see B (A declares B as extra, not C) — link to doc-c is broken
    Assert.Equal<string * string>(
        [ "doc-a.md", "Link to non-existent document 'doc-c'" ],
        checkFolder folderA [ folderB ] |> diagToHuman
    )
