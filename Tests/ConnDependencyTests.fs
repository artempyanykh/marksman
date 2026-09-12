module Marksman.ConnDependencyTests

open Xunit
open Marksman.Helpers
open Marksman.Config
open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Syms
open Marksman.MMap
open Marksman.Graph
open Marksman.Doc
open Marksman.Folder
open Marksman.Conn

let config = {
    Config.Default with
        coreIncrementalReferences = Some true
        coreParanoid = Some true
}

let doc path lines =
    FakeDoc.Mk(path = path, config = config, contentLines = Array.ofList lines)

let folder docs = FakeFolder.Mk(docs = docs, config = config)

/// The incremental graph must have the same observable results and internal
/// connection state as a clean construction.
let assertMatchesCleanConstruction folder =
    let expected = Conn.mk (Folder.oracle folder) (Folder.syms folder)
    let difference = Conn.difference expected (Folder.conn folder)
    Assert.True(difference.IsEmpty(), difference.CompactFormat())

[<Fact>]
let globalTagLookupTracksAddedAndRemovedSources () =
    let first = doc "first.md" [ "#tag" ]
    let second = doc "second.md" [ "#tag" ]
    let tag = Sym.Tag(Tag "tag")
    let sources folder = Conn.Query.resolve (Scope.Global, tag) (Folder.conn folder)
    let both = folder [ first; second ]

    Assert.Equal<Set<ScopedSym>>(
        Set.ofList [ Scope.Doc first.Id, tag; Scope.Doc second.Id, tag ],
        sources both
    )

    let withoutFirst = Folder.withDoc (doc "first.md" [ "No tag" ]) both
    Assert.Equal<Set<ScopedSym>>(Set.singleton (Scope.Doc second.Id, tag), sources withoutFirst)

    let restored = Folder.withDoc first withoutFirst
    Assert.Equal<Set<ScopedSym>>(sources both, sources restored)

[<Fact>]
let removingMissingDocumentReferencesCleansDependencyState () =
    let source lines = doc "source.md" lines

    let f =
        folder [ source [ "[[Absent]]"; "[[Absent#One]]"; "[[Absent#Two]]" ] ]

    assertMatchesCleanConstruction f
    let f = f |> Folder.withDoc (source [ "[[Absent#Two]]" ])
    assertMatchesCleanConstruction f
    let f = f |> Folder.withDoc (source [])
    assertMatchesCleanConstruction f

[<Fact>]
let addingASectionToOneCandidateUpdatesResolution () =
    let source = doc "source.md" [ "[[Alpha#Section]]" ]
    let a = doc "a.md" [ "# Alpha"; "## Section" ]
    let b = doc "b.md" [ "# Alpha" ]
    let f = folder [ source; a; b ]
    assertMatchesCleanConstruction f

    let ref =
        Scope.Doc source.Id, CrossRef(CrossSection("Alpha", Slug.ofString "Section"))

    let f = f |> Folder.withDoc (doc "b.md" [ "# Alpha"; "## Section" ])
    assertMatchesCleanConstruction f
    let targets = Query.resolve (fst ref, Sym.Ref(snd ref)) (Folder.conn f)
    Assert.Equal(2, targets.Count)
    let f = f |> Folder.withoutDoc a.Id |> Option.get
    assertMatchesCleanConstruction f

    Assert.Single(Query.resolve (fst ref, Sym.Ref(snd ref)) (Folder.conn f))
    |> ignore

[<Theory>]
[<InlineData("[[./a#Section]]", "group/a.md")>]
[<InlineData("[[../a#Section]]", "a.md")>]
[<InlineData("[[/a#Section]]", "a.md")>]
[<InlineData("[[a#Section]]", "nested/a.md")>]
[<InlineData("[[nested/a#Section]]", "nested/a.md")>]
[<InlineData("[[nested\\a#Section]]", "nested/a.md")>]
[<InlineData("[[space%20name#Section]]", "nested/space name.md")>]
[<InlineData("[[a.md#Section]]", "nested/a.markdown")>]
let newPathCandidatesResolvePreviouslyMissingReferences link targetPath =
    let source = doc "group/source.md" [ link ]
    let target = doc targetPath [ "# Different title"; "## Section" ]
    let f = folder [ source ] |> Folder.withDoc target
    assertMatchesCleanConstruction f
    let ref = Doc.syms source |> Seq.choose Sym.asRef |> Seq.exactlyOne

    let expected =
        Set.singleton (Scope.Doc target.Id, Sym.Def(Header(2, "section")))

    Assert.Equal<Set<ScopedSym>>(
        expected,
        Query.resolve (Scope.Doc source.Id, Sym.Ref ref) (Folder.conn f)
    )

    f
    |> Folder.withoutDoc target.Id
    |> Option.get
    |> assertMatchesCleanConstruction

[<Fact>]
let canonicalPathReplacementChangesDocumentIdentity () =
    let before = doc "a.md" [ "# Alpha"; "## Section" ]
    let after = doc "a.markdown" [ "# Alpha"; "## Section" ]
    let source = doc "source.md" [ "[[a#Section]]"; "[[Alpha]]" ]
    let f = folder [ source; before ] |> Folder.withDoc after
    assertMatchesCleanConstruction f

    // Removing via an equivalent canonical path must remove the actual owner.
    f
    |> Folder.withoutDoc before.Id
    |> Option.get
    |> assertMatchesCleanConstruction

[<Fact>]
let documentWithoutTitleResolvesToDocumentDefinition () =
    let target = doc "Alpha.md" [ "Just some body text, no heading here." ]
    let source = doc "source.md" [ "[[Alpha]]" ]
    let f = folder [ source; target ]
    let expected = Set.singleton (Scope.Doc target.Id, Sym.Def Def.Doc)

    Assert.Equal<Set<ScopedSym>>(
        expected,
        Query.resolve (Scope.Doc source.Id, Sym.Ref(CrossRef(CrossDoc "Alpha"))) (Folder.conn f)
    )

    assertMatchesCleanConstruction f

/// Count actual oracle calls, not timings or an implementation-maintained counter.
type OracleCallCounts = { candidateDocumentResolutions: int; definitionSelections: int }

let updateWithOracleCallCounts before after others =
    let oldFolder = folder (before :: others)
    let newFolder = folder (after :: others)
    let oracle = Folder.oracle newFolder
    let mutable candidateDocumentResolutions = 0
    let mutable definitionSelections = 0

    let counted = {
        oracle with
            resolveCandidateDocuments =
                fun name ->
                    candidateDocumentResolutions <- candidateDocumentResolutions + 1
                    oracle.resolveCandidateDocuments name
            selectDefinitions =
                fun selector ->
                    definitionSelections <- definitionSelections + 1
                    oracle.selectDefinitions selector
    }

    let aliases doc =
        DocumentAlias.ofDocument
            (config.CoreMarkdownFileExtensions())
            (Doc.slug doc)
            (Doc.pathFromRoot doc)

    let beforeAliases, afterAliases = aliases before, aliases after

    let change: ConnectionChange = {
        symbolDifference =
            Doc.symsDifference before after
            |> Difference.map (Sym.scopedToDoc before.Id)
        invalidatedDocumentAliases = (beforeAliases - afterAliases) + (afterAliases - beforeAliases)
    }

    let actual = Conn.update counted change (Folder.conn oldFolder)
    let diff = Conn.difference (Folder.conn newFolder) actual
    Assert.True(diff.IsEmpty(), diff.CompactFormat())

    {
        candidateDocumentResolutions = candidateDocumentResolutions
        definitionSelections = definitionSelections
    }

[<Theory>]
[<InlineData(10)>]
[<InlineData(1000)>]
let unlinkedRenameDoesNotEvaluateExistingReferences size =
    let others = [
        for i in 0 .. size - 1 -> doc $"doc{i}.md" [ "[[doc0]]"; "[[doc0#Section]]"; "## Section" ]
    ]

    let counts =
        updateWithOracleCallCounts
            (doc "unlinked.md" [ "# Before" ])
            (doc "unlinked.md" [ "# After" ])
            others

    Assert.Equal<OracleCallCounts>(
        { candidateDocumentResolutions = 0; definitionSelections = 0 },
        counts
    )

[<Theory>]
[<InlineData(10)>]
[<InlineData(1000)>]
let addingHeadingDoesNotRetryMissingDocuments size =
    let others = [
        for i in 0 .. size - 1 -> doc $"doc{i}.md" [ "[[Absent#Section]]" ]
    ]

    let counts =
        updateWithOracleCallCounts (doc "a.md" []) (doc "a.md" [ "## Section" ]) others

    Assert.Equal<OracleCallCounts>(
        { candidateDocumentResolutions = 0; definitionSelections = 0 },
        counts
    )

[<Fact>]
let headingEditReevaluatesOnlyReadersOfThatHeading () =
    let others = [ doc "source.md" [ "[[a#Section]]"; "[[a#Other]]"; "[[a]]" ] ]

    let counts =
        updateWithOracleCallCounts
            (doc "a.md" [ "## Other" ])
            (doc "a.md" [ "## Other"; "## Section" ])
            others

    Assert.Equal<OracleCallCounts>(
        { candidateDocumentResolutions = 0; definitionSelections = 1 },
        counts
    )

[<Theory>]
[<InlineData(10)>]
[<InlineData(1000)>]
let aDefinitionSelectionIsSharedByAllDependentReferences size =
    let sources = [ for i in 0 .. size - 1 -> doc $"source{i}.md" [ "[[a#Section]]" ] ]

    let counts =
        updateWithOracleCallCounts (doc "a.md" [ "## Section" ]) (doc "a.md" [ "## Other" ]) sources

    Assert.Equal<OracleCallCounts>(
        { candidateDocumentResolutions = 0; definitionSelections = 1 },
        counts
    )

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let editsAcrossParserConfigurations titleFromHeading =
    let config = {
        config with
            coreTitleFromHeading = Some titleFromHeading
            coreMarkdownFileExtensions = Some [| "md"; "mdown" |]
    }

    let doc path lines =
        FakeDoc.Mk(path = path, config = config, contentLines = Array.ofList lines)

    let paths = [| "nested/a.md"; "a.mdown"; "source.md"; "other.md" |]

    let contents = [|
        [ "# Alpha"; "## Section" ]
        [ "# Beta"; "# Alpha"; "## Other" ]
        [ "# Alpha"; "# Beta"; "## Other" ]
        [ "[[Alpha#Section]]"; "[[a.mdown#Other]]"; "[[nested/a#Section]]" ]
        [ "[link](./a.md#section)"; "[[#Section]]"; "## Section" ]
        [ "[label]"; ""; "[label]: /url" ]
        [ "[label]"; "#tag" ]
        []
    |]

    let random = System.Random(456)
    let mutable f = FakeFolder.Mk(docs = [], config = config)

    for step in 1..300 do
        let path = paths[random.Next(paths.Length)]
        let d = doc path contents[random.Next(contents.Length)]

        f <-
            if random.Next(5) = 0 then
                Folder.withoutDoc d.Id f |> Option.get
            else
                Folder.withDoc d f

        try
            assertMatchesCleanConstruction f
        with ex ->
            failwith
                $"Parser configuration titleFromHeading={titleFromHeading}, step={step}, path={path}: {ex.Message}"

[<Fact>]
let candidateDocumentsAreResolvedOnceForMultipleSections () =
    let counts =
        updateWithOracleCallCounts
            (doc "a.md" [ "# Before"; "## One"; "## Two" ])
            (doc "a.md" [ "# After"; "## One"; "## Two" ])
            [
                doc "source.md" [ "[[Before]]"; "[[Before#One]]"; "[[Before#Two]]" ]
            ]
    // Both dirty computations are evaluated once before the references detach
    // from the definition selection that is no longer reachable.
    Assert.Equal<OracleCallCounts>(
        { candidateDocumentResolutions = 1; definitionSelections = 1 },
        counts
    )

[<Fact>]
let titleChangeUpdatesDefinitionSelectionWithoutResolvingCandidatesAgain () =
    let counts =
        updateWithOracleCallCounts
            (doc "a.md" [ "# Before"; "## Section" ])
            (doc "a.md" [ "# After"; "## Section" ])
            [ doc "source.md" [ "[[a]]"; "[[a#Section]]" ] ]

    Assert.Equal<OracleCallCounts>(
        { candidateDocumentResolutions = 0; definitionSelections = 1 },
        counts
    )

[<Fact>]
let batchChangesCanMoveCandidatesAndReplaceReferenceSources () =
    let oldTarget = doc "old.md" [ "# Alpha"; "## Section" ]
    let newTarget = doc "new.md" [ "# Alpha"; "## Section"; "## Other" ]
    let oldSource = doc "source.md" [ "[[Alpha]]"; "[[Alpha#Section]]" ]
    let newSource = doc "source.md" [ "[[Alpha#Other]]"; "[[Alpha#Section]]" ]
    let before = folder [ oldTarget; oldSource ]
    let after = folder [ newTarget; newSource ]
    let _, symbols = Folder.symsDifference before after

    let aliases document =
        DocumentAlias.ofDocument
            (config.CoreMarkdownFileExtensions())
            (Doc.slug document)
            (Doc.pathFromRoot document)

    let change: ConnectionChange = {
        symbolDifference = symbols
        invalidatedDocumentAliases = aliases oldTarget + aliases newTarget
    }

    let actual = Conn.update (Folder.oracle after) change (Folder.conn before)
    let diff = Conn.difference (Folder.conn after) actual
    Assert.True(diff.IsEmpty(), diff.CompactFormat())
    let reference = CrossRef(CrossSection("Alpha", Slug.ofString "Section"))

    let expected =
        Set.singleton (Scope.Doc newTarget.Id, Sym.Def(Header(2, "section")))

    Assert.Equal<Set<ScopedSym>>(
        expected,
        Query.resolve (Scope.Doc newSource.Id, Sym.Ref reference) actual
    )

[<Fact>]
let changingExtensionsRebuildsCanonicalPathAliases () =
    let beforeConfig = { config with coreMarkdownFileExtensions = Some [| "mdown" |] }
    let target = doc "target.md" [ "# Different"; "## Section" ]

    let source =
        doc "source.md" [ "[[target#Section]]"; "[[/target.md#Section]]" ]

    let f =
        FakeFolder.Mk(docs = [ target; source ], config = beforeConfig)
        |> Folder.withConfig (Some config)

    let expected =
        Set.singleton (Scope.Doc target.Id, Sym.Def(Header(2, "section")))

    for name in [ "target"; "/target.md" ] do
        let ref = CrossRef(CrossSection(name, Slug.ofString "Section"))

        Assert.Equal<Set<ScopedSym>>(
            expected,
            Query.resolve (Scope.Doc source.Id, Sym.Ref ref) (Folder.conn f)
        )

    assertMatchesCleanConstruction f

    f
    |> Folder.withoutDoc target.Id
    |> Option.get
    |> assertMatchesCleanConstruction
