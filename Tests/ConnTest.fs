module Marksman.ConnTest

open Marksman.Config
open Xunit

open Snapper
open Snapper.Attributes

open Marksman.Misc
open Marksman.Helpers
open Marksman.MMap
open Marksman.Folder
open Marksman.Conn

let d1 =
    FakeDoc.Mk(
        path = "d1.md",
        contentLines = [|
            "# Doc 1" //
            ""
            "## D1.S1"
            ""
            "[[doc-2]]"
            "[[doc-NA]]"
            ""
        |]
    )

let d1_dup =
    FakeDoc.Mk(
        path = "d1_dup.md",
        contentLines = [|
            "# Doc 1" //
            ""
            "[[doc-2]]"
            ""
        |]
    )

let d2 =
    FakeDoc.Mk(
        path = "d2.md",
        contentLines = [|
            "# Doc 2" //
            "[[doc-1]]"
            "[[doc-1#d1s1]]"
            "[[doc-NA2]]"
            ""
        |]
    )

let d3 =
    FakeDoc.Mk(
        path = "d3.md",
        contentLines = [|
            "# Doc 3"
            ""
            "[link1]"
            "[link1][]"
            "[linkX]"
            ""
            "[link1]: /url1"
            ""
        |]
    )

let checkSnapshot (conn: Conn) = conn.CompactFormat().Lines().ShouldMatchSnapshot()

let emptyOracle = {
    resolveCandidateDocuments = fun _ -> { documents = Set.empty; aliasesRead = Set.empty } //
    selectDefinitions = fun _ -> [||]
}

let incrConfig = { Config.Config.Default with coreIncrementalReferences = Some true }

let mkFolder docs = FakeFolder.Mk(config = incrConfig, docs = docs)

[<StoreSnapshotsPerClass>]
module ConnGraphTests =
    [<Fact>]
    let emptyGraph () =
        let conn = Conn.mk emptyOracle MMap.empty
        checkSnapshot conn

    [<Fact>]
    let initGraph () =
        let f = mkFolder [ d1; d1_dup; d2; d3 ]
        let conn = Conn.mk (Folder.oracle f) (Folder.syms f)
        checkSnapshot conn

    [<Fact>]
    let removeDoc () =
        let f1 = mkFolder [ d1; d1_dup; d2; d3 ]
        let f2 = Folder.withoutDoc d1_dup.Id f1 |> Option.get

        checkSnapshot (Folder.conn f2)

    [<Fact>]
    let addDoc () =
        let f1 = mkFolder [ d1; d2; d3 ]

        let dNA =
            FakeDoc.Mk(
                path = "docNA.md",
                contentLines = [|
                    "# Doc NA" //
                    ""
                |]
            )

        let f2 = Folder.withDoc dNA f1
        checkSnapshot (Folder.conn f2)

    [<Fact>]
    let addLinkDef () =
        let f1 = mkFolder [ d1; d2; d3 ]

        let d3Update =
            FakeDoc.Mk(
                path = "d3.md",
                contentLines = [|
                    "# Doc 3"
                    ""
                    "[link1]"
                    "[link1][]"
                    "[linkX]"
                    ""
                    "[link1]: /url1"
                    "[linkX]: /url2"
                    ""
                |]
            )

        let f2 = Folder.withDoc d3Update f1
        checkSnapshot (Folder.conn f2)

    [<Fact>]
    let removeHeading () =
        let f1 = mkFolder [ d1; d2; d3 ]

        let d1Update =
            FakeDoc.Mk(
                path = "d1.md",
                contentLines = [|
                    "# Doc 1" //
                    ""
                    "[[doc-2]]"
                    "[[doc-NA]]"
                    ""
                |]
            )

        let f2 = Folder.withDoc d1Update f1
        checkSnapshot (Folder.conn f2)

    [<Fact>]
    let removeTitle_PARANOID () =
        let d1 =
            FakeDoc.Mk(
                path = "ocaml.md",
                contentLines = [| "# OCaml"; ""; "## Multicore"; "[[#Multicore]]" |]
            )

        let d1Upd =
            FakeDoc.Mk(path = "ocaml.md", contentLines = [| ""; "## Multicore" |])

        let d2 =
            FakeDoc.Mk(path = "fsharp.md", contentLines = [| "# FSharp"; "[[OCaml#Multicore]]" |])

        let config = {
            incrConfig with
                complWikiStyle = Some Config.FilePathStem
                coreParanoid = Some true
        }

        let incr =
            FakeFolder.Mk(docs = [ d1; d2 ], config = config)
            |> Folder.withDoc d1Upd
            |> Folder.conn

        let fromScratch =
            FakeFolder.Mk(docs = [ d1Upd; d2 ], config = config) |> Folder.conn

        let connDiff = Conn.difference fromScratch incr

        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let addTitle_SameAsFileName () =
        let d1 = FakeDoc.Mk(path = "d1.md", contentLines = [| "[[d2]]" |])
        let d2 = FakeDoc.Mk(path = "d2.md", contentLines = [| "Some content" |])
        let d3 = FakeDoc.Mk(path = "d3.md", contentLines = [| "# D3" |])

        let d3Upd = FakeDoc.Mk(path = "d3.md", contentLines = [| "# D2" |])

        let incr = mkFolder [ d1; d2; d3 ] |> Folder.withDoc d3Upd |> Folder.conn

        let fromScratch = mkFolder [ d1; d2; d3Upd ] |> Folder.conn

        let connDiff = Conn.difference fromScratch incr
        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let addTitle_CrossSection () =
        let d1 =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# D1"; "[[d2#sub]]" |])

        let d2 = FakeDoc.Mk(path = "d2.md", contentLines = [| "# D2"; "## Sub" |])

        let d1Upd =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# D2"; "[[d2#sub]]" |])

        let incr = mkFolder [ d1; d2 ] |> Folder.withDoc d1Upd |> Folder.conn

        let fromScratch = mkFolder [ d1Upd; d2 ] |> Folder.conn

        let connDiff = Conn.difference fromScratch incr
        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let fixRef () =
        let d1 =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "[[#Lnk]]"; "## Link" |])

        let d1Upd =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "[[#Link]]"; "## Link" |])

        let incr = mkFolder [ d1 ] |> Folder.withDoc d1Upd |> Folder.conn
        let fromScratch = mkFolder [ d1Upd ] |> Folder.conn

        let connDiff = Conn.difference fromScratch incr

        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let breakCrossRef () =
        let d1 =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# Doc1 idx"; "## Sub" |])

        // Update (remove + add a def)
        let d1Upd =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# Doc1 index"; "## Sub" |])

        let d2 =
            FakeDoc.Mk(path = "d2.md", contentLines = [| "[[Doc1 idx#Sub]]" |])

        let f = mkFolder [ d1; d2 ]
        let f = Folder.withDoc d1Upd f
        let incr = Folder.conn f

        let fromScratch = mkFolder [ d1Upd; d2 ] |> Folder.conn
        let connDiff = Conn.difference fromScratch incr

        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let addingEmptyHeader () =
        let d1 =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# Doc1"; "## Sub" |])

        let d2 = FakeDoc.Mk(path = "d2.md", contentLines = [| "[[Doc1#Sub]]" |])

        let d1Upd =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# Doc1"; "## Sub"; "# " |])


        let f = mkFolder [ d1; d2 ]
        let f = Folder.withDoc d1Upd f
        let incr = Folder.conn f

        let fromScratch = mkFolder [ d1Upd; d2 ] |> Folder.conn
        let connDiff = Conn.difference fromScratch incr

        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    module RenameTests =
        let d1 =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# Doc1 idx"; "## Sub" |])

        // Update (remove + add a def)
        let d1Upd =
            FakeDoc.Mk(path = "d1.md", contentLines = [| "# Doc1 index"; "## Sub" |])

        let d2 =
            FakeDoc.Mk(path = "d2.md", contentLines = [| "[[Doc1 idx#Sub]]" |])

        // Fix the reference
        let d2Upd =
            FakeDoc.Mk(path = "d2.md", contentLines = [| "[[Doc1 index#Sub]]" |])

        [<Fact>]
        let renameCrossRef_D1_then_D2 () =
            let f = mkFolder [ d1; d2 ]
            let f = Folder.withDoc d1Upd f |> Folder.withDoc d2Upd
            let incr = Folder.conn f

            let fromScratch = mkFolder [ d1Upd; d2Upd ] |> Folder.conn
            let connDiff = Conn.difference fromScratch incr

            checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

        [<Fact>]
        let renameCrossRef_D2_then_D1 () =
            let f = mkFolder [ d1; d2 ]
            let f = Folder.withDoc d2Upd f |> Folder.withDoc d1Upd
            let incr = Folder.conn f

            let fromScratch = mkFolder [ d1Upd; d2Upd ] |> Folder.conn
            let connDiff = Conn.difference fromScratch incr

            checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let addDocThenTitle () =
        let d1 =
            FakeDoc.Mk(path = "doc-1.md", contentLines = [| "[[non-existent]]" |])

        let dNA1 = FakeDoc.Mk(path = "non-existent.md", contentLines = [| "" |])

        let dNA2 =
            FakeDoc.Mk(path = "non-existent.md", contentLines = [| "# D" |])

        let incr =
            mkFolder [ d1 ]
            |> Folder.withDoc dNA1
            |> Folder.withDoc dNA2
            |> Folder.conn

        let fromScratch = mkFolder [ d1; dNA2 ] |> Folder.conn
        let connDiff = Conn.difference fromScratch incr
        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let addSecondTitle () =
        let d1 = FakeDoc.Mk(path = "doc-1.md", contentLines = [| "[[doc-2]]" |])
        let d2 = FakeDoc.Mk(path = "doc-2.md", contentLines = [| "" |])
        let d21 = FakeDoc.Mk(path = "doc-2.md", contentLines = [| "# T1" |])

        let d22 =
            FakeDoc.Mk(path = "doc-2.md", contentLines = [| "# T1"; "# T2" |])

        let incr =
            mkFolder [ d1; d2 ]
            |> Folder.withDoc d21
            |> Folder.withDoc d22
            |> Folder.conn

        let fromScratch = mkFolder [ d1; d22 ] |> Folder.conn
        let connDiff = Conn.difference fromScratch incr
        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

    [<Fact>]
    let initGraphWithTags () =
        let d1 = FakeDoc.Mk(path = "d1.md", contentLines = [| "#tag1 #tag2" |])
        let d2 = FakeDoc.Mk(path = "d2.md", contentLines = [| "#tag2"; "#tag3" |])
        let f = mkFolder [ d1; d2 ]
        let conn = Conn.mk (Folder.oracle f) (Folder.syms f)
        checkSnapshot conn

    [<Fact>]
    let removingTag () =
        let d1 = FakeDoc.Mk(path = "d1.md", contentLines = [| "#tag1 #tag2" |])
        let d1' = FakeDoc.Mk(path = "d1.md", contentLines = [| "#tag1" |])
        let d2 = FakeDoc.Mk(path = "d2.md", contentLines = [| "#tag2"; "#tag3" |])
        let f' = mkFolder [ d1; d2 ] |> Folder.withDoc d1'
        checkSnapshot (Folder.conn f')

[<StoreSnapshotsPerClass>]
module ConnGraphTests_TitleLess =
    let incrConfig = {
        Config.Config.Default with
            coreIncrementalReferences = Some true
            coreTitleFromHeading = Some false
            complWikiStyle = Some ComplWikiStyle.TitleSlug
    }

    let mkFolder docs = FakeFolder.Mk(config = incrConfig, docs = docs)
    let mkDoc path content = FakeDoc.Mk(path = path, config = incrConfig, contentLines = content)

    [<Fact>]
    let updateH1 () =
        let d1 =
            mkDoc "ocaml.md" [| "# OCaml"; "[[#Multicore]]"; "## Multicore" |]

        let d1' =
            mkDoc "ocaml.md" [| "# OCaml L"; "[[#Multicore]]"; "## Multicore" |]

        let d2 = mkDoc "test.md" [| "# Test"; "[[OCaml#Multicore]]" |]

        let incr =
            mkFolder [ d1 ]
            |> Folder.withDoc d2
            |> Folder.withDoc d1'
            |> Folder.conn

        let fromScratch = mkFolder [ d1'; d2 ] |> Folder.conn
        let connDiff = Conn.difference fromScratch incr
        checkInlineSnapshot id [ connDiff.CompactFormat() ] [ "" ]

module IncrementalRegressionTests =
    open Marksman.ConnDependencyTests

    let doc path lines = FakeDoc.Mk(path = path, contentLines = Array.ofList lines)

    [<Theory>]
    [<InlineData("remove section")>]
    [<InlineData("remove explicit doc")>]
    [<InlineData("add matching heading")>]
    [<InlineData("rename missing target")>]
    [<InlineData("delete missing target")>]
    [<InlineData("add ambiguous target")>]
    let lifecycle scenario =
        let target = doc "target.md" [ "# Target"; "## Sub" ]
        let source lines = doc "source.md" lines

        let folder, update =
            match scenario with
            | "remove section" ->
                mkFolder [ target; source [ "[[target#Sub]]" ] ], Folder.withDoc (source [])
            | "remove explicit doc" ->
                mkFolder [ target; source [ "[[target]]"; "[[target#Sub]]" ] ],
                Folder.withDoc (source [ "[[target#Sub]]" ])
            | "add matching heading" ->
                mkFolder [ target; source [ "[[target#Sub]]" ] ],
                Folder.withDoc (doc "target.md" [ "# Target"; "## Sub"; "### Sub" ])
            | "rename missing target" ->
                mkFolder [ target; source [ "[[Target#Missing]]" ] ],
                Folder.withDoc (doc "target.md" [ "# Other"; "## Sub" ])
            | "delete missing target" ->
                mkFolder [ target; source [ "[[target#Missing]]" ] ],
                (Folder.withoutDoc target.Id >> Option.get)
            | _ ->
                mkFolder [ target; source [ "[[target#Sub]]" ] ],
                Folder.withDoc (doc "other.md" [ "# Target"; "## Sub" ])

        Assert.True(
            (Folder.configOrDefault folder).CoreIncrementalReferences(),
            "incremental enabled"
        )

        let updated = update folder
        assertMatchesCleanConstruction updated

        updated
        |> Folder.withDoc (doc "target.md" [ "# Renamed" ])
        |> assertMatchesCleanConstruction

    [<Fact>]
    let addingAmbiguousScopeInvalidatesMissingSections () =
        mkFolder [ doc "one.md" [ "# Alpha" ]; doc "source.md" [ "[[Alpha#Sub]]" ] ]
        |> Folder.withDoc (doc "two.md" [ "# Alpha"; "## Sub" ])
        |> assertMatchesCleanConstruction

    [<Fact>]
    let addingPathMatchWhenExistingMatchHasDifferentTitle () =
        mkFolder [ doc "nested/a.md" [ "# Beta" ]; doc "source.md" [ "[[a]]" ] ]
        |> Folder.withDoc (doc "a.md" [])
        |> assertMatchesCleanConstruction

    [<Fact>]
    let renamingScopeClearsOldUnresolvedEdges () =
        mkFolder [ doc "one.md" [ "# Alpha" ]; doc "source.md" [ "[[Alpha#Sub]]" ] ]
        |> Folder.withDoc (doc "one.md" [ "# Beta" ])
        |> assertMatchesCleanConstruction

    [<Fact>]
    let reorderingTitlesChangesCandidateSelection () =
        mkFolder [
            doc "one.md" [ "# Alpha"; "# Beta" ]
            doc "source.md" [ "[[Alpha]]"; "[[Beta]]" ]
        ]
        |> Folder.withDoc (doc "one.md" [ "# Beta"; "# Alpha" ])
        |> assertMatchesCleanConstruction

    [<Theory>]
    [<InlineData(17)>]
    [<InlineData(42)>]
    [<InlineData(123)>]
    let editSequences seed =
        let random = System.Random(seed)
        let paths = [| "a.md"; "b.md"; "nested/a.md"; "c.md" |]

        let contents = [|
            []
            [ "# Alpha"; "## Sub" ]
            [ "# Beta"; "## Other" ]
            [ "# Alpha"; "## Other"; "# Beta" ]
            [ "[[Alpha#Sub]]"; "[[a#Other]]"; "[[b]]" ]
            [ "[[Alpha]]"; "[[Alpha#Other]]"; "[[#Sub]]"; "## Sub" ]
            [ "# Beta"; "[[Alpha#Missing]]"; "[[nested/a#Sub]]" ]
            [ "# Alpha"; "#tag"; "[link]"; "[link]: /url" ]
            [ "# Beta"; "# Alpha"; "## Other" ]
            [ "[[a]]"; "[[a#Sub]]"; "[[a#Other]]" ]
            [ "[[a#Other]]"; "[[./a#Sub]]"; "[[/a#Sub]]" ]
            [ "## Sub"; "### Other"; "[link]" ]
            [ "[link]: /url"; "[link]"; "#tag" ]
        |]

        let mutable folder = mkFolder []
        let history = ResizeArray<string>()

        for step in 1..1000 do
            let path = paths[random.Next(paths.Length)]
            let content = random.Next(contents.Length)
            let next = doc path contents[content]
            let delete = random.Next(5) = 0
            let action = if delete then "deleted" else string content
            history.Add($"{step}: {path} = {action}")

            folder <-
                if delete then
                    Folder.withoutDoc next.Id folder |> Option.get
                else
                    Folder.withDoc next folder

            try
                assertMatchesCleanConstruction folder
            with ex ->
                let edits = System.String.Join("\n", history)
                failwith $"seed {seed}\n{edits}\n{ex.Message}"

    [<Fact>]
    let removingTitleClearsUnresolvedScope () =
        mkFolder [
            doc "one.md" [ "# Alpha" ]
            doc "two.md" [ "# Alpha"; "## Sub" ]
            doc "source.md" [ "[[Alpha#Sub]]" ]
        ]
        |> Folder.withDoc (doc "one.md" [])
        |> assertMatchesCleanConstruction

    [<Fact>]
    let paranoidScopeChanges () =
        let config = { incrConfig with coreParanoid = Some true }

        FakeFolder.Mk(
            config = config,
            docs = [ doc "one.md" [ "# Alpha" ]; doc "source.md" [ "[[Alpha#Sub]]" ] ]
        )
        |> Folder.withDoc (doc "two.md" [ "# Alpha"; "## Sub"; "# Beta" ])
        |> Folder.withDoc (doc "two.md" [ "# Beta"; "## Sub"; "# Alpha" ])
        |> Folder.withoutDoc (doc "one.md" []).Id
        |> Option.get
        |> assertMatchesCleanConstruction

    [<Fact>]
    let changingTitlePreservesUnresolvedLocalReferences () =
        mkFolder [ doc "one.md" [ "## Sub"; "[missing]" ] ]
        |> Folder.withDoc (doc "one.md" [ "# Alpha"; "[missing]" ])
        |> assertMatchesCleanConstruction
