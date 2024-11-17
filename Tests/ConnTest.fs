module Marksman.ConnTest

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
        contentLines =
            [| "# Doc 1" //
               ""
               "## D1.S1"
               ""
               "[[doc-2]]"
               "[[doc-NA]]"
               "" |]
    )

let d1_dup =
    FakeDoc.Mk(
        path = "d1_dup.md",
        contentLines =
            [| "# Doc 1" //
               ""
               "[[doc-2]]"
               "" |]
    )

let d2 =
    FakeDoc.Mk(
        path = "d2.md",
        contentLines =
            [| "# Doc 2" //
               "[[doc-1]]"
               "[[doc-1#d1s1]]"
               "[[doc-NA2]]"
               "" |]
    )

let d3 =
    FakeDoc.Mk(
        path = "d3.md",
        contentLines =
            [| "# Doc 3"
               ""
               "[link1]"
               "[link1][]"
               "[linkX]"
               ""
               "[link1]: /url1"
               "" |]
    )

let checkSnapshot (conn: Conn) = conn.CompactFormat().Lines().ShouldMatchSnapshot()

let emptyOracle =
    { resolveToScope = fun _ _ -> [||] //
      resolveInScope = fun _ _ -> [||] }

let incrConfig =
    { Config.Config.Default with coreIncrementalReferences = Some true }

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
                contentLines =
                    [| "# Doc NA" //
                       "" |]
            )

        let f2 = Folder.withDoc dNA f1
        checkSnapshot (Folder.conn f2)

    [<Fact>]
    let addLinkDef () =
        let f1 = mkFolder [ d1; d2; d3 ]

        let d3Update =
            FakeDoc.Mk(
                path = "d3.md",
                contentLines =
                    [| "# Doc 3"
                       ""
                       "[link1]"
                       "[link1][]"
                       "[linkX]"
                       ""
                       "[link1]: /url1"
                       "[linkX]: /url2"
                       "" |]
            )

        let f2 = Folder.withDoc d3Update f1
        checkSnapshot (Folder.conn f2)

    [<Fact>]
    let removeHeading () =
        let f1 = mkFolder [ d1; d2; d3 ]

        let d1Update =
            FakeDoc.Mk(
                path = "d1.md",
                contentLines =
                    [| "# Doc 1" //
                       ""
                       "[[doc-2]]"
                       "[[doc-NA]]"
                       "" |]
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

        let config =
            { incrConfig with
                complWikiStyle = Some Config.FilePathStem
                coreParanoid = Some true }

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
