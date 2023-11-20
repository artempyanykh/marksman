module Marksman.ConnTest

open Xunit

open Snapper
open Snapper.Attributes

open Marksman.Misc
open Marksman.Helpers
open Marksman.MMap
open Marksman.Doc
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

[<StoreSnapshotsPerClass>]
module ConnGraphTests =
    [<Fact>]
    let emptyGraph () =
        let conn = Conn.mk emptyOracle MMap.empty
        checkSnapshot conn

    [<Fact>]
    let initGraph () =
        let f = FakeFolder.Mk([ d1; d1_dup; d2; d3 ])
        let conn = Conn.mk (Folder.oracle f) (Folder.syms f)
        checkSnapshot conn

    [<Fact>]
    let removeDoc () =
        let f = FakeFolder.Mk([ d1; d1_dup; d2; d3 ])
        let conn = Conn.mk (Folder.oracle f) (Folder.syms f)

        let f = FakeFolder.Mk([ d1; d2; d3 ])

        let removed =
            Doc.syms d1_dup
            |> Seq.map (fun s -> Scope.Doc(Doc.id d1_dup), s)
            |> Set.ofSeq

        let conn = Conn.update (Folder.oracle f) Set.empty removed conn
        checkSnapshot conn

    [<Fact>]
    let addDoc () =
        let f = FakeFolder.Mk([ d1; d2; d3 ])
        let conn = Conn.mk (Folder.oracle f) (Folder.syms f)

        let dNA =
            FakeDoc.Mk(
                path = "docNA.md",
                contentLines =
                    [| "# Doc NA" //
                       "" |]
            )

        let f = FakeFolder.Mk([ d1; d2; d3; dNA ])

        let added =
            Doc.syms dNA
            |> Seq.map (fun s -> Scope.Doc(Doc.id dNA), s)
            |> Set.ofSeq

        let conn = Conn.update (Folder.oracle f) added Set.empty conn
        checkSnapshot conn

    [<Fact>]
    let addLinkDef () =
        let f = FakeFolder.Mk([ d1; d2; d3 ])
        let conn = Conn.mk (Folder.oracle f) (Folder.syms f)

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

        let f = FakeFolder.Mk([ d1; d2; d3Update ])

        let diff =
            Doc.symsDifference d3 d3Update
            |> Difference.map (fun s -> Scope.Doc(Doc.id d3), s)

        let conn = Conn.update (Folder.oracle f) diff.added diff.removed conn
        checkSnapshot conn

    [<Fact>]
    let removeHeading () =
        let f = FakeFolder.Mk([ d1; d2; d3 ])
        let conn = Conn.mk (Folder.oracle f) (Folder.syms f)

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

        let f = FakeFolder.Mk([ d1Update; d2; d3 ])

        let diff =
            Doc.symsDifference d1 d1Update
            |> Difference.map (fun s -> Scope.Doc(Doc.id d1), s)

        let conn = Conn.update (Folder.oracle f) diff.added diff.removed conn
        checkSnapshot conn
