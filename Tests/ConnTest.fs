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

let checkSnapshot (conn: Conn) = conn.PP().Lines().ShouldMatchSnapshot()

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
