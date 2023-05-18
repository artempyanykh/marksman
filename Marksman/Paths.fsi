module Marksman.Paths

[<Sealed>]
type PathUri =
    interface System.IComparable
    member LocalPath: string
    member DocumentUri: string

module PathUri =
    val ofString: string -> PathUri

[<Sealed>]
type RootPath =
    interface System.IComparable

module RootPath =
    val ofPath: PathUri -> RootPath
    val ofString: string -> RootPath
    val path: RootPath -> PathUri
