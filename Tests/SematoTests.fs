module Marksman.SematoTests

open Marksman.Misc
open Marksman.Helpers
open Marksman.Workspace
open Marksman.Semato
open Xunit

let folderPath = (dummyRootPath [ "folder" ]) |> RootPath.ofString

let nthToken (data: array<uint32>) n = data[n * 5 .. n * 5 + 4]

[<Fact>]
let testEncoding () =
    let docPath = dummyRootPath [ "folder"; "doc1.md" ] |> PathUri.ofString

    let content =
        """# Title
Start with [[a-wiki-link]]. Then a [ref-link].
[[wiki-at-sol]]
<blank>
End with [[wiki-link-no-eol]]"""

    let doc = Doc.mk docPath folderPath None (Text.mkText content)
    let data = Token.ofIndexEncoded (Doc.index doc)
    Assert.Equal(4 * 5, data.Length)

    Assert.Equal<uint32>([| 1u; 11u; 15u; 0u; 0u |], nthToken data 0)
    Assert.Equal<uint32>([| 0u; 25u; 8u; 1u; 0u |], nthToken data 1)
    Assert.Equal<uint32>([| 1u; 0u; 15u; 0u; 0u |], nthToken data 2)
    Assert.Equal<uint32>([| 2u; 9u; 20u; 0u; 0u |], nthToken data 3)
