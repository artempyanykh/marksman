module Marksman.SematoTests

open Xunit

open Marksman.Helpers
open Marksman.Semato
open Marksman.Doc

let folderPath = (dummyRootPath [ "folder" ]) |> mkFolderId

let nthToken (data: array<uint32>) n = data[n * 5 .. n * 5 + 4]

[<Fact>]
let testEncoding () =
    let docPath = dummyRootPath [ "folder"; "doc1.md" ] |> mkDocId folderPath

    let content =
        """# Title
Start with [[a-wiki-link]]. Then a [ref-link].
[[wiki-at-sol]]
<blank>
End with [[wiki-link-no-eol]] and #tag."""

    let doc =
        Doc.mk Config.ParserSettings.Default docPath None (Text.mkText content)

    let data = Token.ofIndexEncoded (Doc.index doc)
    Assert.Equal(5 * 5, data.Length)

    Assert.Equal<uint32>([| 1u; 13u; 11u; 0u; 0u |], nthToken data 0)
    Assert.Equal<uint32>([| 0u; 23u; 8u; 1u; 0u |], nthToken data 1)
    Assert.Equal<uint32>([| 1u; 2u; 11u; 0u; 0u |], nthToken data 2)
    Assert.Equal<uint32>([| 2u; 11u; 16u; 0u; 0u |], nthToken data 3)
    Assert.Equal<uint32>([| 0u; 23u; 4u; 2u; 0u |], nthToken data 4)
