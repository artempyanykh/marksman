module Marksman.TextTests

open Xunit

type R = System.Range

[<Fact>]
let lineMap_empty () =
    let lm = Text.mkLineMap ""
    Assert.Equal([ 0, 0 ], lm.Map)

[<Fact>]
let lineMap_finalNewLine () =
    let lm = Text.mkLineMap "\n"
    Assert.Equal([ 0, 1 ], lm.Map)

[<Fact>]
let lineMap_singleChar_ascii () =
    let lm = Text.mkLineMap "1"
    Assert.Equal([ 0, 1; 1, 1 ], lm.Map)

[<Fact>]
let lineMap_singleLine_ascii () =
    let lm = Text.mkLineMap "123456789"
    Assert.Equal([ 0, 9; 9, 9 ], lm.Map)

[<Fact>]
let lineMap_multiple_lines () =
    let lm = Text.mkLineMap "12\n345\r\n6789\n"
    Assert.Equal([ 0, 3; 3, 8; 8, 13 ], lm.Map)
