module Marksman.TextTests

open Ionide.LanguageServerProtocol.Types
open Xunit

[<Fact>]
let lineMap_empty () =
    let lm = Text.mkLineMap ""
    Assert.Equal([ 0, 0 ], lm.Map)

[<Fact>]
let lineMap_finalNewLine () =
    let lm = Text.mkLineMap "\n"
    Assert.Equal([ 0, 1; 1, 1 ], lm.Map)

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
    Assert.Equal([ 0, 3; 3, 8; 8, 13; 13, 13 ], lm.Map)

[<Fact>]
let applyTextChange_insert_single () =
    let text = Text.mkText "!"

    let actual =
        Text.applyTextChange
            [| { Range = Some(Text.mkRange ((0, 1), (0, 1)))
                 RangeLength = Some 0
                 Text = " Holla!" } |]
            text

    let expected = "! Holla!"
    Assert.Equal(expected, actual.content)

[<Fact>]
let applyTextChange_insert_multiple () =
    let text = Text.mkText "!"

    let actual =
        Text.applyTextChange
            [| { Range = Some(Text.mkRange ((0, 1), (0, 1)))
                 RangeLength = Some 0
                 Text = " H" }
               { Range = Some(Text.mkRange ((0, 3), (0, 3)))
                 RangeLength = Some 0
                 Text = "i" } |]
            text

    let expected = "! Hi"
    Assert.Equal(expected, actual.content)

[<Fact>]
let applyTextChange_insert_on_empty () =
    let text = Text.mkText ""

    let actual =
        Text.applyTextChange
            [| { Range = Some(Text.mkRange ((0, 0), (0, 0)))
                 RangeLength = Some 0
                 Text = "H" } |]
            text

    let expected = "H"
    Assert.Equal(expected, actual.content)

[<Fact>]
let applyTextChange_insert_next_line () =
    let text = Text.mkText "A\n"

    let actual =
        Text.applyTextChange
            [| { Range = Some(Text.mkRange ((1, 0), (1, 0)))
                 RangeLength = Some 0
                 Text = "B" } |]
            text

    let expected = "A\nB"
    Assert.Equal(expected, actual.content)

[<Fact>]
let applyTextChange_replace_single () =
    let text = Text.mkText "Hello World!"

    let actual =
        Text.applyTextChange
            [| { Range = Some(Text.mkRange ((0, 0), (0, 6)))
                 RangeLength = Some 5
                 Text = "Bye" } |]
            text

    let expected = "Bye World!"
    Assert.Equal(expected, actual.content)
