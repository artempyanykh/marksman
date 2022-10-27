module Marksman.ConfigTests

open Xunit

[<Fact>]
let testParse () =
    let content =
        """
[code_action]
toc.enable = false
"""

    let actual = Config.tryParse content
    let expected = Config.Config()
    expected.CodeAction.Toc.Enable <- false
    Assert.Equal(Some expected, actual)
