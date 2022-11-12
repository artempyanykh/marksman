module Marksman.ConfigTests

open Xunit

[<Fact>]
let testParse_0 () =
    let content =
        """
"""

    let actual = Config.tryParse content

    let expected = { Config.codeAction = None }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_1 () =
    let content =
        """
[code_action]
"""

    let actual = Config.tryParse content
    let expected = { Config.codeAction = Some { toc = None } }
    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_2 () =
    let content =
        """
[code_action]
toc.enable = false
"""

    let actual = Config.tryParse content

    let expected =
        { Config.codeAction = Some { toc = Some { enable = Some false } } }

    Assert.Equal(Some expected, actual)
