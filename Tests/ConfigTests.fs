module Marksman.ConfigTests

open Xunit

open Marksman.Config

[<Fact>]
let testParse_0 () =
    let content =
        """
"""

    let actual = Config.tryParse content

    let expected = { caTocEnable = None }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_1 () =
    let content =
        """
[code_action]
"""

    let actual = Config.tryParse content
    let expected = { caTocEnable = None }
    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_2 () =
    let content =
        """
[code_action]
toc.enable = false
"""

    let actual = Config.tryParse content

    let expected = { caTocEnable = Some false }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_broken_0 () =
    let content = """
blah
"""
    let actual = Config.tryParse content
    Assert.Equal(None, actual)
