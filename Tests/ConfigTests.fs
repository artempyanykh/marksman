module Marksman.ConfigTests

open System.IO
open System.Reflection
open Xunit

open Marksman.Config

[<Fact>]
let testParse_0 () =
    let content =
        """
"""

    let actual = Config.tryParse content

    let expected = Config.Empty

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_1 () =
    let content =
        """
[code_action]
"""

    let actual = Config.tryParse content
    let expected = Config.Empty
    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_2 () =
    let content =
        """
[code_action]
toc.enable = false
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with caTocEnable = Some false }

    Assert.Equal(Some expected, actual)


[<Fact>]
let testParse_3 () =
    let content =
        """
[completion]
wiki.style = "file-stem"
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with complWikiStyle = Some FileStem }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_4 () =
    let content =
        """
[core]
text_sync = "incremental"
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with coreTextSync = Some Incremental }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_broken_0 () =
    let content =
        """
blah
"""

    let actual = Config.tryParse content
    Assert.Equal(None, actual)

[<Fact>]
let testParse_broken_1 () =
    let content =
        """
[core]
markdown.file_extensions = [1, 2]
"""

    let actual = Config.tryParse content
    Assert.Equal(None, actual)

[<Fact>]
let testParse_broken_2 () =
    let content =
        """
[core]
markdown.file_extensions = [["md"], "markdown"]
"""

    let actual = Config.tryParse content
    Assert.Equal(None, actual)

[<Fact>]
let testParse_broken_3 () =
    let content =
        """
[core]
markdown.file_extensions = [["md"], ["markdown"]]
"""

    let actual = Config.tryParse content
    Assert.Equal(None, actual)

[<Fact>]
let testDefault () =
    let content =
        Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream("default.marksman.toml")

    let content = using (new StreamReader(content)) (fun f -> f.ReadToEnd())
    let parsed = Config.tryParse content
    Assert.Equal(Some Config.Default, parsed)
