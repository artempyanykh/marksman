module Marksman.ConfigTests

open System.IO
open System.Reflection
open FSharpPlus.Data.Validation
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
let testParse_tocInclude () =
    let content =
        """
[code_action]
toc.include = [2, 3, 4]
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with caTocInclude = Some [| 2; 3; 4 |] }

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
let testParse_5 () =
    let content =
        """
[core]
incremental_references = true
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with coreIncrementalReferences = Some true }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_6 () =
    let content =
        """
[core]
paranoid = true
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with coreParanoid = Some true }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_7 () =
    let content =
        """
[completion]
candidates = 100
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with complCandidates = Some 100 }

    Assert.Equal(Some expected, actual)

[<Fact>]
let testParse_8 () =
    let content =
        """
[core]
markdown.glfm_heading_ids.enable = true
"""

    let actual = Config.tryParse content

    let expected = { Config.Empty with coreMarkdownGlfmHeadingIdsEnable = Some true }

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
let testParse_broken_4 () =
    let content =
        """
[completion]
candidates = "fifty"
"""

    let actual = Config.tryParse content
    Assert.Equal(None, actual)

[<Fact>]
let testParse_broken_5 () =
    let content =
        """
[completion]
candidates = -1
"""

    let actual = Config.tryParse content
    Assert.Equal(None, actual)

[<Fact>]
let testParse_broken_6 () =
    let content =
        """
[core]
markdown.glfm_heading_ids.enable = -1
"""

    let actual = Config.tryParse content
    Assert.Equal(None, actual)

[<Fact>]
let testParse_broken_tocInclude () =
    let content =
        """
[code_action]
toc.include = [1, -1]
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

[<Fact>]
let testDefault_titleVsCompletionStyle () =
    let content =
        """
[core]
title_from_heading = false
"""

    let actual =
        Config.tryParse content
        |> Option.defaultWith (fun () -> failwith "Expected a successful parse")

    Assert.False(actual.CoreTitleFromHeading())
    Assert.Equal(ComplWikiStyle.FileStem, actual.ComplWikiStyle())
