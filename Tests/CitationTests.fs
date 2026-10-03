module Marksman.CitationTests

open System
open System.IO
open Xunit
open Snapper.Attributes
open Ionide.LanguageServerProtocol.Types
open Marksman.Config
open Marksman.Doc
open Marksman.Folder
open Marksman.Helpers
open Marksman.Misc
open Marksman.Text

let private textAtCursor (input: string) =
    let offset = input.IndexOf('|')
    let text = mkText (input.Remove(offset, 1))

    let precedingLines =
        input.Substring(0, offset).Replace("\r\n", "\n").Split('\n')

    let line = precedingLines.Length - 1
    let position = Position.Mk(line, precedingLines[line].Length)
    text, position

type private BibTeXFixture(files: list<string * string>) =
    let directory = Directory.CreateTempSubdirectory("marksman-citations-")
    let rootPath = directory.FullName
    let root = mkFolderId rootPath
    let paths = files |> List.map fst |> Array.ofList

    do
        for path, content in files do
            let filename = Path.Combine(rootPath, path)
            Directory.CreateDirectory(Path.GetDirectoryName(filename)) |> ignore
            File.WriteAllText(filename, content)

    member _.Root = rootPath

    member _.Paths =
        paths |> Array.map (fun path -> Path.Combine(rootPath, path))

    member _.Candidates(input: string, ?bibFiles: string[], ?singleFile: bool) =
        let text, position = textAtCursor input

        let config = {
            Config.Default with
                complBibFiles = Some(defaultArg bibFiles paths)
        }

        let settings = ParserSettings.OfConfig config
        let singleFile = defaultArg singleFile false
        let sourcePath = Path.Combine(rootPath, "notes/source.md")
        let sourceRoot = if singleFile then mkFolderId sourcePath else root
        let sourceId = mkDocId sourceRoot sourcePath
        let source = Doc.mk settings sourceId None text

        let folder =
            if singleFile then
                Folder.singleFile source (Some config)
            else
                Folder.multiFile "test" root [ source ] (Some config)

        let candidates =
            Compl.findCandidatesInDoc folder source position |> Array.ofSeq

        text, candidates

    interface IDisposable with
        member _.Dispose() = directory.Delete(true)

let private bibliography =
    "@online{jscoreapple, title={JavaScriptCore}}\n"
    + "@software{audiokit, title={AudioKit}}\n"
    + "@book{card1991information,}\n@article{miller1968response,}"

let private applyCompletion (text: Text, candidates: CompletionItem[]) =
    let candidate = Assert.Single candidates

    match candidate.TextEdit with
    | Some(First edit) ->
        let before, after = text.Cutout(edit.Range)
        before + edit.NewText + after
    | _ -> failwith "Expected a replacement text edit"

module Candidates =
    let private findCandidates (fixture: BibTeXFixture) input =
        let text, position = textAtCursor input
        let doc = FakeDoc.Mk(text.content)

        let config = { Config.Default with complBibFiles = Some fixture.Paths }

        let folder = FakeFolder.Mk([ doc ], config = config)

        let candidates =
            Compl.findCandidatesInDoc folder doc position |> Array.ofSeq

        text, candidates

    [<Theory>]
    [<InlineData("Use [@jsc|].", "Use [@jscoreapple].")>]
    [<InlineData("🔊 Use [@jsc|].", "🔊 Use [@jscoreapple].")>]
    [<InlineData("Use [@jsc|", "Use [@jscoreapple")>]
    [<InlineData("Use @jsc|.", "Use @jscoreapple.")>]
    [<InlineData("Use @aud|--the library.", "Use @audiokit--the library.")>]
    [<InlineData("Use @aud|...more", "Use @audiokit...more")>]
    [<InlineData("Use (@jsc|).", "Use (@jscoreapple).")>]
    [<InlineData("Use (-@jsc|).", "Use (-@jscoreapple).")>]
    [<InlineData("Use (@jsc|", "Use (@jscoreapple")>]
    [<InlineData("Use [-@jsc|].", "Use [-@jscoreapple].")>]
    [<InlineData("Use [@aud|,p.3].", "Use [@audiokit,p.3].")>]
    [<InlineData("Use [see @jsc|, p. 1].", "Use [see @jscoreapple, p. 1].")>]
    [<InlineData("Use [@jsc|oreapple].", "Use [@jscoreapple].")>]
    [<InlineData("Use [@jsc|; @audiokit].", "Use [@jscoreapple; @audiokit].")>]
    [<InlineData("Use [@card1991information;\n@mil|].",
                 "Use [@card1991information;\n@miller1968response].")>]
    [<InlineData("Use [@card1991information;\r\n@mil|].",
                 "Use [@card1991information;\r\n@miller1968response].")>]
    let replacesOnlyTheCitationKey (source: string, expected: string) =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let actual = findCandidates fixture source |> applyCompletion
        Assert.Equal(expected, actual)

    [<Fact>]
    let replacesOnlyTheSecondCitationKey () =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let completion = findCandidates fixture "Use [@jscoreapple; @aud|]."

        let expected = "Use [@jscoreapple; @audiokit]."
        Assert.Equal(expected, applyCompletion completion)

    [<Fact>]
    let offersKeysImmediatelyAfterAtSign () =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let _, candidates = findCandidates fixture "[@|]"

        let labels =
            candidates |> Array.map (fun item -> item.Label) |> Array.sort

        let expected = [|
            "audiokit"
            "card1991information"
            "jscoreapple"
            "miller1968response"
        |]

        Assert.Equal<string>(expected, labels)

    [<Theory>]
    [<InlineData("@|", 1, 1)>]
    [<InlineData("[@aud|]", 2, 5)>]
    [<InlineData("[@aud|iokit]", 2, 10)>]
    let setsCitationCompletionMetadata (source: string, start: int, end_: int) =
        use fixture = new BibTeXFixture([ "one.bib", "@book{audiokit,}" ])

        let _, candidates = findCandidates fixture source
        let range = Range.Mk(0, start, 0, end_)
        let edit = { Range = range; NewText = "audiokit" }

        let expected = {
            CompletionItem.Create("audiokit") with
                Kind = Some CompletionItemKind.Reference
                FilterText = Some "audiokit"
                TextEdit = Some(First edit)
        }

        Assert.Equal(expected, Assert.Single candidates)

    [<Theory>]
    [<InlineData("@|", "@audiokit")>]
    [<InlineData("[@|aud]", "[@audiokit]")>]
    [<InlineData("@audiokit @aud|", "@audiokit @audiokit")>]
    let completesKeysAtTextBoundaries (source: string, expected: string) =
        use fixture = new BibTeXFixture([ "one.bib", "@book{audiokit,}" ])

        Assert.Equal(expected, findCandidates fixture source |> applyCompletion)

    [<Theory>]
    [<InlineData("Élan", "Él")>]
    [<InlineData("δοκιμή", "δο")>]
    [<InlineData("Café", "Café")>]
    [<InlineData("a‿b", "a‿")>]
    [<InlineData("-audiokit", "-aud")>]
    [<InlineData("doe$2020", "doe$")>]
    [<InlineData("doe+2020", "doe+")>]
    [<InlineData("doe<2020", "doe<")>]
    [<InlineData("doe>2020", "doe>")>]
    [<InlineData("doe~2020", "doe~")>]
    let completesUnicodePunctuationAndSymbolKeys (key: string, input: string) =
        let content = $"@book{{{key},}}"
        use fixture = new BibTeXFixture([ "references.bib", content ])
        let completion = findCandidates fixture $"[@{input}|]"
        Assert.Equal($"[@{key}]", applyCompletion completion)

    [<Fact>]
    let ranksExactAndPrefixMatchesAheadOfLooseMatches () =
        let content = "@book{s-m-i-t-h,}\n@book{smith2020,}\n@book{Smith,}"
        use fixture = new BibTeXFixture([ "references.bib", content ])
        let _, candidates = findCandidates fixture "[@smith|]"
        let labels = candidates |> Array.map (fun item -> item.Label)
        let expected = [| "Smith"; "smith2020"; "s-m-i-t-h" |]
        Assert.Equal<string>(expected, labels)

    [<Fact>]
    let matchesAsciiKeysRegardlessOfCurrentCulture () =
        let original = Globalization.CultureInfo.CurrentCulture

        try
            let culture = Globalization.CultureInfo("tr-TR")
            Globalization.CultureInfo.CurrentCulture <- culture

            use fixture =
                new BibTeXFixture([ "references.bib", "@book{Information,}" ])

            let completion = findCandidates fixture "[@inf|]"
            Assert.Equal("[@Information]", applyCompletion completion)
        finally
            Globalization.CultureInfo.CurrentCulture <- original

    [<Fact>]
    let matchesKeysWithoutChangingTheirCaseOrPunctuation () =
        let content = "@book{Smith:2020_A-b.c,}"
        use fixture = new BibTeXFixture([ "references.bib", content ])
        let completion = findCandidates fixture "[@smith:2020_a-b.c|]"
        Assert.Equal("[@Smith:2020_A-b.c]", applyCompletion completion)

    [<Theory>]
    [<InlineData("[@Smith:|]")>]
    [<InlineData("[@Smith:2020_A-|]")>]
    [<InlineData("[@Smith:2020_A-b.|]")>]
    [<InlineData("[@Smith:2020_A-b.c|]")>]
    [<InlineData("[@Smith|:2020_A-b.c]")>]
    [<InlineData("[@Smith:|2020_A-b.c]")>]
    let completesPrefixesEndingInKeyPunctuation (source: string) =
        let content = "@book{Smith:2020_A-b.c,}"
        use fixture = new BibTeXFixture([ "references.bib", content ])
        let completion = findCandidates fixture source
        Assert.Equal("[@Smith:2020_A-b.c]", applyCompletion completion)

    [<Fact>]
    let completesCitationsInProseAfterCodeAndFrontMatter () =
        let source =
            "---\nauthor: @audiokit\n---\n\n"
            + "```\n@audiokit\n```\n\nUse `code` [@jsc|]."

        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let expected = source.Replace("jsc|", "jscoreapple")
        Assert.Equal(expected, findCandidates fixture source |> applyCompletion)

    [<Fact>]
    let doesNotOfferUnmatchedKeys () =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let _, candidates = findCandidates fixture "[@missing|]"
        Assert.Empty(candidates)

[<StoreSnapshotsPerClass>]
module ExistingCompletions =
    let private checkSnapshot source =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let text, position = textAtCursor source
        let doc = FakeDoc.Mk(text.content, path = "notes/source.md")

        let target =
            FakeDoc.Mk("# Target\n\n## Section\n\n#tagged", path = "target.md")

        let config = { Config.Default with complBibFiles = Some fixture.Paths }
        let folder = FakeFolder.Mk([ doc; target ], config = config)

        Compl.findCandidatesInDoc folder doc position
        |> Array.ofSeq
        |> ComplTests.Candidates.checkSnapshot

    [<Fact>]
    let wikiTarget () = checkSnapshot "[[ta|]]"

    [<Fact>]
    let wikiHeadingInOtherDoc () = checkSnapshot "[[target#Sec|]]"

    [<Fact>]
    let wikiHeadingInSourceDoc () = checkSnapshot "[[#Sec|]]\n\n## Section"

    [<Fact>]
    let inlineTarget () = checkSnapshot "[link](../tar|)"

    [<Fact>]
    let inlineHeading () = checkSnapshot "[link](../target.md#Sec|)"

    [<Fact>]
    let shortcutReference () =
        let source = "[re|]\n\n[reference]: https://example.com"
        checkSnapshot source

    [<Fact>]
    let fullReference () =
        let source = "[link][re|]\n\n[reference]: https://example.com"
        checkSnapshot source

    [<Fact>]
    let referenceLabelStartingWithAtSign () =
        checkSnapshot "[@re|]\n\n[@reference]: https://example.com"

    [<Fact>]
    let tag () = checkSnapshot "#ta|"

module Bibliographies =

    [<Fact>]
    let mergesBibliographiesWithoutDuplicateKeys () =
        let files = [
            "one.bib", "@book{first,}\n@book{shared,}"
            "two.bib", "@book{shared,}\n@book{second,}"
        ]

        use fixture = new BibTeXFixture(files)
        let _, candidates = fixture.Candidates "[@|]"

        let labels =
            candidates |> Array.map (fun item -> item.Label) |> Array.sort

        let expected = [| "first"; "second"; "shared" |]
        Assert.Equal<string>(expected, labels)

    [<Fact>]
    let supportsAbsoluteBibliographyPaths () =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let paths = [| Path.Combine(fixture.Root, "references.bib") |]
        let completion = fixture.Candidates("[@aud|]", bibFiles = paths)
        Assert.Equal("[@audiokit]", applyCompletion completion)

    [<Fact>]
    let resolvesRelativeBibliographyPathsInSingleFileMode () =
        let files = [ "notes/references.bib", bibliography ]
        use fixture = new BibTeXFixture(files)
        let paths = [| "references.bib" |]

        let completion =
            fixture.Candidates("[@aud|]", bibFiles = paths, singleFile = true)

        Assert.Equal("[@audiokit]", applyCompletion completion)

    [<Fact>]
    let supportsAbsoluteBibliographyPathsInSingleFileMode () =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let paths = [| Path.Combine(fixture.Root, "references.bib") |]

        let completion =
            fixture.Candidates("[@aud|]", bibFiles = paths, singleFile = true)

        Assert.Equal("[@audiokit]", applyCompletion completion)

    [<Fact>]
    let missingBibliographyDoesNotHideOtherKeys () =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let paths = [| "missing.bib"; "references.bib" |]
        let completion = fixture.Candidates("[@aud|]", bibFiles = paths)
        Assert.Equal("[@audiokit]", applyCompletion completion)

    [<Theory>]
    [<InlineData(".")>]
    [<InlineData("invalid\u0000.bib")>]
    let unreadableBibliographyDoesNotHideOtherKeys (path: string) =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let paths = [| path; "references.bib" |]
        let completion = fixture.Candidates("[@aud|]", bibFiles = paths)
        Assert.Equal("[@audiokit]", applyCompletion completion)

    [<Fact>]
    let readsSavedBibliographyChanges () =
        use fixture = new BibTeXFixture([ "references.bib", "@book{oldKey,}" ])
        let _, before = fixture.Candidates "[@|]"
        Assert.Equal("oldKey", (Assert.Single before).Label)

        let path = Path.Combine(fixture.Root, "references.bib")
        File.WriteAllText(path, "@book{newKey,}")
        let _, after = fixture.Candidates "[@|]"
        Assert.Equal("newKey", (Assert.Single after).Label)

    [<Fact>]
    let stopsOfferingKeysFromDeletedBibliographies () =
        use fixture = new BibTeXFixture([ "references.bib", "@book{oldKey,}" ])
        let _, before = fixture.Candidates "[@|]"
        Assert.Equal("oldKey", (Assert.Single before).Label)

        File.Delete(Path.Combine(fixture.Root, "references.bib"))
        let _, after = fixture.Candidates "[@|]"
        Assert.Empty(after)

    [<Fact>]
    let readsBibliographiesRecreatedAfterDeletion () =
        use fixture = new BibTeXFixture([ "references.bib", "@book{oldKey,}" ])
        let _, before = fixture.Candidates "[@|]"
        Assert.Equal("oldKey", (Assert.Single before).Label)

        let path = Path.Combine(fixture.Root, "references.bib")
        File.Delete(path)
        let _, deleted = fixture.Candidates "[@|]"
        Assert.Empty(deleted)

        File.WriteAllText(path, "@book{newKey,}")
        let _, recreated = fixture.Candidates "[@|]"
        Assert.Equal("newKey", (Assert.Single recreated).Label)

    [<Fact>]
    let doesNotOfferCitationKeysWhenDisabled () =
        use fixture = new BibTeXFixture([ "references.bib", bibliography ])
        let _, candidates = fixture.Candidates("@aud|", bibFiles = [||])
        Assert.Empty(candidates)
