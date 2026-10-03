module Marksman.BibTeXTests

open Xunit
open Marksman.BibTeX

[<Theory>]
[<InlineData("@article{jscoreapple, title={JavaScriptCore}}")>]
[<InlineData("@ARTICLE ( jscoreapple , title = \"JavaScriptCore\" )")>]
[<InlineData("@online\n{\njscoreapple,\n}")>]
[<InlineData("@custom{jscoreapple,}")>]
[<InlineData("@article{jscoreapple, title={unfinished")>]
[<InlineData("@article\u2003{\u00a0jscoreapple\t,}")>]
let readsEntryKeys (content: string) =
    Assert.Equal<CitationKey>([ CitationKey "jscoreapple" ], parseKeys content)

[<Theory>]
[<InlineData("")>]
[<InlineData("ordinary text")>]
[<InlineData("@string{journal = \"Journal\"}")>]
[<InlineData("@PREAMBLE{\"Text\"}")>]
[<InlineData("@comment{@article{fake, title={Ignored}}}")>]
[<InlineData("% @article{fake,}\n")>]
[<InlineData("@article{, title={No key}}")>]
[<InlineData("@article{missingComma}")>]
[<InlineData("@{key,}")>]
[<InlineData("@article")>]
[<InlineData("@article key,")>]
[<InlineData("@article1{key,}")>]
[<InlineData("@article_{key,}")>]
[<InlineData("@árticle{key,}")>]
[<InlineData("@article{two words,}")>]
[<InlineData("@article{key=value,}")>]
[<InlineData("@article{\"key\",}")>]
[<InlineData("@article{key#value,}")>]
[<InlineData("@article{key%value,}")>]
let ignoresNonEntries (content: string) = Assert.Empty(parseKeys content)

[<Theory>]
[<InlineData("{Title with @article{fake,} inside}")>]
[<InlineData("\"Title with @article{fake,} inside\"")>]
[<InlineData("{A quote \" inside braces}")>]
[<InlineData("\"Escaped quote \\\" and } inside a title\"")>]
[<InlineData("{Escaped braces \\{ and \\}}")>]
let skipsFieldContents (title: string) =
    let content =
        $"@article{{first, title={title}}}\n@book(second, title={{Book}})"

    let expected = [ CitationKey "first"; CitationKey "second" ]
    Assert.Equal<CitationKey>(expected, parseKeys content)

[<Theory>]
[<InlineData("Élan")>]
[<InlineData("Smith:2020_A-b.c")>]
[<InlineData("key/+?<>~&$!")>]
let readsKeysWithUnicodeAndPunctuation (key: string) =
    let content = $"@book{{{key},}}"
    Assert.Equal<CitationKey>([ CitationKey key ], parseKeys content)

[<Fact>]
let readsEntriesAfterMalformedHeaders () =
    let content = "@article1{ignored,}\n@book{valid,}"
    Assert.Equal<CitationKey>([ CitationKey "valid" ], parseKeys content)

[<Theory>]
[<InlineData("@comment{A lone \" and @book{fake,} inside.}")>]
[<InlineData("@comment{A literal % before the closing brace.}")>]
[<InlineData("@preamble{A lone \" inside.}")>]
let skipsBracedCommentsAndPreambles (prefix: string) =
    let content = prefix + "\n@book{second, title={Book}}"
    Assert.Equal<CitationKey>([ CitationKey "second" ], parseKeys content)

[<Theory>]
[<InlineData("% }")>]
[<InlineData("% \"")>]
[<InlineData("% @book{fake,}")>]
let ignoresCommentsBetweenFields (comment: string) =
    let content =
        $"@book{{first,\n{comment}\n"
        + "title={Contains @book{fake,} literal},\n}\n@book{second,}"

    let expected = [ CitationKey "first"; CitationKey "second" ]
    Assert.Equal<CitationKey>(expected, parseKeys content)

[<Theory>]
[<InlineData("{A literal % and @book{fake,} inside}")>]
[<InlineData("\"A literal % and @book{fake,} inside\"")>]
let preservesPercentSignsInFieldValues (title: string) =
    let content = $"@book{{first, title={title}}}\n@book{{second,}}"
    let expected = [ CitationKey "first"; CitationKey "second" ]
    Assert.Equal<CitationKey>(expected, parseKeys content)
