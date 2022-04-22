module Marksman.ParserTests

open Xunit

[<Fact>]
let parse_empty () =
    let text = ""
    let document = Parser.scrapeDocument text
    Assert.Equal<Parser.Element>([], document)