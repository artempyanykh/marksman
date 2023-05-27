module Marsman.ServerTests

open Newtonsoft.Json.Linq
open Xunit

open Marksman
open Marksman.State

module InitOptionTests =
    [<Fact>]
    let extractEmpty () =
        let json = JToken.Parse("{}")
        Assert.Equal(InitOptions.empty, InitOptions.ofJson json)

        let json = JToken.Parse("[]")
        Assert.Equal(InitOptions.empty, InitOptions.ofJson json)

    [<Fact>]
    let extractCorrect () =
        let json = JToken.Parse("""{"preferredTextSyncKind": 1}""")
        Assert.Equal({ preferredTextSyncKind = Some Config.Full }, InitOptions.ofJson json)

        let json = JToken.Parse("""{"preferredTextSyncKind": 2}""")
        Assert.Equal({ preferredTextSyncKind = Some Config.Incremental }, InitOptions.ofJson json)

    [<Fact>]
    let extractMalformed () =
        let json = JToken.Parse("""{"preferredTextSyncKind": 42}""")
        Assert.Equal({ preferredTextSyncKind = None }, InitOptions.ofJson json)

        let json = JToken.Parse("""{"preferredTextSyncKind": "full"}""")
        Assert.Equal({ preferredTextSyncKind = None }, InitOptions.ofJson json)
