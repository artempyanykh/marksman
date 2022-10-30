module Marksman.Config

open System.IO
open Tomlyn

type TocConfig = { enable: bool }
type CodeActionConfig = { toc: TocConfig }
type Config = { codeAction: CodeActionConfig }

module Serde =
    type TocConfig() =
        member val Enable: bool = true with get, set
        member this.Build() = { enable = this.Enable }

    type CodeActionConfig() =
        member val Toc = TocConfig() with get, set
        member this.Build() = { toc = this.Toc.Build() }

    type Config() =
        member val CodeAction = CodeActionConfig() with get, set
        member this.Build() = { codeAction = this.CodeAction.Build() }


let tryParse (content: string) =
    let isOk, toml, _diag = Toml.TryToModel<Serde.Config>(content)

    if not isOk then None else Some(toml.Build())

let read (filepath: string) =
    try
        let content = using (new StreamReader(filepath)) (fun f -> f.ReadToEnd())
        tryParse content
    with :? FileNotFoundException ->
        None
