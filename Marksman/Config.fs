module Marksman.Config

open System.IO
open Tomlyn

type TocConfig() =
    member val Enable = true with get, set

type CodeActionConfig() =
    member val Toc = TocConfig() with get, set

type Config() =
    member val CodeAction = CodeActionConfig() with get, set

let tryParse (content: string) =
    let isOk, toml, _diag = Toml.TryToModel<Config>(content)

    if not isOk then None else Some toml

let read (filepath: string) =
    try
        let content = using (new StreamReader(filepath)) (fun f -> f.ReadToEnd())
        tryParse content
    with :? FileNotFoundException ->
        None
