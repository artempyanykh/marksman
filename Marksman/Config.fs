module Marksman.Config

open System.IO
open Tomlyn
open Tomlyn.Model

open FSharpPlus.GenericBuilders

type LookupError =
    | NotFound of path: list<string>
    | WrongType of path: list<string> * value: obj * expectedType: System.Type

type LookupResult<'R> = Result<'R, LookupError>

let private getFromTable<'R>
    (table: TomlTable)
    (revContext: list<string>)
    (path: list<string>)
    : LookupResult<'R> =
    let rec go (table: TomlTable) revContext path =
        match path with
        | [] -> failwith "getFromTable unreachable"
        | [ last ] ->
            match table.TryGetValue(last) with
            | true, value ->
                match value with
                | :? 'T as value -> Ok value
                | _ -> Error(WrongType(List.rev (last :: revContext), value, typeof<'T>))
            | false, _ -> Error(NotFound(List.rev (last :: revContext)))
        | next :: tail ->
            match table.TryGetValue(next) with
            | true, value ->
                match value with
                | :? TomlTable as nextTable -> go nextTable (next :: revContext) tail
                | _ -> Error(WrongType(List.rev (next :: revContext), value, typeof<TomlTable>))
            | false, _ -> Error(NotFound(List.rev (next :: revContext)))

    match path with
    | [] -> failwith "Cannot query a table with an empty path"
    | path -> go table revContext path

let lookupAsOpt =
    function
    | Ok found -> Ok(Some found)
    | Error (NotFound _) -> Ok None
    | Error err -> Error err

let getFromTableOpt<'R> table revSeenPath remPath : Result<option<'R>, LookupError> =
    getFromTable table revSeenPath remPath |> lookupAsOpt

type TocConfig = { enable: option<bool> }
type CodeActionConfig = { toc: option<TocConfig> }
type Config = { codeAction: option<CodeActionConfig> }

let tocOfTable (context: list<string>) (table: TomlTable) : LookupResult<TocConfig> =
    monad' {
        let! enable = getFromTableOpt<bool> table context [ "enable" ]
        { enable = enable }
    }

let caOfTable (context: list<string>) (table: TomlTable) : LookupResult<CodeActionConfig> =
    monad' {
        let! toc =
            getFromTable<TomlTable> table context [ "toc" ]
            |> Result.bind (tocOfTable ("toc" :: context))
            |> lookupAsOpt

        { toc = toc }
    }

let configOfTable (context: list<string>) (table: TomlTable) : LookupResult<Config> =
    monad {
        let! ca =
            getFromTable<TomlTable> table [] [ "code_action" ]
            |> Result.bind (caOfTable ("code_action" :: context))
            |> lookupAsOpt

        { codeAction = ca }
    }

let tryParse (content: string) =
    let table = Toml.ToModel(content)

    match configOfTable [] table with
    | Ok parsed -> Some parsed
    | _ -> None

let read (filepath: string) =
    try
        let content = using (new StreamReader(filepath)) (fun f -> f.ReadToEnd())
        tryParse content
    with :? FileNotFoundException ->
        None
