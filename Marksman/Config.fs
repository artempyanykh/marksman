module Marksman.Config

open System.IO
open Ionide.LanguageServerProtocol.Logging
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

let private lookupAsOpt =
    function
    | Ok found -> Ok(Some found)
    | Error (NotFound _) -> Ok None
    | Error err -> Error err

let private getFromTableOpt<'R> table revSeenPath remPath : Result<option<'R>, LookupError> =
    getFromTable table revSeenPath remPath |> lookupAsOpt

/// Configuration knobs for the Marksman LSP.
///
/// Note: all config options are laid out flat to make working with the config
/// without lenses manageable.
type Config =
    { caTocEnable: option<bool> }

    static member Default = { caTocEnable = Some true }

let private configOfTable (table: TomlTable) : LookupResult<Config> =
    monad {
        let! caTocEnable = getFromTableOpt<bool> table [] [ "code_action"; "toc"; "enable" ]

        { caTocEnable = caTocEnable }
    }

module Config =
    let logger = LogProvider.getLoggerByName "Config"

    let merge hi low = { caTocEnable = hi.caTocEnable |> Option.orElse low.caTocEnable }

    let tryParse (content: string) =
        let ok, table, _ = Toml.TryToModel(content)

        if ok then
            logger.trace (Log.setMessage "Parsing as TOML was successful")

            match configOfTable table with
            | Ok parsed -> Some parsed
            | _ -> None
        else
            logger.trace (Log.setMessage "Parsing as TOML failed")
            None

    let read (filepath: string) =
        try
            let content = using (new StreamReader(filepath)) (fun f -> f.ReadToEnd())
            tryParse content
        with :? FileNotFoundException ->
            None

    let private marksman = "marksman"

    let userConfigDir =
        Path.Join(
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData),
            marksman
        )

    let userConfigFile = Path.Join(userConfigDir, "config.toml")
