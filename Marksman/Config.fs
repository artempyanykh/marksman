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

module LookupResult =
    let collect (results: list<option<'R>>) : option<array<'R>> =
        let rec go (acc: list<'R>) (rest: list<option<'R>>) : option<list<'R>> =
            match rest with
            | [] -> Some acc
            | None :: _ -> None
            | Some r :: tail -> go (r :: acc) tail

        Option.map (List.rev >> Array.ofList) (go [] results)

let rec private tryCoerce<'R> (targetType: System.Type) (value: obj) : option<'R> =
    if targetType.IsInstanceOfType(value) then
        Some(value :?> 'R)
    else
        match value with
        | :? TomlArray as value when targetType.IsArray ->
            let elType = targetType.GetElementType()

            let coercedEls =
                seq { for el in value -> tryCoerce elType el } |> List.ofSeq

            match LookupResult.collect coercedEls with
            | None -> None
            | Some array ->
                // Apparently, one cannot simply cast an object[] with string elements to string[].
                // What works is creating an instance of an array with the right dynamic type and
                // then copying all elements to this new array. This feels wrong! There must be a
                // better way...
                let castedArray = System.Array.CreateInstance(elType, array.Length)
                Array.iteri (fun i v -> castedArray.SetValue(v, i)) array
                Some(box castedArray :?> 'R)
        | _ -> None


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
                match tryCoerce<'R> typeof<'R> value with
                | Some value -> Ok value
                | _ -> Error(WrongType(List.rev (last :: revContext), value, typeof<'R>))
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
    { caTocEnable: option<bool>
      coreMarkdownExtensions: option<array<string>> }

    static member Default =
        { caTocEnable = Some true
          coreMarkdownExtensions = Some [| "md"; "markdown" |] }

    static member Empty = { caTocEnable = None; coreMarkdownExtensions = None }

    member this.CaTocEnable() =
        this.caTocEnable
        |> Option.orElse Config.Default.caTocEnable
        |> Option.get

    member this.CoreMarkdownExtensions() =
        this.coreMarkdownExtensions
        |> Option.orElse Config.Default.coreMarkdownExtensions
        |> Option.get

let private configOfTable (table: TomlTable) : LookupResult<Config> =
    monad {
        let! caTocEnable = getFromTableOpt<bool> table [] [ "code_action"; "toc"; "enable" ]

        let! coreMarkdownExtensions =
            getFromTableOpt<array<string>> table [] [ "core"; "markdown"; "extensions" ]

        { caTocEnable = caTocEnable
          coreMarkdownExtensions = coreMarkdownExtensions }
    }

module Config =
    let logger = LogProvider.getLoggerByName "Config"

    let merge hi low =
        { caTocEnable = hi.caTocEnable |> Option.orElse low.caTocEnable
          coreMarkdownExtensions =
            hi.coreMarkdownExtensions |> Option.orElse low.coreMarkdownExtensions }

    let tryParse (content: string) =
        let mutable table, diag = null, null
        let ok = Toml.TryToModel(content, &table, &diag)

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
