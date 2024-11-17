module Marksman.Config

open FSharpPlus
open Ionide.LanguageServerProtocol.Logging
open System.IO
open Tomlyn
open Tomlyn.Model

type LookupError =
    | NotFound of path: list<string>
    | WrongType of path: list<string> * value: obj * expectedType: System.Type
    | WrongValue of path: list<string> * value: obj * err: string

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
    | Error(NotFound _) -> Ok None
    | Error err -> Error err

let private getFromTableOpt<'R> table revSeenPath remPath : Result<option<'R>, LookupError> =
    getFromTable table revSeenPath remPath |> lookupAsOpt

type ComplWikiStyle =
    /// Document title's slug, e.g. "A B C" -> "a-b-c"
    | TitleSlug
    /// File name without an extension, e.g. "path/to/doc.md" -> "doc"
    | FileStem
    /// File path without an extension, e.g. "path/to/doc.md" -> "path/to/doc"
    | FilePathStem

module ComplWikiStyle =
    let ofString (input: string) : Result<ComplWikiStyle, string> =
        match input.ToLower() with
        | "title-slug" -> Ok TitleSlug
        | "file-stem" -> Ok FileStem
        | "file-path-stem" -> Ok FilePathStem
        | other -> Error $"Unknown ComplWikiStyle: {other}"

    let ofStringOpt input = Option.ofResult (ofString input)

type TextSync =
    | Full
    | Incremental

module TextSync =
    let ofString (input: string) : Result<TextSync, string> =
        match input.ToLower() with
        | "full" -> Ok Full
        | "incremental" -> Ok Incremental
        | other -> Error $"Unknown text sync setting: {other}"

    let ofStringOpt input = Option.ofResult (ofString input)

    let ord =
        function
        | Full -> 0
        | Incremental -> 1

/// Configuration knobs for the Marksman LSP.
///
/// Note: all config options are laid out flat to make working with the config
/// without lenses manageable.
type Config = {
    caTocEnable: option<bool>
    caCreateMissingFileEnable: option<bool>
    coreMarkdownFileExtensions: option<array<string>>
    coreTextSync: option<TextSync>
    coreIncrementalReferences: option<bool>
    coreParanoid: option<bool>
    complWikiStyle: option<ComplWikiStyle>
    complCandidates: option<int>
} with

    static member Default = {
        caTocEnable = Some true
        caCreateMissingFileEnable = Some true
        coreMarkdownFileExtensions = Some [| "md"; "markdown" |]
        coreTextSync = Some Full
        coreIncrementalReferences = Some false
        coreParanoid = Some false
        complWikiStyle = Some TitleSlug
        complCandidates = Some 50
    }

    static member Empty = {
        caTocEnable = None
        caCreateMissingFileEnable = None
        coreMarkdownFileExtensions = None
        coreTextSync = None
        coreIncrementalReferences = None
        coreParanoid = None
        complWikiStyle = None
        complCandidates = None
    }

    member this.CaTocEnable() =
        this.caTocEnable
        |> Option.orElse Config.Default.caTocEnable
        |> Option.get

    member this.CaCreateMissingFileEnable() =
        this.caCreateMissingFileEnable
        |> Option.orElse Config.Default.caCreateMissingFileEnable
        |> Option.get

    member this.CoreMarkdownFileExtensions() =
        this.coreMarkdownFileExtensions
        |> Option.orElse Config.Default.coreMarkdownFileExtensions
        |> Option.get

    member this.CoreTextSync() =
        this.coreTextSync
        |> Option.orElse Config.Default.coreTextSync
        |> Option.get

    member this.CoreIncrementalReferences() =
        this.coreIncrementalReferences
        |> Option.orElse Config.Default.coreIncrementalReferences
        |> Option.get

    member this.CoreParanoid() =
        this.coreParanoid
        |> Option.orElse Config.Default.coreParanoid
        |> Option.get

    member this.ComplWikiStyle() =
        this.complWikiStyle
        |> Option.orElse Config.Default.complWikiStyle
        |> Option.get

    member this.ComplCandidates() =
        this.complCandidates
        |> Option.orElse Config.Default.complCandidates
        |> Option.get

let private configOfTable (table: TomlTable) : LookupResult<Config> =
    monad {
        let! caTocEnable = getFromTableOpt<bool> table [] [ "code_action"; "toc"; "enable" ]

        let! caCreateMissingFileEnable =
            getFromTableOpt<bool> table [] [ "code_action"; "create_missing_file"; "enable" ]

        let! coreMarkdownFileExtensions =
            getFromTableOpt<array<string>> table [] [ "core"; "markdown"; "file_extensions" ]

        let! coreTextSync = getFromTableOpt<string> table [] [ "core"; "text_sync" ]
        let coreTextSync = coreTextSync |> Option.bind TextSync.ofStringOpt

        let! coreIncrementalReferences =
            getFromTableOpt<bool> table [] [ "core"; "incremental_references" ]

        let! coreParanoid = getFromTableOpt<bool> table [] [ "core"; "paranoid" ]

        let! complWikiStyle = getFromTableOpt<string> table [] [ "completion"; "wiki"; "style" ]

        let complWikiStyle =
            complWikiStyle |> Option.bind ComplWikiStyle.ofStringOpt

        // TOML parser represents numbers as int64, hence extract as int64 and
        // convert to int
        let complCandidatesPath = [ "completion"; "candidates" ]
        let! complCandidates = getFromTableOpt<int64> table [] complCandidatesPath

        let! complCandidates =
            match complCandidates with
            | None -> Ok None
            | Some v ->
                if v > 0 then
                    Ok(Some(int v))
                else
                    Error(WrongValue(complCandidatesPath, v, "expected a non-negative number"))


        {
            caTocEnable = caTocEnable
            caCreateMissingFileEnable = caCreateMissingFileEnable
            coreMarkdownFileExtensions = coreMarkdownFileExtensions
            coreTextSync = coreTextSync
            coreIncrementalReferences = coreIncrementalReferences
            coreParanoid = coreParanoid
            complWikiStyle = complWikiStyle
            complCandidates = complCandidates
        }
    }

module Config =
    let logger = LogProvider.getLoggerByName "Config"

    let merge hi low = {
        caTocEnable = hi.caTocEnable |> Option.orElse low.caTocEnable
        caCreateMissingFileEnable =
            hi.caCreateMissingFileEnable
            |> Option.orElse low.caCreateMissingFileEnable
        coreMarkdownFileExtensions =
            hi.coreMarkdownFileExtensions
            |> Option.orElse low.coreMarkdownFileExtensions
        coreTextSync = hi.coreTextSync |> Option.orElse low.coreTextSync
        coreIncrementalReferences =
            hi.coreIncrementalReferences
            |> Option.orElse low.coreIncrementalReferences
        coreParanoid = hi.coreParanoid |> Option.orElse low.coreParanoid
        complWikiStyle = hi.complWikiStyle |> Option.orElse low.complWikiStyle
        complCandidates = hi.complCandidates |> Option.orElse low.complCandidates
    }

    let mergeOpt hi low =
        match low with
        | None -> hi
        | Some low ->
            match hi with
            | None -> Some low
            | Some hi -> Some(merge hi low)

    let tryParse (content: string) =
        let mutable table, diag = null, null
        let ok = Toml.TryToModel(content, &table, &diag)

        if ok then
            logger.trace (Log.setMessage "Parsing as TOML was successful")

            match configOfTable table with
            | Ok parsed -> Some parsed
            | err ->
                logger.error (
                    Log.setMessage "Failed to parse configuration"
                    >> Log.addContext "error" err
                )

                None
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

    let orDefault configOpt = Option.defaultValue Config.Default configOpt

let defaultMarkdownExtensions =
    Config.Default.CoreMarkdownFileExtensions() |> Seq.ofArray
