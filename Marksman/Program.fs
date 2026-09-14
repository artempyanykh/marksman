module Marksman.Program

open System
open System.Diagnostics
open System.Threading
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Logging
open Serilog

module MS = Marksman.Server

open FSharp.SystemCommandLine
open Marksman.Check
open Marksman.Misc

let configureLogging (verbosity: int) : unit =
    let loggerConfig = LoggerConfiguration()

    let loggerConfig =
        match verbosity with
        | 0 -> loggerConfig.MinimumLevel.Error()
        | 1 -> loggerConfig.MinimumLevel.Warning()
        | 2 -> loggerConfig.MinimumLevel.Information()
        | 3 -> loggerConfig.MinimumLevel.Debug()
        | _ -> loggerConfig.MinimumLevel.Verbose()

    Log.Logger <-
        loggerConfig.WriteTo
            .Console(
                outputTemplate =
                    "[{Timestamp:HH:mm:ss} {Level:u3}] <{SourceContext}> {Message:lj}: {Properties:lj}{NewLine}{Exception}",
                standardErrorFromLevel = Events.LogEventLevel.Verbose
            )
            .Enrich.FromLogContext()
            .CreateLogger()

    ()

let startLSP (args: int * bool) : int =
    let verbosity, waitForDebugger = args

    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    configureLogging verbosity
    let logger = LogProvider.getLoggerByName "LSP Entry"

    if waitForDebugger && not Debugger.IsAttached then
        logger.warn (Log.setMessage "Waiting for debugger to attach...")

        while not Debugger.IsAttached do
            Thread.Sleep(1000)


    let version = getAssemblyVersion ()
    let os = System.Runtime.InteropServices.RuntimeInformation.OSDescription

    let arch =
        System.Runtime.InteropServices.RuntimeInformation.OSArchitecture

    logger.info (
        Log.setMessage "Starting Marksman LSP server"
        >> Log.addContext "version" version
        >> Log.addContext "os" os
        >> Log.addContext "arch" arch
    )

    let requestHandlings = Server.defaultRequestHandlings ()

    let result =
        Server.start
            requestHandlings
            input
            output
            MS.MarksmanClient
            (fun client -> new MS.MarksmanServer(client))
            Server.defaultRpc

    logger.trace (Log.setMessage "Stopped Marksman LSP server")

    int result

[<EntryPoint>]
let main args =
    let verbosity =
        Input.option "--verbose"
        |> Input.alias "-v"
        |> Input.defaultValue 2
        |> Input.desc "Set logging verbosity level"

    let waitForDebugger =
        Input.option "--wait-for-debugger"
        |> Input.defaultValue false
        |> Input.desc "Wait for debugger to attach before running the program"

    let lspCommand =
        command "server" {
            description "Start LSP server on stdin/stdout"
            inputs (verbosity, waitForDebugger)
            setAction startLSP
        }

    let checkPath =
        Input.argument "[PATH]"
        |> Input.defaultValue "."
        |> Input.desc "Workspace directory or markdown file to check (default: current directory)"

    let checkFormat =
        Input.option "--format"
        |> Input.defaultValue "text"
        |> Input.desc "Output format: 'text' (default) or 'json'"

    let checkRoot =
        Input.option "--root"
        |> Input.defaultValue ""
        |> Input.desc "Override workspace root directory"

    let runCheck (args: string * string * string) : int =
        let path, fmt, root = args

        let format =
            match fmt.ToLower() with
            | "json" -> OutputFormat.Json
            | _ -> OutputFormat.Text

        let rootOverride =
            if String.IsNullOrWhiteSpace(root) then None else Some root

        Check.check path rootOverride format

    let checkCommand =
        command "check" {
            description "Check workspace for broken links and other issues"
            inputs (checkPath, checkFormat, checkRoot)
            setAction runCheck
        }

    rootCommand args {
        description "Marksman is a language server for Markdown"
        setAction (fun () -> startLSP (2, false))
        addCommand lspCommand
        addCommand checkCommand
    }
