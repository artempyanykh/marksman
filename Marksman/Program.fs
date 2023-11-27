﻿module Marksman.Program

open System
open System.Diagnostics
open System.Threading
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Logging
open Serilog

module MS = Marksman.Server

open FSharp.SystemCommandLine

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
        loggerConfig
            .WriteTo
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


    logger.info (Log.setMessage "Starting Marksman LSP server")

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
        Input.Option([ "--verbose"; "-v" ], 2, "Set logging verbosity level")

    let waitForDebugger =
        Input.Option(
            [ "--wait-for-debugger" ],
            false,
            "Wait for debugger to attach before running the program"
        )

    let lspCommand =
        command "server" {
            description "Start LSP server on stdin/stdout"
            inputs (verbosity, waitForDebugger)
            setHandler startLSP
        }

    rootCommand args {
        description "Marksman is a language server for Markdown"
        setHandler (fun () -> startLSP (2, false))
        addCommand lspCommand
    }
