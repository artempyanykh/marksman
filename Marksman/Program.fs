module Marksman.Program

open System
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

let startLSP (verbosity: int) : int =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    configureLogging verbosity

    let logger =
        LogProvider.getLoggerByName "LSP Entry"

    logger.info (Log.setMessage "Starting Marksman LSP server")

    let requestHandlings =
        Server.defaultRequestHandlings ()

    let result =
        Server.start requestHandlings input output MS.MarksmanClient (fun client -> new MS.MarksmanServer(client))
    
    logger.trace (Log.setMessage "Stopped Marksman LSP server")

    int result

[<EntryPoint>]
let main args =
    let verbosity =
        Input.Option([ "--verbose"; "-v" ], 2, "Set logging verbosity level")
        
    let lspCommand =
        command "server" {
            description "Start LSP server on stdin/stdout"
            inputs verbosity
            setHandler startLSP
        }
        
    rootCommand args {
        description "Marksman is a language server for Markdown"
        setHandler (fun () -> startLSP 2)
        addCommand lspCommand
    }
