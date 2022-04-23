module Marksman.Program

    open System
    open Ionide.LanguageServerProtocol
    module MS = Marksman.Server

    [<EntryPoint>]
    let main args =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()
        let requestHandlings = Server.defaultRequestHandlings()

        let result = Server.start requestHandlings input output MS.MarksmanClient (fun client -> new MS.MarksmanServer(client))
        int result

