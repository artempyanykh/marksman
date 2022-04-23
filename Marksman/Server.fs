module Marksman.Server

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open Misc

type MarksmanClient (notSender: ClientNotificationSender, reqSender: ClientRequestSender) =
    inherit LspClient ()

type MarksmanServer (client: MarksmanClient) =
    inherit LspServer()

    override this.Initialize(parameters: InitializeParams) : AsyncLspResult<InitializeResult> =
        let initResult = InitializeResult.Default
        AsyncLspResult.success initResult

    override this.Dispose() = ()