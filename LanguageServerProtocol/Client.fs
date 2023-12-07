namespace Ionide.LanguageServerProtocol

open Ionide.LanguageServerProtocol.Types

module private ClientUtil =
  /// Return the JSON-RPC "not implemented" error
  let notImplemented<'t> = async.Return LspResult.notImplemented<'t>

  /// Do nothing and ignore the notification
  let ignoreNotification = async.Return(())

open ClientUtil

[<Interface>]
type ILspClient =
  /// The show message notification is sent from a server to a client to ask the client to display
  /// a particular message in the user interface.
  abstract member WindowShowMessage: ShowMessageParams -> Async<unit>

  /// The show message request is sent from a server to a client to ask the client to display
  /// a particular message in the user interface. In addition to the show message notification the
  /// request allows to pass actions and to wait for an answer from the client.
  abstract member WindowShowMessageRequest: ShowMessageRequestParams -> AsyncLspResult<MessageActionItem option>


  /// The log message notification is sent from the server to the client to ask the client to log
  ///a particular message.
  abstract member WindowLogMessage: LogMessageParams -> Async<unit>

  /// The show document request is sent from a server to a client to ask the client to display a particular
  /// resource referenced by a URI in the user interface.
  abstract member WindowShowDocument: ShowDocumentParams -> AsyncLspResult<ShowDocumentResult>

  /// The telemetry notification is sent from the server to the client to ask the client to log
  /// a telemetry event.
  abstract member TelemetryEvent: Newtonsoft.Json.Linq.JToken -> Async<unit>

  /// The `client/registerCapability` request is sent from the server to the client to register for a new
  /// capability on the client side. Not all clients need to support dynamic capability registration.
  /// A client opts in via the dynamicRegistration property on the specific client capabilities. A client
  /// can even provide dynamic registration for capability A but not for capability B.
  abstract member ClientRegisterCapability: RegistrationParams -> AsyncLspResult<unit>

  /// The `client/unregisterCapability` request is sent from the server to the client to unregister a previously
  /// registered capability.
  abstract member ClientUnregisterCapability: UnregistrationParams -> AsyncLspResult<unit>


  /// Many tools support more than one root folder per workspace. Examples for this are VS Code’s multi-root
  /// support, Atom’s project folder support or Sublime’s project support. If a client workspace consists of
  /// multiple roots then a server typically needs to know about this. The protocol up to know assumes one root
  /// folder which is announce to the server by the rootUri property of the InitializeParams.
  /// If the client supports workspace folders and announces them via the corresponding workspaceFolders client
  /// capability the InitializeParams contain an additional property workspaceFolders with the configured
  /// workspace folders when the server starts.
  ///
  /// The workspace/workspaceFolders request is sent from the server to the client to fetch the current open
  /// list of workspace folders. Returns null in the response if only a single file is open in the tool.
  /// Returns an empty array if a workspace is open but no folders are configured.
  abstract member WorkspaceWorkspaceFolders: unit -> AsyncLspResult<WorkspaceFolder[] option>

  /// The workspace/configuration request is sent from the server to the client to fetch configuration
  /// settings from the client.
  ///
  /// The request can fetch n configuration settings in one roundtrip. The order of the returned configuration
  /// settings correspond to the order of the passed ConfigurationItems (e.g. the first item in the response
  /// is the result for the first configuration item in the params).
  abstract member WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken[]>


  abstract member WorkspaceApplyEdit: ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResponse>

  /// The workspace/semanticTokens/refresh request is sent from the server to the client.
  /// Servers can use it to ask clients to refresh the editors for which this server provides semantic tokens.
  /// As a result the client should ask the server to recompute the semantic tokens for these editors.
  /// This is useful if a server detects a project wide configuration change which requires a re-calculation
  /// of all semantic tokens. Note that the client still has the freedom to delay the re-calculation of
  /// the semantic tokens if for example an editor is currently not visible.
  abstract member WorkspaceSemanticTokensRefresh: unit -> Async<unit>


  /// The `workspace/inlayHint/refresh` request is sent from the server to the client.
  /// Servers can use it to ask clients to refresh the inlay hints currently shown in editors.
  /// As a result the client should ask the server to recompute the inlay hints for these editors.
  /// This is useful if a server detects a configuration change which requires a re-calculation
  /// of all inlay hints. Note that the client still has the freedom to delay the re-calculation of the inlay hints
  /// if for example an editor is currently not visible.
  abstract member WorkspaceInlayHintRefresh: unit -> Async<unit>


  /// The workspace/codeLens/refresh request is sent from the server to the client. Servers can use it to ask
  /// clients to refresh the code lenses currently shown in editors. As a result the client should ask the
  /// server to recompute the code lenses for these editors. This is useful if a server detects a
  /// configuration change which requires a re-calculation of all code lenses. Note that the client still has
  /// the freedom to delay the re-calculation of the code lenses if for example an editor is currently not
  /// visible.
  abstract member WorkspaceCodeLensRefresh: unit -> Async<unit>


  /// The workspace/inlineValue/refresh request is sent from the server to the client. Servers can use it to
  /// ask clients to refresh the inline values currently shown in editors. As a result the client should ask
  /// the server to recompute the inline values for these editors. This is useful if a server detects a
  /// configuration change which requires a re-calculation of all inline values. Note that the client still
  /// has the freedom to delay the re-calculation of the inline values if for example an editor is currently
  /// not visible.
  abstract member WorkspaceInlineValueRefresh: unit -> Async<unit>


  /// Diagnostics notification are sent from the server to the client to signal results of validation runs.
  ///
  /// Diagnostics are “owned” by the server so it is the server’s responsibility to clear them if necessary.
  /// The following rule is used for VS Code servers that generate diagnostics:
  ///
  /// * if a language is single file only (for example HTML) then diagnostics are cleared by the server when
  ///   the file is closed.
  /// * if a language has a project system (for example C#) diagnostics are not cleared when a file closes.
  ///   When a project is opened all diagnostics for all files are recomputed (or read from a cache).
  ///
  /// When a file changes it is the server’s responsibility to re-compute diagnostics and push them to the
  /// client. If the computed set is empty it has to push the empty array to clear former diagnostics.
  /// Newly pushed diagnostics always replace previously pushed diagnostics. There is no merging that happens
  /// on the client side.
  abstract member TextDocumentPublishDiagnostics: PublishDiagnosticsParams -> Async<unit>

  /// The workspace/diagnostic/refresh request is sent from the server to the client. Servers can use it to
  /// ask clients to refresh all needed document and workspace diagnostics. This is useful if a server detects
  /// a project wide configuration change which requires a re-calculation of all diagnostics.
  abstract member WorkspaceDiagnosticRefresh: unit -> Async<unit>

  /// The window/workDoneProgress/create request is sent from the server to the client to ask the client to create a work done progress.
  abstract member WorkDoneProgressCreate: ProgressToken -> AsyncLspResult<unit>

  /// The base protocol offers also support to report progress in a generic fashion.
  /// This mechanism can be used to report any kind of progress including work done progress
  /// (usually used to report progress in the user interface using a progress bar) and
  /// partial result progress to support streaming of results.
  abstract member Progress: ProgressToken * 'Progress -> Async<unit>

[<AbstractClass>]
type LspClient() =

  /// The show message notification is sent from a server to a client to ask the client to display
  /// a particular message in the user interface.
  abstract member WindowShowMessage: ShowMessageParams -> Async<unit>

  default __.WindowShowMessage(_) = ignoreNotification

  /// The show message request is sent from a server to a client to ask the client to display
  /// a particular message in the user interface. In addition to the show message notification the
  /// request allows to pass actions and to wait for an answer from the client.
  abstract member WindowShowMessageRequest: ShowMessageRequestParams -> AsyncLspResult<MessageActionItem option>

  default __.WindowShowMessageRequest(_) = notImplemented

  /// The log message notification is sent from the server to the client to ask the client to log
  ///a particular message.
  abstract member WindowLogMessage: LogMessageParams -> Async<unit>

  default __.WindowLogMessage(_) = ignoreNotification

  /// The show document request is sent from a server to a client to ask the client to display a particular
  /// resource referenced by a URI in the user interface.
  abstract member WindowShowDocument: ShowDocumentParams -> AsyncLspResult<ShowDocumentResult>

  default __.WindowShowDocument(_) = notImplemented

  /// The telemetry notification is sent from the server to the client to ask the client to log
  /// a telemetry event.
  abstract member TelemetryEvent: Newtonsoft.Json.Linq.JToken -> Async<unit>

  default __.TelemetryEvent(_) = ignoreNotification

  /// The `client/registerCapability` request is sent from the server to the client to register for a new
  /// capability on the client side. Not all clients need to support dynamic capability registration.
  /// A client opts in via the dynamicRegistration property on the specific client capabilities. A client
  /// can even provide dynamic registration for capability A but not for capability B.
  abstract member ClientRegisterCapability: RegistrationParams -> AsyncLspResult<unit>

  default __.ClientRegisterCapability(_) = notImplemented

  /// The `client/unregisterCapability` request is sent from the server to the client to unregister a previously
  /// registered capability.
  abstract member ClientUnregisterCapability: UnregistrationParams -> AsyncLspResult<unit>

  default __.ClientUnregisterCapability(_) = notImplemented

  /// Many tools support more than one root folder per workspace. Examples for this are VS Code’s multi-root
  /// support, Atom’s project folder support or Sublime’s project support. If a client workspace consists of
  /// multiple roots then a server typically needs to know about this. The protocol up to know assumes one root
  /// folder which is announce to the server by the rootUri property of the InitializeParams.
  /// If the client supports workspace folders and announces them via the corresponding workspaceFolders client
  /// capability the InitializeParams contain an additional property workspaceFolders with the configured
  /// workspace folders when the server starts.
  ///
  /// The workspace/workspaceFolders request is sent from the server to the client to fetch the current open
  /// list of workspace folders. Returns null in the response if only a single file is open in the tool.
  /// Returns an empty array if a workspace is open but no folders are configured.
  abstract member WorkspaceWorkspaceFolders: unit -> AsyncLspResult<WorkspaceFolder[] option>

  default __.WorkspaceWorkspaceFolders() = notImplemented

  /// The workspace/configuration request is sent from the server to the client to fetch configuration
  /// settings from the client.
  ///
  /// The request can fetch n configuration settings in one roundtrip. The order of the returned configuration
  /// settings correspond to the order of the passed ConfigurationItems (e.g. the first item in the response
  /// is the result for the first configuration item in the params).
  abstract member WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken[]>

  default __.WorkspaceConfiguration(_) = notImplemented

  abstract member WorkspaceApplyEdit: ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResponse>
  default __.WorkspaceApplyEdit(_) = notImplemented

  /// The workspace/semanticTokens/refresh request is sent from the server to the client.
  /// Servers can use it to ask clients to refresh the editors for which this server provides semantic tokens.
  /// As a result the client should ask the server to recompute the semantic tokens for these editors.
  /// This is useful if a server detects a project wide configuration change which requires a re-calculation
  /// of all semantic tokens. Note that the client still has the freedom to delay the re-calculation of
  /// the semantic tokens if for example an editor is currently not visible.
  abstract member WorkspaceSemanticTokensRefresh: unit -> Async<unit>

  default __.WorkspaceSemanticTokensRefresh() = ignoreNotification

  /// The `workspace/inlayHint/refresh` request is sent from the server to the client.
  /// Servers can use it to ask clients to refresh the inlay hints currently shown in editors.
  /// As a result the client should ask the server to recompute the inlay hints for these editors.
  /// This is useful if a server detects a configuration change which requires a re-calculation
  /// of all inlay hints. Note that the client still has the freedom to delay the re-calculation of the inlay hints
  /// if for example an editor is currently not visible.
  abstract member WorkspaceInlayHintRefresh: unit -> Async<unit>

  default __.WorkspaceInlayHintRefresh() = ignoreNotification

  /// The workspace/codeLens/refresh request is sent from the server to the client. Servers can use it to ask
  /// clients to refresh the code lenses currently shown in editors. As a result the client should ask the
  /// server to recompute the code lenses for these editors. This is useful if a server detects a
  /// configuration change which requires a re-calculation of all code lenses. Note that the client still has
  /// the freedom to delay the re-calculation of the code lenses if for example an editor is currently not
  /// visible.
  abstract member WorkspaceCodeLensRefresh: unit -> Async<unit>

  default __.WorkspaceCodeLensRefresh() = ignoreNotification

  /// The workspace/inlineValue/refresh request is sent from the server to the client. Servers can use it to
  /// ask clients to refresh the inline values currently shown in editors. As a result the client should ask
  /// the server to recompute the inline values for these editors. This is useful if a server detects a
  /// configuration change which requires a re-calculation of all inline values. Note that the client still
  /// has the freedom to delay the re-calculation of the inline values if for example an editor is currently
  /// not visible.
  abstract member WorkspaceInlineValueRefresh: unit -> Async<unit>

  default __.WorkspaceInlineValueRefresh() = ignoreNotification

  /// Diagnostics notification are sent from the server to the client to signal results of validation runs.
  ///
  /// Diagnostics are “owned” by the server so it is the server’s responsibility to clear them if necessary.
  /// The following rule is used for VS Code servers that generate diagnostics:
  ///
  /// * if a language is single file only (for example HTML) then diagnostics are cleared by the server when
  ///   the file is closed.
  /// * if a language has a project system (for example C#) diagnostics are not cleared when a file closes.
  ///   When a project is opened all diagnostics for all files are recomputed (or read from a cache).
  ///
  /// When a file changes it is the server’s responsibility to re-compute diagnostics and push them to the
  /// client. If the computed set is empty it has to push the empty array to clear former diagnostics.
  /// Newly pushed diagnostics always replace previously pushed diagnostics. There is no merging that happens
  /// on the client side.
  abstract member TextDocumentPublishDiagnostics: PublishDiagnosticsParams -> Async<unit>

  default __.TextDocumentPublishDiagnostics(_) = ignoreNotification

  /// The workspace/diagnostic/refresh request is sent from the server to the client. Servers can use it to
  /// ask clients to refresh all needed document and workspace diagnostics. This is useful if a server detects
  /// a project wide configuration change which requires a re-calculation of all diagnostics.
  abstract member WorkspaceDiagnosticRefresh: unit -> Async<unit>

  default __.WorkspaceDiagnosticRefresh() = ignoreNotification

  abstract member Progress: ProgressToken * 'Progress -> Async<unit>

  default __.Progress(_, _) = ignoreNotification

  /// The window/workDoneProgress/create request is sent from the server to the client to ask the client to create a work done progress.
  abstract member WorkDoneProgressCreate: ProgressToken -> AsyncLspResult<unit>
  default __.WorkDoneProgressCreate(_) = notImplemented

  interface ILspClient with
    member this.WindowShowMessage(p: ShowMessageParams) = this.WindowShowMessage(p)
    member this.WindowShowMessageRequest(p: ShowMessageRequestParams) = this.WindowShowMessageRequest(p)
    member this.WindowLogMessage(p: LogMessageParams) = this.WindowLogMessage(p)
    member this.WindowShowDocument(p: ShowDocumentParams) = this.WindowShowDocument(p)
    member this.TelemetryEvent(p: Newtonsoft.Json.Linq.JToken) = this.TelemetryEvent(p)
    member this.ClientRegisterCapability(p: RegistrationParams) = this.ClientRegisterCapability(p)
    member this.ClientUnregisterCapability(p: UnregistrationParams) = this.ClientUnregisterCapability(p)
    member this.WorkspaceWorkspaceFolders() = this.WorkspaceWorkspaceFolders()
    member this.WorkspaceConfiguration(p: ConfigurationParams) = this.WorkspaceConfiguration(p)
    member this.WorkspaceApplyEdit(p: ApplyWorkspaceEditParams) = this.WorkspaceApplyEdit(p)
    member this.WorkspaceSemanticTokensRefresh() = this.WorkspaceSemanticTokensRefresh()
    member this.WorkspaceInlayHintRefresh() = this.WorkspaceInlayHintRefresh()
    member this.WorkspaceCodeLensRefresh() = this.WorkspaceCodeLensRefresh()
    member this.WorkspaceInlineValueRefresh() = this.WorkspaceInlineValueRefresh()
    member this.TextDocumentPublishDiagnostics(p: PublishDiagnosticsParams) = this.TextDocumentPublishDiagnostics(p)
    member this.WorkspaceDiagnosticRefresh() = this.WorkspaceDiagnosticRefresh()
    member this.WorkDoneProgressCreate(token: ProgressToken) = this.WorkDoneProgressCreate(token)
    member this.Progress(token, data) = this.Progress(token, data)