namespace Ionide.LanguageServerProtocol

open Ionide.LanguageServerProtocol.Types

module private ServerUtil =
  /// Return the JSON-RPC "not implemented" error
  let notImplemented<'t> = async.Return LspResult.notImplemented<'t>

  /// Do nothing and ignore the notification
  let ignoreNotification = async.Return(())

open ServerUtil

[<Interface>]
type ILspServer =
  inherit System.IDisposable

  /// The initialize request is sent as the first request from the client to the server.
  /// The initialize request may only be sent once.
  abstract member Initialize: InitializeParams -> AsyncLspResult<InitializeResult>

  /// The initialized notification is sent from the client to the server after the client received the result
  /// of the initialize request but before the client is sending any other request or notification to the server.
  /// The server can use the initialized notification for example to dynamically register capabilities.
  /// The initialized notification may only be sent once.
  abstract member Initialized: InitializedParams -> Async<unit>

  /// The shutdown request is sent from the client to the server. It asks the server to shut down, but to not
  /// exit (otherwise the response might not be delivered correctly to the client). There is a separate exit
  /// notification that asks the server to exit.
  abstract member Shutdown: unit -> Async<unit>


  /// A notification to ask the server to exit its process.
  abstract member Exit: unit -> Async<unit>

  /// The hover request is sent from the client to the server to request hover information at a given text
  /// document position.
  abstract member TextDocumentHover: TextDocumentPositionParams -> AsyncLspResult<Hover option>


  /// The document open notification is sent from the client to the server to signal newly opened text
  /// documents.
  ///
  /// The document’s truth is now managed by the client and the server must not try to read the document’s
  /// truth using the document’s uri. Open in this sense means it is managed by the client. It doesn't
  /// necessarily mean that its content is presented in an editor. An open notification must not be sent
  /// more than once without a corresponding close notification send before. This means open and close
  /// notification must be balanced and the max open count for a particular textDocument is one.
  abstract member TextDocumentDidOpen: DidOpenTextDocumentParams -> Async<unit>


  /// The document change notification is sent from the client to the server to signal changes to a text document.
  abstract member TextDocumentDidChange: DidChangeTextDocumentParams -> Async<unit>

  /// The Completion request is sent from the client to the server to compute completion items at a given
  /// cursor position. Completion items are presented in the IntelliSense user interface.
  ///
  /// If computing full completion items is expensive, servers can additionally provide a handler for the
  /// completion item resolve request (‘completionItem/resolve’). This request is sent when a completion
  /// item is selected in the user interface. A typical use case is for example: the ‘textDocument/completion’
  /// request doesn’t fill in the documentation property for returned completion items since it is expensive
  /// to compute. When the item is selected in the user interface then a ‘completionItem/resolve’ request is
  /// sent with the selected completion item as a param. The returned completion item should have the
  /// documentation property filled in. The request can delay the computation of the detail and documentation
  /// properties. However, properties that are needed for the initial sorting and filtering, like sortText,
  /// filterText, insertText, and textEdit must be provided in the textDocument/completion request and must
  /// not be changed during resolve.
  abstract member TextDocumentCompletion: CompletionParams -> AsyncLspResult<CompletionList option>


  /// The request is sent from the client to the server to resolve additional information for a given
  /// completion item.
  abstract member CompletionItemResolve: CompletionItem -> AsyncLspResult<CompletionItem>


  /// The rename request is sent from the client to the server to perform a workspace-wide rename of a symbol.
  abstract member TextDocumentRename: RenameParams -> AsyncLspResult<WorkspaceEdit option>

  /// The prepare rename request is sent from the client to the server to setup and test the validity of a rename operation at a given location.
  /// If None is returned then it is deemed that a ‘textDocument/rename’ request is not valid at the given position.
  abstract member TextDocumentPrepareRename: PrepareRenameParams -> AsyncLspResult<PrepareRenameResult option>



  /// The goto definition request is sent from the client to the server to resolve the definition location of
  /// a symbol at a given text document position.
  abstract member TextDocumentDefinition: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>


  /// The references request is sent from the client to the server to resolve project-wide references for
  /// the symbol denoted by the given text document position.
  abstract member TextDocumentReferences: ReferenceParams -> AsyncLspResult<Location [] option>


  /// The document highlight request is sent from the client to the server to resolve a document highlights
  /// for a given text document position. For programming languages this usually highlights all references
  /// to the symbol scoped to this file.
  ///
  /// However we kept `textDocument/documentHighlight` and `textDocument/references` separate requests since
  /// the first one is allowed to be more fuzzy. Symbol matches usually have a DocumentHighlightKind of Read
  /// or Write whereas fuzzy or textual matches use Text as the kind.
  abstract member TextDocumentDocumentHighlight:
    TextDocumentPositionParams -> AsyncLspResult<DocumentHighlight [] option>


  /// The document links request is sent from the client to the server to request the location of links
  /// in a document.
  abstract member TextDocumentDocumentLink: DocumentLinkParams -> AsyncLspResult<DocumentLink [] option>


  /// The goto type definition request is sent from the client to the server to resolve the type definition
  /// location of a symbol at a given text document position.
  abstract member TextDocumentTypeDefinition: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>


  /// The goto implementation request is sent from the client to the server to resolve the implementation
  /// location of a symbol at a given text document position.
  abstract member TextDocumentImplementation: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>


  /// The code action request is sent from the client to the server to compute commands for a given text
  /// document and range. These commands are typically code fixes to either fix problems or to
  /// beautify/refactor code. The result of a textDocument/codeAction request is an array of Command literals
  /// which are typically presented in the user interface. When the command is selected the server should be
  /// contacted again (via the workspace/executeCommand) request to execute the command.
  abstract member TextDocumentCodeAction: CodeActionParams -> AsyncLspResult<TextDocumentCodeActionResult option>


  /// The code action request is sent from the client to the server to compute commands for a given text
  /// document and range. These commands are typically code fixes to either fix problems or to
  /// beautify/refactor code. The result of a textDocument/codeAction request is an array of Command literals
  /// which are typically presented in the user interface. When the command is selected the server should be
  /// contacted again (via the workspace/executeCommand) request to execute the command.
  abstract member CodeActionResolve: CodeAction -> AsyncLspResult<CodeAction option>


  /// The code lens request is sent from the client to the server to compute code lenses for a given
  /// text document.
  abstract member TextDocumentCodeLens: CodeLensParams -> AsyncLspResult<CodeLens [] option>


  /// The code lens resolve request is sent from the client to the server to resolve the command for
  /// a given code lens item.
  abstract member CodeLensResolve: CodeLens -> AsyncLspResult<CodeLens>


  /// The signature help request is sent from the client to the server to request signature information at
  /// a given cursor position.
  abstract member TextDocumentSignatureHelp: SignatureHelpParams -> AsyncLspResult<SignatureHelp option>


  /// The document link resolve request is sent from the client to the server to resolve the target of
  /// a given document link.
  abstract member DocumentLinkResolve: DocumentLink -> AsyncLspResult<DocumentLink>


  /// The document color request is sent from the client to the server to list all color references
  /// found in a given text document. Along with the range, a color value in RGB is returned.
  abstract member TextDocumentDocumentColor: DocumentColorParams -> AsyncLspResult<ColorInformation []>

  /// The color presentation request is sent from the client to the server to obtain a list of
  /// presentations for a color value at a given location. Clients can use the result to
  abstract member TextDocumentColorPresentation: ColorPresentationParams -> AsyncLspResult<ColorPresentation []>


  /// The document formatting request is sent from the client to the server to format a whole document.
  abstract member TextDocumentFormatting: DocumentFormattingParams -> AsyncLspResult<TextEdit [] option>

  /// The document range formatting request is sent from the client to the server to format a given
  /// range in a document.
  abstract member TextDocumentRangeFormatting: DocumentRangeFormattingParams -> AsyncLspResult<TextEdit [] option>


  /// The document on type formatting request is sent from the client to the server to format parts
  /// of the document during typing.
  abstract member TextDocumentOnTypeFormatting: DocumentOnTypeFormattingParams -> AsyncLspResult<TextEdit [] option>


  /// The document symbol request is sent from the client to the server to return a flat list of all symbols
  /// found in a given text document. Neither the symbol’s location range nor the symbol’s container name
  /// should be used to infer a hierarchy.
  abstract member TextDocumentDocumentSymbol:
    DocumentSymbolParams -> AsyncLspResult<U2<SymbolInformation [], DocumentSymbol []> option>


  /// The watched files notification is sent from the client to the server when the client detects changes
  /// to files watched by the language client. It is recommended that servers register for these file
  /// events using the registration mechanism. In former implementations clients pushed file events without
  /// the server actively asking for it.
  abstract member WorkspaceDidChangeWatchedFiles: DidChangeWatchedFilesParams -> Async<unit>


  /// The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server to inform
  /// the server about workspace folder configuration changes. The notification is sent by default if both
  /// *ServerCapabilities/workspace/workspaceFolders* and *ClientCapabilities/workapce/workspaceFolders* are
  /// true; or if the server has registered to receive this notification it first.
  abstract member WorkspaceDidChangeWorkspaceFolders: DidChangeWorkspaceFoldersParams -> Async<unit>


  /// A notification sent from the client to the server to signal the change of configuration settings.
  abstract member WorkspaceDidChangeConfiguration: DidChangeConfigurationParams -> Async<unit>

  /// The will create files request is sent from the client to the server before files are actually created
  /// as long as the creation is triggered from within the client either by a user action or by applying a
  /// workspace edit
  abstract member WorkspaceWillCreateFiles: CreateFilesParams -> AsyncLspResult<WorkspaceEdit option>


  /// The did create files notification is sent from the client to the server when files were created
  /// from within the client.
  abstract member WorkspaceDidCreateFiles: CreateFilesParams -> Async<unit>


  /// The will rename files request is sent from the client to the server before files are actually renamed
  /// as long as the rename is triggered from within the client either by a user action or by applying a
  /// workspace edit.
  abstract member WorkspaceWillRenameFiles: RenameFilesParams -> AsyncLspResult<WorkspaceEdit option>


  /// The did rename files notification is sent from the client to the server when files were renamed from
  /// within the client.
  abstract member WorkspaceDidRenameFiles: RenameFilesParams -> Async<unit>


  /// The will delete files request is sent from the client to the server before files are actually deleted
  /// as long as the deletion is triggered from within the client either by a user action or by applying a
  /// workspace edit.
  abstract member WorkspaceWillDeleteFiles: DeleteFilesParams -> AsyncLspResult<WorkspaceEdit option>


  /// The did delete files notification is sent from the client to the server when files were deleted from
  /// within the client.
  abstract member WorkspaceDidDeleteFiles: DeleteFilesParams -> Async<unit>


  /// The workspace symbol request is sent from the client to the server to list project-wide symbols matching
  /// the query string.
  abstract member WorkspaceSymbol: WorkspaceSymbolParams -> AsyncLspResult<SymbolInformation [] option>



  /// The `workspace/executeCommand` request is sent from the client to the server to trigger command execution
  /// on the server. In most cases the server creates a `WorkspaceEdit` structure and applies the changes to the
  /// workspace using the request `workspace/applyEdit` which is sent from the server to the client.
  abstract member WorkspaceExecuteCommand: ExecuteCommandParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken>


  /// The document will save notification is sent from the client to the server before the document is
  /// actually saved.
  abstract member TextDocumentWillSave: WillSaveTextDocumentParams -> Async<unit>


  /// The document will save request is sent from the client to the server before the document is actually saved.
  /// The request can return an array of TextEdits which will be applied to the text document before it is saved.
  /// Please note that clients might drop results if computing the text edits took too long or if a server
  /// constantly fails on this request. This is done to keep the save fast and reliable.
  abstract member TextDocumentWillSaveWaitUntil: WillSaveTextDocumentParams -> AsyncLspResult<TextEdit [] option>


  /// The document save notification is sent from the client to the server when the document was saved
  /// in the client.
  abstract member TextDocumentDidSave: DidSaveTextDocumentParams -> Async<unit>


  /// The document close notification is sent from the client to the server when the document got closed in the
  /// client. The document’s truth now exists where the document’s uri points to (e.g. if the document’s uri is
  /// a file uri the truth now exists on disk). As with the open notification the close notification is about
  /// managing the document’s content. Receiving a close notification doesn't mean that the document was open in
  /// an editor before. A close notification requires a previous open notification to be sent.
  abstract member TextDocumentDidClose: DidCloseTextDocumentParams -> Async<unit>


  /// The folding range request is sent from the client to the server to return all folding ranges found in a given text document.
  abstract member TextDocumentFoldingRange: FoldingRangeParams -> AsyncLspResult<FoldingRange list option>

  /// The selection range request is sent from the client to the server to return suggested selection ranges at an array of given positions.
  /// A selection range is a range around the cursor position which the user might be interested in selecting.
  abstract member TextDocumentSelectionRange: SelectionRangeParams -> AsyncLspResult<SelectionRange list option>

  abstract member TextDocumentSemanticTokensFull: SemanticTokensParams -> AsyncLspResult<SemanticTokens option>

  abstract member TextDocumentSemanticTokensFullDelta:
    SemanticTokensDeltaParams -> AsyncLspResult<U2<SemanticTokens, SemanticTokensDelta> option>

  abstract member TextDocumentSemanticTokensRange: SemanticTokensRangeParams -> AsyncLspResult<SemanticTokens option>

  /// The inlay hints request is sent from the client to the server to compute inlay hints for a given [text document, range] tuple
  ///  that may be rendered in the editor in place with other text.
  abstract member TextDocumentInlayHint: InlayHintParams -> AsyncLspResult<InlayHint [] option>

  /// The request is sent from the client to the server to resolve additional information for a given inlay hint.
  /// This is usually used to compute the `tooltip`, `location` or `command` properties of a inlay hint’s label part
  /// to avoid its unnecessary computation during the `textDocument/inlayHint` request.
  ///
  /// Consider the clients announces the `label.location` property as a property that can be resolved lazy using the client capability
  /// ```typescript
  /// textDocument.inlayHint.resolveSupport = { properties: ['label.location'] };
  /// ```
  /// then an inlay hint with a label part without a location needs to be resolved using the `inlayHint/resolve` request before it can be used.
  abstract member InlayHintResolve: InlayHint -> AsyncLspResult<InlayHint>

[<AbstractClass>]
type LspServer() =
  abstract member Dispose: unit -> unit

  /// The initialize request is sent as the first request from the client to the server.
  /// The initialize request may only be sent once.
  abstract member Initialize: InitializeParams -> AsyncLspResult<InitializeResult>

  default __.Initialize(_) = notImplemented

  /// The initialized notification is sent from the client to the server after the client received the result
  /// of the initialize request but before the client is sending any other request or notification to the server.
  /// The server can use the initialized notification for example to dynamically register capabilities.
  /// The initialized notification may only be sent once.
  abstract member Initialized: InitializedParams -> Async<unit>

  default __.Initialized(_) = ignoreNotification

  /// The shutdown request is sent from the client to the server. It asks the server to shut down, but to not
  /// exit (otherwise the response might not be delivered correctly to the client). There is a separate exit
  /// notification that asks the server to exit.
  abstract member Shutdown: unit -> Async<unit>

  default __.Shutdown() = ignoreNotification

  /// A notification to ask the server to exit its process.
  abstract member Exit: unit -> Async<unit>
  default __.Exit() = ignoreNotification

  /// The hover request is sent from the client to the server to request hover information at a given text
  /// document position.
  abstract member TextDocumentHover: TextDocumentPositionParams -> AsyncLspResult<Hover option>

  default __.TextDocumentHover(_) = notImplemented

  /// The document open notification is sent from the client to the server to signal newly opened text
  /// documents.
  ///
  /// The document’s truth is now managed by the client and the server must not try to read the document’s
  /// truth using the document’s uri. Open in this sense means it is managed by the client. It doesn't
  /// necessarily mean that its content is presented in an editor. An open notification must not be sent
  /// more than once without a corresponding close notification send before. This means open and close
  /// notification must be balanced and the max open count for a particular textDocument is one.
  abstract member TextDocumentDidOpen: DidOpenTextDocumentParams -> Async<unit>

  default __.TextDocumentDidOpen(_) = ignoreNotification

  /// The document change notification is sent from the client to the server to signal changes to a text document.
  abstract member TextDocumentDidChange: DidChangeTextDocumentParams -> Async<unit>
  default __.TextDocumentDidChange(_) = ignoreNotification

  /// The Completion request is sent from the client to the server to compute completion items at a given
  /// cursor position. Completion items are presented in the IntelliSense user interface.
  ///
  /// If computing full completion items is expensive, servers can additionally provide a handler for the
  /// completion item resolve request (‘completionItem/resolve’). This request is sent when a completion
  /// item is selected in the user interface. A typical use case is for example: the ‘textDocument/completion’
  /// request doesn’t fill in the documentation property for returned completion items since it is expensive
  /// to compute. When the item is selected in the user interface then a ‘completionItem/resolve’ request is
  /// sent with the selected completion item as a param. The returned completion item should have the
  /// documentation property filled in. The request can delay the computation of the detail and documentation
  /// properties. However, properties that are needed for the initial sorting and filtering, like sortText,
  /// filterText, insertText, and textEdit must be provided in the textDocument/completion request and must
  /// not be changed during resolve.
  abstract member TextDocumentCompletion: CompletionParams -> AsyncLspResult<CompletionList option>

  default __.TextDocumentCompletion(_) = notImplemented

  /// The request is sent from the client to the server to resolve additional information for a given
  /// completion item.
  abstract member CompletionItemResolve: CompletionItem -> AsyncLspResult<CompletionItem>

  default __.CompletionItemResolve(_) = notImplemented

  /// The rename request is sent from the client to the server to perform a workspace-wide rename of a symbol.
  abstract member TextDocumentRename: RenameParams -> AsyncLspResult<WorkspaceEdit option>
  default __.TextDocumentRename(_) = notImplemented

  /// The prepare rename request is sent from the client to the server to setup and test the validity of a rename operation at a given location.
  /// If None is returned then it is deemed that a ‘textDocument/rename’ request is not valid at the given position.
  abstract member TextDocumentPrepareRename: PrepareRenameParams -> AsyncLspResult<PrepareRenameResult option>

  default __.TextDocumentPrepareRename(_) =
    AsyncLspResult.success (Some(PrepareRenameResult.Default { DefaultBehavior = true }))

  /// The goto definition request is sent from the client to the server to resolve the definition location of
  /// a symbol at a given text document position.
  abstract member TextDocumentDefinition: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>

  default __.TextDocumentDefinition(_) = notImplemented

  /// The references request is sent from the client to the server to resolve project-wide references for
  /// the symbol denoted by the given text document position.
  abstract member TextDocumentReferences: ReferenceParams -> AsyncLspResult<Location [] option>

  default __.TextDocumentReferences(_) = notImplemented

  /// The document highlight request is sent from the client to the server to resolve a document highlights
  /// for a given text document position. For programming languages this usually highlights all references
  /// to the symbol scoped to this file.
  ///
  /// However we kept `textDocument/documentHighlight` and `textDocument/references` separate requests since
  /// the first one is allowed to be more fuzzy. Symbol matches usually have a DocumentHighlightKind of Read
  /// or Write whereas fuzzy or textual matches use Text as the kind.
  abstract member TextDocumentDocumentHighlight:
    TextDocumentPositionParams -> AsyncLspResult<DocumentHighlight [] option>

  default __.TextDocumentDocumentHighlight(_) = notImplemented

  /// The document links request is sent from the client to the server to request the location of links
  /// in a document.
  abstract member TextDocumentDocumentLink: DocumentLinkParams -> AsyncLspResult<DocumentLink [] option>

  default __.TextDocumentDocumentLink(_) = notImplemented

  /// The goto type definition request is sent from the client to the server to resolve the type definition
  /// location of a symbol at a given text document position.
  abstract member TextDocumentTypeDefinition: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>

  default __.TextDocumentTypeDefinition(_) = notImplemented

  /// The goto implementation request is sent from the client to the server to resolve the implementation
  /// location of a symbol at a given text document position.
  abstract member TextDocumentImplementation: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>

  default __.TextDocumentImplementation(_) = notImplemented

  /// The code action request is sent from the client to the server to compute commands for a given text
  /// document and range. These commands are typically code fixes to either fix problems or to
  /// beautify/refactor code. The result of a textDocument/codeAction request is an array of Command literals
  /// which are typically presented in the user interface. When the command is selected the server should be
  /// contacted again (via the workspace/executeCommand) request to execute the command.
  abstract member TextDocumentCodeAction: CodeActionParams -> AsyncLspResult<TextDocumentCodeActionResult option>

  default __.TextDocumentCodeAction(_) = notImplemented

  /// The code action request is sent from the client to the server to compute commands for a given text
  /// document and range. These commands are typically code fixes to either fix problems or to
  /// beautify/refactor code. The result of a textDocument/codeAction request is an array of Command literals
  /// which are typically presented in the user interface. When the command is selected the server should be
  /// contacted again (via the workspace/executeCommand) request to execute the command.
  abstract member CodeActionResolve: CodeAction -> AsyncLspResult<CodeAction option>

  default __.CodeActionResolve(_) = notImplemented

  /// The code lens request is sent from the client to the server to compute code lenses for a given
  /// text document.
  abstract member TextDocumentCodeLens: CodeLensParams -> AsyncLspResult<CodeLens [] option>

  default __.TextDocumentCodeLens(_) = notImplemented

  /// The code lens resolve request is sent from the client to the server to resolve the command for
  /// a given code lens item.
  abstract member CodeLensResolve: CodeLens -> AsyncLspResult<CodeLens>

  default __.CodeLensResolve(_) = notImplemented

  /// The signature help request is sent from the client to the server to request signature information at
  /// a given cursor position.
  abstract member TextDocumentSignatureHelp: SignatureHelpParams -> AsyncLspResult<SignatureHelp option>

  default __.TextDocumentSignatureHelp(_) = notImplemented

  /// The document link resolve request is sent from the client to the server to resolve the target of
  /// a given document link.
  abstract member DocumentLinkResolve: DocumentLink -> AsyncLspResult<DocumentLink>

  default __.DocumentLinkResolve(_) = notImplemented

  /// The document color request is sent from the client to the server to list all color references
  /// found in a given text document. Along with the range, a color value in RGB is returned.
  abstract member TextDocumentDocumentColor: DocumentColorParams -> AsyncLspResult<ColorInformation []>

  default __.TextDocumentDocumentColor(_) = notImplemented

  /// The color presentation request is sent from the client to the server to obtain a list of
  /// presentations for a color value at a given location. Clients can use the result to
  abstract member TextDocumentColorPresentation: ColorPresentationParams -> AsyncLspResult<ColorPresentation []>

  default __.TextDocumentColorPresentation(_) = notImplemented

  /// The document formatting request is sent from the client to the server to format a whole document.
  abstract member TextDocumentFormatting: DocumentFormattingParams -> AsyncLspResult<TextEdit [] option>
  default __.TextDocumentFormatting(_) = notImplemented

  /// The document range formatting request is sent from the client to the server to format a given
  /// range in a document.
  abstract member TextDocumentRangeFormatting: DocumentRangeFormattingParams -> AsyncLspResult<TextEdit [] option>

  default __.TextDocumentRangeFormatting(_) = notImplemented

  /// The document on type formatting request is sent from the client to the server to format parts
  /// of the document during typing.
  abstract member TextDocumentOnTypeFormatting: DocumentOnTypeFormattingParams -> AsyncLspResult<TextEdit [] option>

  default __.TextDocumentOnTypeFormatting(_) = notImplemented

  /// The document symbol request is sent from the client to the server to return a flat list of all symbols
  /// found in a given text document. Neither the symbol’s location range nor the symbol’s container name
  /// should be used to infer a hierarchy.
  abstract member TextDocumentDocumentSymbol:
    DocumentSymbolParams -> AsyncLspResult<U2<SymbolInformation [], DocumentSymbol []> option>

  default __.TextDocumentDocumentSymbol(_) = notImplemented

  /// The watched files notification is sent from the client to the server when the client detects changes
  /// to files watched by the language client. It is recommended that servers register for these file
  /// events using the registration mechanism. In former implementations clients pushed file events without
  /// the server actively asking for it.
  abstract member WorkspaceDidChangeWatchedFiles: DidChangeWatchedFilesParams -> Async<unit>

  default __.WorkspaceDidChangeWatchedFiles(_) = ignoreNotification

  /// The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server to inform
  /// the server about workspace folder configuration changes. The notification is sent by default if both
  /// *ServerCapabilities/workspace/workspaceFolders* and *ClientCapabilities/workapce/workspaceFolders* are
  /// true; or if the server has registered to receive this notification it first.
  abstract member WorkspaceDidChangeWorkspaceFolders: DidChangeWorkspaceFoldersParams -> Async<unit>

  default __.WorkspaceDidChangeWorkspaceFolders(_) = ignoreNotification

  /// A notification sent from the client to the server to signal the change of configuration settings.
  abstract member WorkspaceDidChangeConfiguration: DidChangeConfigurationParams -> Async<unit>
  default __.WorkspaceDidChangeConfiguration(_) = ignoreNotification

  /// The will create files request is sent from the client to the server before files are actually created
  /// as long as the creation is triggered from within the client either by a user action or by applying a
  /// workspace edit
  abstract member WorkspaceWillCreateFiles: CreateFilesParams -> AsyncLspResult<WorkspaceEdit option>

  default __.WorkspaceWillCreateFiles(_) = notImplemented

  /// The did create files notification is sent from the client to the server when files were created
  /// from within the client.
  abstract member WorkspaceDidCreateFiles: CreateFilesParams -> Async<unit>

  default __.WorkspaceDidCreateFiles(_) = ignoreNotification

  /// The will rename files request is sent from the client to the server before files are actually renamed
  /// as long as the rename is triggered from within the client either by a user action or by applying a
  /// workspace edit.
  abstract member WorkspaceWillRenameFiles: RenameFilesParams -> AsyncLspResult<WorkspaceEdit option>

  default __.WorkspaceWillRenameFiles(_) = notImplemented

  /// The did rename files notification is sent from the client to the server when files were renamed from
  /// within the client.
  abstract member WorkspaceDidRenameFiles: RenameFilesParams -> Async<unit>

  default __.WorkspaceDidRenameFiles(_) = ignoreNotification

  /// The will delete files request is sent from the client to the server before files are actually deleted
  /// as long as the deletion is triggered from within the client either by a user action or by applying a
  /// workspace edit.
  abstract member WorkspaceWillDeleteFiles: DeleteFilesParams -> AsyncLspResult<WorkspaceEdit option>

  default __.WorkspaceWillDeleteFiles(_) = notImplemented

  /// The did delete files notification is sent from the client to the server when files were deleted from
  /// within the client.
  abstract member WorkspaceDidDeleteFiles: DeleteFilesParams -> Async<unit>

  default __.WorkspaceDidDeleteFiles(_) = ignoreNotification

  /// The workspace symbol request is sent from the client to the server to list project-wide symbols matching
  /// the query string.
  abstract member WorkspaceSymbol: WorkspaceSymbolParams -> AsyncLspResult<SymbolInformation [] option>

  default __.WorkspaceSymbol(_) = notImplemented

  /// The `workspace/executeCommand` request is sent from the client to the server to trigger command execution
  /// on the server. In most cases the server creates a `WorkspaceEdit` structure and applies the changes to the
  /// workspace using the request `workspace/applyEdit` which is sent from the server to the client.
  abstract member WorkspaceExecuteCommand: ExecuteCommandParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken>

  default __.WorkspaceExecuteCommand(_) = notImplemented

  /// The document will save notification is sent from the client to the server before the document is
  /// actually saved.
  abstract member TextDocumentWillSave: WillSaveTextDocumentParams -> Async<unit>

  default __.TextDocumentWillSave(_) = ignoreNotification

  /// The document will save request is sent from the client to the server before the document is actually saved.
  /// The request can return an array of TextEdits which will be applied to the text document before it is saved.
  /// Please note that clients might drop results if computing the text edits took too long or if a server
  /// constantly fails on this request. This is done to keep the save fast and reliable.
  abstract member TextDocumentWillSaveWaitUntil: WillSaveTextDocumentParams -> AsyncLspResult<TextEdit [] option>

  default __.TextDocumentWillSaveWaitUntil(_) = notImplemented

  /// The document save notification is sent from the client to the server when the document was saved
  /// in the client.
  abstract member TextDocumentDidSave: DidSaveTextDocumentParams -> Async<unit>

  default __.TextDocumentDidSave(_) = ignoreNotification

  /// The document close notification is sent from the client to the server when the document got closed in the
  /// client. The document’s truth now exists where the document’s uri points to (e.g. if the document’s uri is
  /// a file uri the truth now exists on disk). As with the open notification the close notification is about
  /// managing the document’s content. Receiving a close notification doesn't mean that the document was open in
  /// an editor before. A close notification requires a previous open notification to be sent.
  abstract member TextDocumentDidClose: DidCloseTextDocumentParams -> Async<unit>

  default __.TextDocumentDidClose(_) = ignoreNotification

  /// The folding range request is sent from the client to the server to return all folding ranges found in a given text document.
  abstract member TextDocumentFoldingRange: FoldingRangeParams -> AsyncLspResult<FoldingRange list option>
  default __.TextDocumentFoldingRange(_) = notImplemented

  /// The selection range request is sent from the client to the server to return suggested selection ranges at an array of given positions.
  /// A selection range is a range around the cursor position which the user might be interested in selecting.
  abstract member TextDocumentSelectionRange: SelectionRangeParams -> AsyncLspResult<SelectionRange list option>

  default __.TextDocumentSelectionRange(_) = notImplemented

  abstract member TextDocumentSemanticTokensFull: SemanticTokensParams -> AsyncLspResult<SemanticTokens option>
  default __.TextDocumentSemanticTokensFull(_) = notImplemented

  abstract member TextDocumentSemanticTokensFullDelta:
    SemanticTokensDeltaParams -> AsyncLspResult<U2<SemanticTokens, SemanticTokensDelta> option>

  default __.TextDocumentSemanticTokensFullDelta(_) = notImplemented

  abstract member TextDocumentSemanticTokensRange: SemanticTokensRangeParams -> AsyncLspResult<SemanticTokens option>
  default __.TextDocumentSemanticTokensRange(_) = notImplemented

  /// The inlay hints request is sent from the client to the server to compute inlay hints for a given [text document, range] tuple
  ///  that may be rendered in the editor in place with other text.
  abstract member TextDocumentInlayHint: InlayHintParams -> AsyncLspResult<InlayHint [] option>

  default __.TextDocumentInlayHint(_) = notImplemented

  /// The request is sent from the client to the server to resolve additional information for a given inlay hint.
  /// This is usually used to compute the `tooltip`, `location` or `command` properties of a inlay hint’s label part
  /// to avoid its unnecessary computation during the `textDocument/inlayHint` request.
  ///
  /// Consider the clients announces the `label.location` property as a property that can be resolved lazy using the client capability
  /// ```typescript
  /// textDocument.inlayHint.resolveSupport = { properties: ['label.location'] };
  /// ```
  /// then an inlay hint with a label part without a location needs to be resolved using the `inlayHint/resolve` request before it can be used.
  abstract member InlayHintResolve: InlayHint -> AsyncLspResult<InlayHint>

  default __.InlayHintResolve(_) = notImplemented

  interface ILspServer with
    member this.Dispose() = this.Dispose()
    member this.Initialize(p: InitializeParams) = this.Initialize(p)
    member this.Initialized(p: InitializedParams) = this.Initialized(p)
    member this.Shutdown() = this.Shutdown()
    member this.Exit() = this.Exit()
    member this.TextDocumentHover(p: TextDocumentPositionParams) = this.TextDocumentHover(p)
    member this.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = this.TextDocumentDidOpen(p)
    member this.TextDocumentDidChange(p: DidChangeTextDocumentParams) = this.TextDocumentDidChange(p)
    member this.TextDocumentCompletion(p: CompletionParams) = this.TextDocumentCompletion(p)
    member this.CompletionItemResolve(p: CompletionItem) = this.CompletionItemResolve(p)
    member this.TextDocumentRename(p: RenameParams) = this.TextDocumentRename(p)
    member this.TextDocumentPrepareRename(p: PrepareRenameParams) = this.TextDocumentPrepareRename(p)
    member this.TextDocumentDefinition(p: TextDocumentPositionParams) = this.TextDocumentDefinition(p)
    member this.TextDocumentReferences(p: ReferenceParams) = this.TextDocumentReferences(p)
    member this.TextDocumentDocumentHighlight(p: TextDocumentPositionParams) = this.TextDocumentDocumentHighlight(p)
    member this.TextDocumentDocumentLink(p: DocumentLinkParams) = this.TextDocumentDocumentLink(p)
    member this.TextDocumentTypeDefinition(p: TextDocumentPositionParams) = this.TextDocumentTypeDefinition(p)
    member this.TextDocumentImplementation(p: TextDocumentPositionParams) = this.TextDocumentImplementation(p)
    member this.TextDocumentCodeAction(p: CodeActionParams) = this.TextDocumentCodeAction(p)
    member this.CodeActionResolve(p: CodeAction) = this.CodeActionResolve(p)
    member this.TextDocumentCodeLens(p: CodeLensParams) = this.TextDocumentCodeLens(p)
    member this.CodeLensResolve(p: CodeLens) = this.CodeLensResolve(p)
    member this.TextDocumentSignatureHelp(p: SignatureHelpParams) = this.TextDocumentSignatureHelp(p)
    member this.DocumentLinkResolve(p: DocumentLink) = this.DocumentLinkResolve(p)
    member this.TextDocumentDocumentColor(p: DocumentColorParams) = this.TextDocumentDocumentColor(p)
    member this.TextDocumentColorPresentation(p: ColorPresentationParams) = this.TextDocumentColorPresentation(p)
    member this.TextDocumentFormatting(p: DocumentFormattingParams) = this.TextDocumentFormatting(p)
    member this.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) = this.TextDocumentRangeFormatting(p)
    member this.TextDocumentOnTypeFormatting(p: DocumentOnTypeFormattingParams) = this.TextDocumentOnTypeFormatting(p)
    member this.TextDocumentDocumentSymbol(p: DocumentSymbolParams) = this.TextDocumentDocumentSymbol(p)
    member this.WorkspaceDidChangeWatchedFiles(p: DidChangeWatchedFilesParams) = this.WorkspaceDidChangeWatchedFiles(p)

    member this.WorkspaceDidChangeWorkspaceFolders(p: DidChangeWorkspaceFoldersParams) =
      this.WorkspaceDidChangeWorkspaceFolders(p)

    member this.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) =
      this.WorkspaceDidChangeConfiguration(p)

    member this.WorkspaceWillCreateFiles(p: CreateFilesParams) = this.WorkspaceWillCreateFiles(p)
    member this.WorkspaceDidCreateFiles(p: CreateFilesParams) = this.WorkspaceDidCreateFiles(p)
    member this.WorkspaceWillRenameFiles(p: RenameFilesParams) = this.WorkspaceWillRenameFiles(p)
    member this.WorkspaceDidRenameFiles(p: RenameFilesParams) = this.WorkspaceDidRenameFiles(p)
    member this.WorkspaceWillDeleteFiles(p: DeleteFilesParams) = this.WorkspaceWillDeleteFiles(p)
    member this.WorkspaceDidDeleteFiles(p: DeleteFilesParams) = this.WorkspaceDidDeleteFiles(p)
    member this.WorkspaceSymbol(p: WorkspaceSymbolParams) = this.WorkspaceSymbol(p)
    member this.WorkspaceExecuteCommand(p: ExecuteCommandParams) = this.WorkspaceExecuteCommand(p)
    member this.TextDocumentWillSave(p: WillSaveTextDocumentParams) = this.TextDocumentWillSave(p)
    member this.TextDocumentWillSaveWaitUntil(p: WillSaveTextDocumentParams) = this.TextDocumentWillSaveWaitUntil(p)
    member this.TextDocumentDidSave(p: DidSaveTextDocumentParams) = this.TextDocumentDidSave(p)
    member this.TextDocumentDidClose(p: DidCloseTextDocumentParams) = this.TextDocumentDidClose(p)
    member this.TextDocumentFoldingRange(p: FoldingRangeParams) = this.TextDocumentFoldingRange(p)
    member this.TextDocumentSelectionRange(p: SelectionRangeParams) = this.TextDocumentSelectionRange(p)
    member this.TextDocumentSemanticTokensFull(p: SemanticTokensParams) = this.TextDocumentSemanticTokensFull(p)

    member this.TextDocumentSemanticTokensFullDelta(p: SemanticTokensDeltaParams) =
      this.TextDocumentSemanticTokensFullDelta(p)

    member this.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams) = this.TextDocumentSemanticTokensRange(p)
    member this.TextDocumentInlayHint(p: InlayHintParams) = this.TextDocumentInlayHint(p)
    member this.InlayHintResolve(p: InlayHint) = this.InlayHintResolve(p)