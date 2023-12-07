module Ionide.LanguageServerProtocol.Types

open System.Diagnostics
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System
open System.Collections.Generic
open System.Runtime.Serialization

type ErasedUnionAttribute() =
  inherit Attribute()

[<ErasedUnion>]
type U2<'a, 'b> =
  | First of 'a
  | Second of 'b

type LspResult<'t> = Result<'t, JsonRpc.Error>
type AsyncLspResult<'t> = Async<LspResult<'t>>

module LspResult =
  open Ionide.LanguageServerProtocol

  let success x : LspResult<_> = Result.Ok x

  let invalidParams s : LspResult<_> = Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, s))

  let internalError<'a> (s: string) : LspResult<'a> =
    Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.internalError, s))

  let notImplemented<'a> : LspResult<'a> = Result.Error(JsonRpc.Error.MethodNotFound)

  let requestCancelled<'a> : LspResult<'a> = Result.Error(JsonRpc.Error.RequestCancelled)

module AsyncLspResult =
  open Ionide.LanguageServerProtocol

  let success x : AsyncLspResult<_> = async.Return(Result.Ok x)

  let invalidParams s : AsyncLspResult<_> =
    async.Return(Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, s)))

  let internalError s : AsyncLspResult<_> =
    async.Return(Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.internalError, s)))

  let notImplemented<'a> : AsyncLspResult<'a> = async.Return(Result.Error(JsonRpc.Error.MethodNotFound))

/// The LSP any type
type LSPAny = JToken

type TextDocumentSyncKind =
  | None = 0
  | Full = 1
  | Incremental = 2

type DocumentFilter =
  {
    /// A language id, like `typescript`.
    Language: string option

    /// A Uri scheme, like `file` or `untitled`.
    Scheme: string option

    /// A glob pattern, like `*.{ts,js}`.
    Pattern: string option
  }

type DocumentSelector = DocumentFilter[]

/// Position in a text document expressed as zero-based line and zero-based character offset.
/// A position is between two characters like an ‘insert’ cursor in a editor.
[<StructuredFormatDisplay("{DebuggerDisplay}")>]
type Position =
  {
    /// Line position in a document (zero-based).
    Line: int

    /// Character offset on a line in a document (zero-based). Assuming that the line is
    /// represented as a string, the `character` value represents the gap between the
    /// `character` and `character + 1`.
    ///
    /// If the character value is greater than the line length it defaults back to the
    /// line length.
    Character: int
  }

  [<DebuggerBrowsable(DebuggerBrowsableState.Never); JsonIgnore>]
  member x.DebuggerDisplay = $"({x.Line},{x.Character})"
  override x.ToString() = x.DebuggerDisplay

/// A range in a text document expressed as (zero-based) start and end positions.
/// A range is comparable to a selection in an editor. Therefore the end position is exclusive.
///
/// If you want to specify a range that contains a line including the line ending character(s)
/// then use an end position denoting the start of the next line. For example:
///
/// ```fsharp
/// {
///     Start = { Line = 5; character = 23 }
///     End = { Line = 6; character = 0 }
/// }
/// ```
[<StructuredFormatDisplay("{DebuggerDisplay}")>]
type Range =
  {
    /// The range's start position.
    Start: Position

    /// The range's end position.
    End: Position
  }

  [<DebuggerBrowsable(DebuggerBrowsableState.Never); JsonIgnore>]
  member x.DebuggerDisplay = $"{x.Start.DebuggerDisplay}-{x.End.DebuggerDisplay}"
  override x.ToString() = x.DebuggerDisplay

type DocumentUri = string

/// Represents a location inside a resource, such as a line inside a text file.
type Location = { Uri: DocumentUri; Range: Range }

type ITextDocumentIdentifier =
  /// Warning: normalize this member by UrlDecoding it before use
  abstract member Uri: DocumentUri

type TextDocumentIdentifier =
  {
    /// The text document's URI.
    Uri: DocumentUri
  }

  interface ITextDocumentIdentifier with
    member this.Uri = this.Uri

type VersionedTextDocumentIdentifier =
  {
    /// The text document's URI.
    Uri: DocumentUri

    /// The version number of this document.
    /// The version number of a document will increase after each change,
    /// including undo/redo. The number doesn't need to be consecutive.
    Version: int
  }

  interface ITextDocumentIdentifier with
    member this.Uri = this.Uri

type OptionalVersionedTextDocumentIdentifier =
  {
    /// The text document's URI.
    Uri: DocumentUri

    /// The version number of this document. If a versioned text document identifier
    /// is sent from the server to the client and the file is not open in the editor
    /// (the server has not received an open notification before) the server can send
    /// `null` to indicate that the version is known and the content on disk is the
    /// truth (as speced with document content ownership)
    /// Explicitly include the null value here.
    [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
    Version: int option
  }

  interface ITextDocumentIdentifier with
    member this.Uri = this.Uri

type SymbolKind =
  | File = 1
  | Module = 2
  | Namespace = 3
  | Package = 4
  | Class = 5
  | Method = 6
  | Property = 7
  | Field = 8
  | Constructor = 9
  | Enum = 10
  | Interface = 11
  | Function = 12
  | Variable = 13
  | Constant = 14
  | String = 15
  | Number = 16
  | Boolean = 17
  | Array = 18
  | Object = 19
  | Key = 20
  | Null = 21
  | EnumMember = 22
  | Struct = 23
  | Event = 24
  | Operator = 25
  | TypeParameter = 26

type SymbolTag =
  | Deprecated = 1

/// Represents information about programming constructs like variables, classes,
/// interfaces etc.
type SymbolInformation =
  {
    /// The name of this symbol.
    Name: string

    /// The kind of this symbol.
    Kind: SymbolKind

    /// Tags for this symbol.
    Tags: SymbolTag[] option

    /// Indicates if this symbol is deprecated.
    /// @deprecated Use tags instead
    Deprecated: bool option

    /// The location of this symbol. The location's range is used by a tool
    /// to reveal the location in the editor. If the symbol is selected in the
    /// tool the range's start information is used to position the cursor. So
    /// the range usually spans more then the actual symbol's name and does
    /// normally include things like visibility modifiers.
    ///
    /// The range doesn't have to denote a node range in the sense of a abstract
    /// syntax tree. It can therefore not be used to re-construct a hierarchy of
    /// the symbols.
    Location: Location

    /// The name of the symbol containing this symbol. This information is for
    /// user interface purposes (e.g. to render a qualifier in the user interface
    /// if necessary). It can't be used to re-infer a hierarchy for the document
    /// symbols.
    ContainerName: string option
  }

/// Represents programming constructs like variables, classes, interfaces etc.
/// that appear in a document. Document symbols can be hierarchical and they
/// have two ranges: one that encloses its definition and one that points to its
/// most interesting range, e.g. the range of an identifier.
type DocumentSymbol =
  {
    /// The name of this symbol. Will be displayed in the user interface and
    /// therefore must not be an empty string or a string only consisting of
    /// white spaces.
    Name: string
    /// More detail for this symbol, e.g the signature of a function.
    Detail: string option
    /// The kind of this symbol.
    Kind: SymbolKind
    /// tags for this document symbol.
    Tags: SymbolTag[] option
    /// Indicates if this symbol is deprecated.
    /// @deprecated Use tags instead
    Deprecated: bool option
    /// The range enclosing this symbol not including leading/trailing whitespace
    /// but everything else like comments. This information is typically used to
    /// determine if the clients cursor is inside the symbol to reveal in the
    /// symbol in the UI.
    Range: Range
    /// The range that should be selected and revealed when this symbol is being
    /// picked, e.g. the name of a function. Must be contained by the `range`.
    SelectionRange: Range
    /// Children of this symbol, e.g. properties of a class.
    Children: DocumentSymbol[] option
  }

type WorkspaceSymbol =
  {
    /// The name of this symbol.
    Name: string

    /// The kind of this symbol.
    Kind: SymbolKind

    /// Tags for this completion item.
    Tags: SymbolTag[] option

    /// The name of the symbol containing this symbol. This information is for
    /// user interface purposes (e.g. to render a qualifier in the user
    /// interface if necessary). It can't be used to re-infer a hierarchy for
    /// the document symbols.
    ContainerName: string option

    /// The location of this symbol. Whether a server is allowed to return a
    /// location without a range depends on the client capability
    /// `workspace.symbol.resolveSupport`.
    /// See also `SymbolInformation.location`.
    Location: U2<Location, TextDocumentIdentifier>

    /// A data entry field that is preserved on a workspace symbol between a
    /// workspace symbol request and a workspace symbol resolve request.
    Data: LSPAny option
  }

/// A textual edit applicable to a text document.
type TextEdit =
  {
    /// The range of the text document to be manipulated. To insert
    /// text into a document create a range where start === end.
    Range: Range

    /// The string to be inserted. For delete operations use an
    /// empty string.
    NewText: string
  }

/// Describes textual changes on a single text document. The text document is referred to as a
/// `VersionedTextDocumentIdentifier` to allow clients to check the text document version before an edit is
/// applied. A `TextDocumentEdit` describes all changes on a version Si and after they are applied move the
/// document to version Si+1. So the creator of a `TextDocumentEdit `doesn't need to sort the array or do any
/// kind of ordering. However the edits must be non overlapping.
type TextDocumentEdit =
  {
    /// The text document to change.
    TextDocument: OptionalVersionedTextDocumentIdentifier

    /// The edits to be applied.
    Edits: TextEdit[]
  }

/// Create file operation
type CreateFile =
  { /// The kind of resource operation. This should always be `"create"`.
    Kind: string

    /// The resource to create.
    Uri: DocumentUri
  }

/// Rename file operation
type RenameFile =
  { /// The kind of resource operation. This should always be `"rename"`.
    Kind: string

    /// The old (existing) location.
    oldUri: DocumentUri

    /// The new location.
    newUri: DocumentUri
  }

/// Delete file operation
type DeleteFile =
  { /// The kind of resource operation. This should always be `"delete"`.
    Kind: string

    /// The file to delete.
    Uri: DocumentUri
  }


/// Represents the possible values in the `WorkspaceEdit`'s `DocumentChanges` field.
[<ErasedUnion>]
type DocumentChange =
  | TextDocumentEdit of TextDocumentEdit
  | CreateFile of CreateFile
  | RenameFile of RenameFile
  | DeleteFile of DeleteFile

module DocumentChange =
    let createFile uri = DocumentChange.CreateFile {Kind = "create"; Uri = uri}
    let renameFile oldUri newUri = DocumentChange.RenameFile {Kind = "rename"; oldUri = oldUri; newUri = newUri}
    let deleteFile uri = DocumentChange.DeleteFile {Kind = "delete"; Uri = uri}

type TraceSetting =
  | Off = 0
  | Messages = 1
  | Verbose = 2

/// Capabilities for methods that support dynamic registration.
type DynamicCapabilities =
  {
    /// Method supports dynamic registration.
    DynamicRegistration: bool option
  }

type DynamicLinkSupportCapabilities =
  {
    /// Whether implementation supports dynamic registration.
    DynamicRegistration: bool option

    /// The client supports additional metadata in the form of declaration links.
    LinkSupport: bool option
  }

type ResourceOperationKind =
  | Create
  | Rename
  | Delete

type FailureHandlingKind =
  | Abort
  | Transactional
  | Undo
  | TextOnlyTransactional

type ChangeAnnotationSupport = { GroupsOnLabel: bool option }

/// Capabilities specific to `WorkspaceEdit`s
type WorkspaceEditCapabilities =
  {
    /// The client supports versioned document changes in `WorkspaceEdit`s
    DocumentChanges: bool option
    /// The resource operations the client supports. Clients should at least
    /// support 'create', 'rename' and 'delete' files and folders.
    ResourceOperations: ResourceOperationKind[] option
    /// The failure handling strategy of a client if applying the workspace edit fails.
    FailureHandling: FailureHandlingKind option
    /// Whether the client normalizes line endings to the client specific setting.
    /// If set to `true` the client will normalize line ending characters
    /// in a workspace edit to the client specific new line character(s).
    NormalizesLineEndings: bool option
    /// Whether the client in general supports change annotations on text edits, create file, rename file and delete file changes.
    ChangeAnnotationSupport: ChangeAnnotationSupport option
  }

/// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
type SymbolKindCapabilities =
  {
    /// The symbol kind values the client supports. When this
    /// property exists the client also guarantees that it will
    /// handle values outside its set gracefully and falls back
    /// to a default value when unknown.
    ///
    /// If this property is not present the client only supports
    /// the symbol kinds from `File` to `Array` as defined in
    /// the initial version of the protocol.
    ValueSet: SymbolKind[] option
  }

  static member DefaultValueSet =
    [| SymbolKind.File
       SymbolKind.Module
       SymbolKind.Namespace
       SymbolKind.Package
       SymbolKind.Class
       SymbolKind.Method
       SymbolKind.Property
       SymbolKind.Field
       SymbolKind.Constructor
       SymbolKind.Enum
       SymbolKind.Interface
       SymbolKind.Function
       SymbolKind.Variable
       SymbolKind.Constant
       SymbolKind.String
       SymbolKind.Number
       SymbolKind.Boolean
       SymbolKind.Array |]

type SymbolTagSupport =
  {
    /// The tags supported by the client.
    ValueSet: SymbolTag[]
  }

type ResolveSupport =
  {
    /// The properties that a client can resolve lazily.
    Properties: string[]
  }

/// Capabilities specific to the `workspace/symbol` request.
type SymbolCapabilities =
  {
    /// Symbol request supports dynamic registration.
    DynamicRegistration: bool option

    /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
    SymbolKind: SymbolKindCapabilities option

    /// The client supports tags on `SymbolInformation` and `WorkspaceSymbol`.
    /// Clients supporting tags have to handle unknown tags gracefully.
    TagSupport: SymbolTagSupport option

    /// The client support partial workspace symbols. The client will send the
    /// request `workspaceSymbol/resolve` to the server to resolve additional
    /// properties.
    ResolveSupport: ResolveSupport option
  }

type SemanticTokensWorkspaceClientCapabilities =
  {
    /// Whether the client implementation supports a refresh request sent from
    /// the server to the client.
    ///
    /// Note that this event is global and will force the client to refresh all
    /// semantic tokens currently shown. It should be used with absolute care
    /// and is useful for situation where a server for example detect a project
    /// wide change that requires such a calculation.
    RefreshSupport: bool option
  }

/// Client workspace capabilities specific to inlay hints.
type InlayHintWorkspaceClientCapabilities =
  {
    /// Whether the client implementation supports a refresh request sent from
    /// the server to the client.
    ///
    /// Note that this event is global and will force the client to refresh all
    /// inlay hints currently shown. It should be used with absolute care and
    /// is useful for situation where a server for example detects a project wide
    /// change that requires such a calculation.
    RefreshSupport: bool option
  }

/// Client workspace capabilities specific to inline values.
type InlineValueWorkspaceClientCapabilities =
  {
    /// Whether the client implementation supports a refresh request sent from
    /// the server to the client.
    ///
    /// Note that this event is global and will force the client to refresh all
    /// inline values currently shown. It should be used with absolute care and
    /// is useful for situation where a server for example detects a project wide
    /// change that requires such a calculation.
    RefreshSupport: bool option
  }

type CodeLensWorkspaceClientCapabilities =
  {
    /// Whether the client implementation supports a refresh request sent from the
    /// server to the client.
    ///
    /// Note that this event is global and will force the client to refresh all
    /// code lenses currently shown. It should be used with absolute care and is
    /// useful for situation where a server for example detect a project wide
    /// change that requires such a calculation.
    RefreshSupport: bool option
  }

type WorkspaceFileOperationsClientCapabilities =
  {
    /// Whether the client supports dynamic registration for file
    /// requests/notifications.
    DynamicRegistration: bool option

    /// The client has support for sending didCreateFiles notifications.
    DidCreate: bool option

    /// The client has support for sending willCreateFiles requests.
    WillCreate: bool option

    /// The client has support for sending didRenameFiles notifications.
    DidRename: bool option

    /// The client has support for sending willRenameFiles requests.
    WillRename: bool option

    /// The client has support for sending didDeleteFiles notifications.
    DidDelete: bool option

    /// The client has support for sending willDeleteFiles requests.
    WillDelete: bool option
  }

type DidChangeWatchedFilesClientCapabilities =
  {
    /// Did change watched files notification supports dynamic registration.
    /// Please note that the current protocol doesn't support static
    /// configuration for file changes from the server side.
    DynamicRegistration: bool option

    /// Whether the client has support for relative patterns or not.
    RelativePatternSupport: bool option
  }

type DiagnosticWorkspaceClientCapabilities =
  {
    /// Whether the client implementation supports a refresh request sent from
    /// the server to the client.
    /// Note that this event is global and will force the client to refresh all
    /// pulled diagnostics currently shown. It should be used with absolute care
    /// and is useful for situation where a server for example detects a project
    /// wide change that requires such a calculation.
    RefreshSupport: bool option
  }

/// Workspace specific client capabilities.
type WorkspaceClientCapabilities =
  {
    /// The client supports applying batch edits to the workspace by supporting
    /// the request 'workspace/applyEdit'
    ApplyEdit: bool option

    /// Capabilities specific to `WorkspaceEdit`s
    WorkspaceEdit: WorkspaceEditCapabilities option

    /// Capabilities specific to the `workspace/didChangeConfiguration` notification.
    DidChangeConfiguration: DynamicCapabilities option

    /// Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
    DidChangeWatchedFiles: DidChangeWatchedFilesClientCapabilities option

    /// Capabilities specific to the `workspace/symbol` request.
    Symbol: SymbolCapabilities option

    /// Capabilities specific to the `workspace/executeCommand` request.
    ExecuteCommand: DynamicCapabilities option

    /// The client has support for workspace folders.
    WorkspaceFolders: bool option

    /// The client supports `workspace/configuration` requests.
    Configuration: bool option

    /// Capabilities specific to the semantic token requests scoped to the
    /// workspace.
    ///
    /// @since 3.16.0
    SemanticTokens: SemanticTokensWorkspaceClientCapabilities option

    /// Client workspace capabilities specific to inlay hints.
    ///
    /// @since 3.17.0
    InlayHint: InlayHintWorkspaceClientCapabilities option


    /// Client workspace capabilities specific to inline value.
    ///
    /// @since 3.17.0
    InlineValue: InlineValueWorkspaceClientCapabilities option

    /// Client workspace capabilities specific to code lenses.
    ///
    /// @since 3.16.0
    CodeLens: CodeLensWorkspaceClientCapabilities option

    /// The client has support for file requests/notifications.
    FileOperations: WorkspaceFileOperationsClientCapabilities option

    /// Client workspace capabilities specific to diagnostics.
    Diagnostics: DiagnosticWorkspaceClientCapabilities option
  }

type SynchronizationCapabilities =
  {
    /// Whether text document synchronization supports dynamic registration.
    DynamicRegistration: bool option

    /// The client supports sending will save notifications.
    WillSave: bool option

    /// The client supports sending a will save request and
    /// waits for a response providing text edits which will
    /// be applied to the document before it is saved.
    WillSaveWaitUntil: bool option

    /// The client supports did save notifications.
    DidSave: bool option
  }

module MarkupKind =
  let PlainText = "plaintext"
  let Markdown = "markdown"

type HoverCapabilities =
  {
    /// Whether hover synchronization supports dynamic registration.
    DynamicRegistration: bool option

    /// Client supports the follow content formats for the content
    /// property. The order describes the preferred format of the client.
    /// See `MarkupKind` for common values
    ContentFormat: string[] option
  }

type CompletionItemTag =
  /// Render a completion as obsolete, usually using a strike-out.
  | Deprecated = 1

type CompletionItemTagSupport =
  {
    /// The tags supported by the client.
    ValueSet: CompletionItemTag[]
  }

type InsertTextMode =
  /// The insertion or replace strings is taken as it is. If the value is multi
  /// line the lines below the cursor will be inserted using the indentation
  /// defined in the string value.  The client will not apply any kind of
  /// adjustments to the string.
  | AsIs = 1
  /// The editor adjusts leading whitespace of new lines so that they match the
  /// indentation up to the cursor of the line for which the item is accepted.
  /// Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a multi
  /// line completion item is indented using 2 tabs and all following lines
  /// inserted will be indented using 2 tabs as well.
  | AdjustIndentation = 2

type InsertTextModeSupportCapability = { ValueSet: InsertTextMode[] }

type CompletionItemCapabilities =
  {
    /// Client supports snippets as insert text.
    ///
    /// A snippet can define tab stops and placeholders with `$1`, `$2`
    /// and `${3:foo}`. `$0` defines the final tab stop, it defaults to
    /// the end of the snippet. Placeholders with equal identifiers are linked,
    /// that is typing in one will update others too.
    SnippetSupport: bool option

    /// Client supports commit characters on a completion item.
    CommitCharactersSupport: bool option

    /// Client supports the follow content formats for the documentation
    /// property. The order describes the preferred format of the client.
    /// See `MarkupKind` for common values
    DocumentationFormat: string[] option

    /// Client supports the deprecated property on a completion item.
    DeprecatedSupport: bool option

    /// Client supports the preselect property on a completion item.
    PreselectSupport: bool option

    /// Client supports the tag property on a completion item. Clients
    /// supporting tags have to handle unknown tags gracefully. Clients
    /// especially need to preserve unknown tags when sending a completion item
    /// back to the server in a resolve call.
    TagSupport: CompletionItemTagSupport option

    /// Client supports insert replace edit to control different behavior if a
    /// completion item is inserted in the text or should replace text.
    InsertReplaceSupport: bool option

    /// Indicates which properties a client can resolve lazily on a completion
    /// item. Before version 3.16.0 only the predefined properties
    /// `documentation` and `detail` could be resolved lazily.
    ResolveSupport: ResolveSupport option

    /// The client supports the `insertTextMode` property on a completion item
    /// to override the whitespace handling mode as defined by the client.
    InsertTextModeSupport: InsertTextModeSupportCapability option

    /// The client has support for completion item label details.
    LabelDetailsSupport: bool option
  }

type CompletionItemKind =
  | Text = 1
  | Method = 2
  | Function = 3
  | Constructor = 4
  | Field = 5
  | Variable = 6
  | Class = 7
  | Interface = 8
  | Module = 9
  | Property = 10
  | Unit = 11
  | Value = 12
  | Enum = 13
  | Keyword = 14
  | Snippet = 15
  | Color = 16
  | File = 17
  | Reference = 18
  | Folder = 19
  | EnumMember = 20
  | Constant = 21
  | Struct = 22
  | Event = 23
  | Operator = 24
  | TypeParameter = 25

type CompletionItemKindCapabilities =
  {
    /// The completion item kind values the client supports. When this
    /// property exists the client also guarantees that it will
    /// handle values outside its set gracefully and falls back
    /// to a default value when unknown.
    ///
    /// If this property is not present the client only supports
    /// the completion items kinds from `Text` to `Reference` as defined in
    /// the initial version of the protocol.
    ValueSet: CompletionItemKind[] option
  }

  static member DefaultValueSet =
    [| CompletionItemKind.Text
       CompletionItemKind.Method
       CompletionItemKind.Function
       CompletionItemKind.Constructor
       CompletionItemKind.Field
       CompletionItemKind.Variable
       CompletionItemKind.Class
       CompletionItemKind.Interface
       CompletionItemKind.Module
       CompletionItemKind.Property
       CompletionItemKind.Unit
       CompletionItemKind.Value
       CompletionItemKind.Enum
       CompletionItemKind.Keyword
       CompletionItemKind.Snippet
       CompletionItemKind.Color
       CompletionItemKind.File
       CompletionItemKind.Reference |]

type CompletionListCapabilities =
  {
    /// The client supports the following itemDefaults on a completion list.
    /// The value lists the supported property names of the
    /// `CompletionList.itemDefaults` object. If omitted no properties are
    /// supported.
    ItemDefaults: string[] option
  }

/// Capabilities specific to the `textDocument/completion`
type CompletionCapabilities =
  {
    /// Whether completion supports dynamic registration.
    DynamicRegistration: bool option

    /// The client supports the following `CompletionItem` specific
    /// capabilities.
    CompletionItem: CompletionItemCapabilities option

    CompletionItemKind: CompletionItemKindCapabilities option

    /// The client supports to send additional context information for a
    /// `textDocument/completion` request.
    ContextSupport: bool option

    /// The client's default when the completion item doesn't provide a
    /// `insertTextMode` property.
    InsertTextMode: InsertTextMode option

    /// The client supports the following `CompletionList` specific capabilities.
    CompletionList: CompletionListCapabilities option
  }

type ParameterInformationCapability =
  {
    /// The client supports processing label offsets instead of a simple label
    /// string.
    LabelOffsetSupport: bool option
  }

type SignatureInformationCapabilities =
  {
    /// Client supports the follow content formats for the documentation
    /// property. The order describes the preferred format of the client.
    /// See `MarkupKind` for common values
    DocumentationFormat: string[] option

    /// Client capabilities specific to parameter information.
    ParameterInformation: ParameterInformationCapability option

    /// The client supports the `activeParameter` property on
    /// `SignatureInformation` literal.
    ActiveParameterSupport: bool option
  }

type SignatureHelpCapabilities =
  {
    /// Whether signature help supports dynamic registration.
    DynamicRegistration: bool option

    /// The client supports the following `SignatureInformation`
    /// specific properties.
    SignatureInformation: SignatureInformationCapabilities option

    /// The client supports to send additional context information for a
    /// `textDocument/signatureHelp` request. A client that opts into
    /// contextSupport will also support the `retriggerCharacters` on
    /// `SignatureHelpOptions`.
    ContextSupport: bool option
  }

/// capabilities specific to the `textDocument/documentSymbol`
type DocumentSymbolCapabilities =
  {
    /// Whether document symbol supports dynamic registration.
    DynamicRegistration: bool option

    /// Specific capabilities for the `SymbolKind`.
    SymbolKind: SymbolKindCapabilities option

    /// The client supports hierarchical document symbols.
    HierarchicalDocumentSymbolSupport: bool option

    /// The client supports tags on `SymbolInformation`. Tags are supported on
    /// `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
    /// Clients supporting tags have to handle unknown tags gracefully.
    TagSupport: SymbolTagSupport option

    /// The client supports an additional label presented in the UI when
    /// registering a document symbol provider.
    LabelSupport: bool option
  }

module CodeActionKind =
  /// Empty kind.
  let Empty = ""

  /// Base kind for quickfix actions: 'quickfix'.
  let QuickFix = "quickfix"

  /// Base kind for refactoring actions: 'refactor'.
  let Refactor = "refactor"

  /// Base kind for refactoring extraction actions: 'refactor.extract'.
  ///
  /// Example extract actions:
  ///
  /// - Extract method
  /// - Extract function
  /// - Extract variable
  /// - Extract interface from class
  /// - ...
  let RefactorExtract = "refactor.extract"

  /// Base kind for refactoring inline actions: 'refactor.inline'.
  ///
  /// Example inline actions:
  ///
  /// - Inline function
  /// - Inline variable
  /// - Inline constant
  /// - ...
  let RefactorInline = "refactor.inline"

  /// Base kind for refactoring rewrite actions: 'refactor.rewrite'.
  ///
  /// Example rewrite actions:
  ///
  /// - Convert JavaScript function to class
  /// - Add or remove parameter
  /// - Encapsulate field
  /// - Make method static
  /// - Move method to base class
  /// - ...
  let RefactorRewrite = "refactor.rewrite"

  /// Base kind for source actions: `source`.
  ///
  /// Source code actions apply to the entire file.
  let Source = "source"

  ///
  /// Base kind for an organize imports source action:
  /// `source.organizeImports`.
  ///
  let SourceOrganizeImports = "source.organizeImports"

  ///
  /// Base kind for a 'fix all' source action: `source.fixAll`.
  ///
  /// 'Fix all' actions automatically fix errors that have a clear fix that
  /// do not require user input. They should not suppress errors or perform
  /// unsafe fixes such as generating new types or classes.
  ///
  /// @since 3.17.0
  let SourceFixAll = "source.fixAll"

type CodeActionClientCapabilityLiteralSupportCodeActionKind =
  {
    /// The code action kind values the client supports. When this
    /// property exists the client also guarantees that it will
    /// handle values outside its set gracefully and falls back
    /// to a default value when unknown.
    /// See `CodeActionKind` for common values
    ValueSet: string[]
  }

type CodeActionClientCapabilityLiteralSupport =
  {
    /// The code action kind is supported with the following value set.
    CodeActionKind: CodeActionClientCapabilityLiteralSupportCodeActionKind
  }

/// capabilities specific to the `textDocument/codeAction`
type CodeActionClientCapabilities =
  {
    /// Whether document symbol supports dynamic registration.
    DynamicRegistration: bool option

    /// The client supports code action literals as a valid
    /// response of the `textDocument/codeAction` request.
    CodeActionLiteralSupport: CodeActionClientCapabilityLiteralSupport option

    /// Whether code action supports the `isPreferred` property.
    IsPreferredSupport: bool option

    /// Whether code action supports the `disabled` property.
    DisabledSupport: bool option

    /// Whether code action supports the `data` property which is
    /// preserved between a `textDocument/codeAction` and a
    /// `codeAction/resolve` request.
    DataSupport: bool option

    /// Whether the client supports resolving additional code action
    /// properties via a separate `codeAction/resolve` request.
    ResolveSupport: ResolveSupport option

    /// Whether the client honors the change annotations in
    /// text edits and resource operations returned via the
    /// `CodeAction#edit` property by for example presenting
    /// the workspace edit in the user interface and asking
    /// for confirmation.
    HonorsChangeAnnotations: bool option
  }

[<RequireQualifiedAccess>]
type DiagnosticTag =
  /// Unused or unnecessary code.
  ///
  /// Clients are allowed to render diagnostics with this tag faded out instead of having
  /// an error squiggle.
  | Unnecessary = 1
  /// Deprecated or obsolete code.
  ///
  /// Clients are allowed to rendered diagnostics with this tag strike through.
  | Deprecated = 2

type DiagnosticTagSupport =
  {

    /// Represents the tags supported by the client
    ValueSet: DiagnosticTag[]
  }

/// Capabilities specific to `textDocument/publishDiagnostics`.
type PublishDiagnosticsCapabilities =
  {
    /// Whether the clients accepts diagnostics with related information.
    RelatedInformation: bool option

    /// Client supports the tag property to provide meta data about a diagnostic.
    TagSupport: DiagnosticTagSupport option

    /// Whether the client interprets the version property of the
    /// `textDocument/publishDiagnostics` notification's parameter.
    VersionSupport: bool option

    /// Client supports a codeDescription property
    CodeDescriptionSupport: bool option

    /// Whether code action supports the `data` property which is preserved
    /// between a `textDocument/publishDiagnostics` and `textDocument/codeAction`
    /// request.
    DataSupport: bool option
  }

type FoldingRangeKindCapabilities =
  {
    /// The folding range kind values the client supports. When this property
    /// exists the client also guarantees that it will handle values outside its
    /// set gracefully and falls back to a default value when unknown.
    ValueSet: string[] option
  }

type FoldingRangeCapabilities' =
  {
    /// If set, the client signals that it supports setting collapsedText on
    /// folding ranges to display custom labels instead of the default text.
    CollapsedText: bool option
  }

type FoldingRangeCapabilities =
  {
    /// Whether implementation supports dynamic registration for folding range providers. If this is set to `true`
    /// the client supports the new `(FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
    /// return value for the corresponding server capability as well.
    DynamicRegistration: bool option

    /// The maximum number of folding ranges that the client prefers to receive per document. The value serves as a
    /// hint, servers are free to follow the limit.
    RangeLimit: int option

    /// If set, the client signals that it only supports folding complete lines. If set, client will
    /// ignore specified `startCharacter` and `endCharacter` properties in a FoldingRange.
    LineFoldingOnly: bool option

    /// Specific options for the folding range kind.
    FoldingRangeKind: FoldingRangeKindCapabilities option

    /// Specific options for the folding range.
    FoldingRange: FoldingRangeCapabilities' option
  }

type SemanticTokenFullRequestType =
  {
    /// The client will send the `textDocument/semanticTokens/full/delta`
    /// request if the server provides a corresponding handler.
    Delta: bool option
  }

type SemanticTokensRequests =
  {
    /// The client will send the `textDocument/semanticTokens/range` request
    /// if the server provides a corresponding handler.
    Range: bool option

    /// The client will send the `textDocument/semanticTokens/full` request
    /// if the server provides a corresponding handler.
    Full: U2<bool, SemanticTokenFullRequestType> option
  }

type TokenFormat = | Relative

type SemanticTokensClientCapabilities =
  {
    /// Whether implementation supports dynamic registration. If this is set to
    /// `true` the client supports the new `(TextDocumentRegistrationOptions &
    /// StaticRegistrationOptions)` return value for the corresponding server
    /// capability as well.
    DynamicRegistration: bool option

    /// Which requests the client supports and might send to the server
    /// depending on the server's capability. Please note that clients might not
    /// show semantic tokens or degrade some of the user experience if a range
    /// or full request is advertised by the client but not provided by the
    /// server. If for example the client capability `requests.full` and
    /// `request.range` are both set to true but the server only provides a
    /// range provider the client might not render a minimap correctly or might
    /// even decide to not show any semantic tokens at all.
    Requests: SemanticTokensRequests

    /// The token types that the client supports.
    TokenTypes: string[]

    /// The token modifiers that the client supports.
    TokenModifiers: string[]

    /// The formats the clients supports.
    Formats: TokenFormat[]

    /// Whether the client supports tokens that can overlap each other.
    OverlappingTokenSupport: bool option

    /// Whether the client supports tokens that can span multiple lines.
    MultilineTokenSupport: bool option

    /// Whether the client allows the server to actively cancel a semantic token
    /// request, e.g. supports returning ErrorCodes.ServerCancelled. If a server
    /// does the client needs to retrigger the request.
    ServerCancelSupport: bool option

    /// Whether the client uses semantic tokens to augment existing syntax
    /// tokens. If set to `true` client side created syntax tokens and semantic
    /// tokens are both used for colorization. If set to `false` the client only
    /// uses the returned semantic tokens for colorization.
    /// If the value is `undefined` then the client behavior is not specified.
    AugmentsSyntaxTokens: bool option
  }

/// Inlay hint client capabilities.
type InlayHintClientCapabilities =
  {
    /// Whether inlay hints support dynamic registration.
    DynamicRegistration: bool option
    /// Indicates which properties a client can resolve lazily on a inlay
    /// hint.
    ResolveSupport: ResolveSupport option
  }

type DiagnosticCapabilities =
  {
    /// Whether implementation supports dynamic registration. If this is set to `true` the client supports the new
    /// `(TextDocumentRegistrationOptions & StaticRegistrationOptions)` return value for the corresponding server
    /// capability as well.
    DynamicRegistration: bool option

    /// Whether the clients supports related documents for document diagnostic pulls.
    RelatedDocumentSupport: bool option
  }


/// Inline value client capabilities.
type InlineValueClientCapabilities =
  {
    /// Whether inline value support dynamic registration.
    DynamicRegistration: bool option
    /// Indicates which properties a client can resolve lazily on a inline
    /// value.
    ResolveSupport: ResolveSupport option
  }

type PrepareSupportDefaultBehavior =
  /// The client's default behavior is to select the identifier according to the
  /// language's syntax rule.
  | Identifier = 1

type RenameClientCapabilities =
  {
    /// Whether rename supports dynamic registration.
    DynamicRegistration: bool option

    /// Client supports testing for validity of rename operations before execution.
    /// @since version 3.12.0
    PrepareSupport: bool option

    /// Client supports the default behavior result
    /// (`{ defaultBehavior: boolean }`).
    /// The value indicates the default behavior used by the client.
    PrepareSupportDefaultBehavior: PrepareSupportDefaultBehavior option

    /// Whether the client honors the change annotations in text edits and resource operations
    /// returned via the rename request's workspace edit by for example presenting the workspace
    /// edit in the user interface and asking for confirmation.
    ///
    /// @since 3.16.0
    HonorsChangeAnnotations: bool option
  }

type DocumentLinkCapabilities =
  {
    /// Whether document link supports dynamic registration.
    DynamicRegistration: bool option

    /// Whether the client supports the `tooltip` property on `DocumentLink`.
    TooltipSupport: bool option
  }

/// Text document specific client capabilities.
type TextDocumentClientCapabilities =
  {
    Synchronization: SynchronizationCapabilities option

    /// Capabilities specific to `textDocument/publishDiagnostics`.
    PublishDiagnostics: PublishDiagnosticsCapabilities option

    /// Capabilities specific to the `textDocument/completion`
    Completion: CompletionCapabilities option

    /// Capabilities specific to the `textDocument/hover`
    Hover: HoverCapabilities option

    /// Capabilities specific to the `textDocument/signatureHelp`
    SignatureHelp: SignatureHelpCapabilities option

    /// Capabilities specific to the `textDocument/declaration` request.
    Declaration: DynamicLinkSupportCapabilities option

    /// Capabilities specific to the `textDocument/references`
    References: DynamicCapabilities option

    /// Whether document highlight supports dynamic registration.
    DocumentHighlight: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/documentSymbol`
    DocumentSymbol: DocumentSymbolCapabilities option

    /// Capabilities specific to the `textDocument/formatting`
    Formatting: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/rangeFormatting`
    RangeFormatting: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/onTypeFormatting`
    OnTypeFormatting: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/definition`
    Definition: DynamicLinkSupportCapabilities option

    /// Capabilities specific to the `textDocument/typeDefinition` request.
    TypeDefinition: DynamicLinkSupportCapabilities option

    /// Capabilities specific to the `textDocument/implementation` request.
    Implementation: DynamicLinkSupportCapabilities option

    /// Capabilities specific to the `textDocument/codeAction`
    CodeAction: CodeActionClientCapabilities option

    /// Capabilities specific to the `textDocument/codeLens`
    CodeLens: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/documentLink`
    DocumentLink: DocumentLinkCapabilities option

    /// Capabilities specific to the `textDocument/documentColor` and the `textDocument/colorPresentation` request.
    ColorProvider: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/rename`
    Rename: RenameClientCapabilities option

    /// Capabilities for the `textDocument/foldingRange`
    FoldingRange: FoldingRangeCapabilities option

    /// Capabilities for the `textDocument/selectionRange`
    SelectionRange: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/linkedEditingRange` request.
    LinkedEditingRange: DynamicCapabilities option

    /// Capabilities specific to the various call hierarchy requests.
    ///
    /// @since 3.16.0
    CallHierarchy: DynamicCapabilities option

    /// Capabilities specific to the various semantic token requests.
    /// @since 3.16.0
    SemanticTokens: SemanticTokensClientCapabilities option

    /// Capabilities specific to the `textDocument/moniker` request.
    Moniker: DynamicCapabilities option

    /// Capabilities specific to the various type hierarchy requests.
    ///
    /// @since 3.17.0
    TypeHierarchy: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/inlineValue` request.
    InlineValue: DynamicCapabilities option

    /// Capabilities specific to the `textDocument/inlayHint` request.
    ///
    /// @since 3.17.0
    InlayHint: InlayHintClientCapabilities option

    /// Capabilities specific to the diagnostic pull model.
    Diagnostic: DiagnosticCapabilities option
  }

/// Client capabilities for the showDocument request.
///
/// @since 3.16.0
type ShowDocumentClientCapabilities =
  {
    /// The client has support for the showDocument request
    support: bool
  }

/// Capabilities specific to the `MessageActionItem` type
type MessageActionItemCapabilties =
  {
    /// Whether the client supports additional attributes which
    /// are preserved and send back to the server in the
    /// request's response.
    additionalPropertiesSupport: bool option
  }

/// Show message request client capabilities
type ShowMessageRequestClientCapabilities =
  {
    /// Capabilities specific to the `MessageActionItem` type
    messageActionItem: MessageActionItemCapabilties option
  }

type WindowClientCapabilities =
  {
    ///
    /// It indicates whether the client supports server initiated
    /// progress using the `window/workDoneProgress/create` request.
    ///
    /// The capability also controls Whether client supports handling
    /// of progress notifications. If set servers are allowed to report a
    /// `workDoneProgress` property in the request specific server
    /// capabilities.
    ///
    /// @since 3.15.0
    workDoneProgress: bool option

    /// Capabilities specific to the showMessage request.
    ///
    /// @since 3.16.0
    showMessage: ShowMessageRequestClientCapabilities option

    /// Capabilities specific to the showDocument request.
    ///
    ///  @since 3.16.0
    showDocument: ShowDocumentClientCapabilities option
  }

type StaleRequestSupportClientCapabilities =
  {
    /// The client will actively cancel the request.
    Cancel: bool

    /// The list of requests for which the client will retry the request if it
    /// receives a response with error code `ContentModified``
    RetryOnContentModified: string[]
  }

type RegularExpressionsClientCapabilities =
  {
    /// The engine's name.
    Engine: string

    /// The engine's version.
    Version: string option
  }

type MarkdownClientCapabilities =
  {
    /// The name of the parser.
    Parser: string

    /// The version of the parser.
    Version: string option

    /// A list of HTML tags that the client allows / supports in Markdown.
    AllowedTags: string[] option
  }

type GeneralClientCapabilities =
  {
    /// Client capability that signals how the client handles stale requests
    /// (e.g. a request for which the client will not process the response
    /// anymore since the information is outdated).
    StaleRequestSupport: StaleRequestSupportClientCapabilities option

    /// Client capabilities specific to regular expressions.
    RegularExpressions: RegularExpressionsClientCapabilities option

    /// Client capabilities specific to the client's markdown parser.
    Markdown: MarkdownClientCapabilities option

    /// The position encodings supported by the client. Client and server have
    /// to agree on the same position encoding to ensure that offsets (e.g.
    /// character position in a line) are interpreted the same on both side.
    /// To keep the protocol backwards compatible the following applies: if the
    /// value 'utf-16' is missing from the array of position encodings servers
    /// can assume that the client supports UTF-16. UTF-16 is therefore a
    /// mandatory encoding.
    /// If omitted it defaults to ['utf-16'].
    /// Implementation considerations: since the conversion from one encoding
    /// into another requires the content of the file / line the conversion is
    /// best done where the file is read which is usually on the server side.
    PositionEncodings: string[] option
  }

type ClientCapabilities =
  {
    /// Workspace specific client capabilities.
    Workspace: WorkspaceClientCapabilities option

    /// Text document specific client capabilities.
    TextDocument: TextDocumentClientCapabilities option

    /// General client capabilities.
    General: GeneralClientCapabilities option

    /// Experimental client capabilities.
    Experimental: JToken option

    /// Window specific client capabilities.
    Window: WindowClientCapabilities option
  }

type WorkspaceFolder =
  {
    /// The associated URI for this workspace folder.
    Uri: DocumentUri

    /// The name of the workspace folder. Defaults to the
    /// uri's basename.
    Name: string
  }

type ClientInfo = { Name: string; Version: string option }

type InitializeParams =
  {
    /// The process Id of the parent process that started the server. Is null if
    /// the process has not been started by another process. If the parent
    /// process is not alive then the server should exit (see exit notification)
    /// its process.
    ProcessId: int option
    /// Information about the client.
    /// @since 3.15.0
    ClientInfo: ClientInfo option
    /// The locale the client is currently showing the user interface in. This
    /// must not necessarily be the locale of the operating system.
    /// Uses IETF language tags as the value's syntax (See
    /// https://en.wikipedia.org/wiki/IETF_language_tag)
    Locale: string option
    /// The rootPath of the workspace. Is null if no folder is open.
    /// @deprecated in favour of `rootUri`.
    RootPath: string option
    /// The rootUri of the workspace. Is null if no folder is open. If both
    /// `rootPath` and `rootUri` are set `rootUri` wins.
    /// @deprecated in favour of `workspaceFolders`
    RootUri: string option
    /// User provided initialization options.
    InitializationOptions: JToken option
    /// The capabilities provided by the client (editor or tool)
    Capabilities: ClientCapabilities option
    /// The initial trace setting. If omitted trace is disabled ('off').
    trace: string option
    /// The workspace folders configured in the client when the server starts.
    /// This property is only available if the client supports workspace folders.
    /// It can be `null` if the client supports workspace folders but none are configured.
    /// @since 3.6.0
    WorkspaceFolders: WorkspaceFolder[] option
  }

type InitializedParams() =
  override _.Equals(o) = o :? InitializedParams
  override _.GetHashCode() = 0
  override _.ToString() = "{}"


type CompletionItemOptions =
  {
    /// The server has support for completion item label details (see also
    /// `CompletionItemLabelDetails`) when receiving a completion item in a resolve call.
    LabelDetailsSupport: bool option
  }

/// Completion options.
type CompletionOptions =
  {
    /// The server provides support to resolve additional information for a completion item.
    ResolveProvider: bool option

    /// The characters that trigger completion automatically.
    TriggerCharacters: char[] option

    /// The list of all possible characters that commit a completion.
    /// This field can be used if clients don't support individual commit
    /// characters per completion item.
    ///
    /// See `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`.
    ///
    /// If a server provides both `allCommitCharacters` and commit characters
    /// on an individual completion item, the ones on the completion item win.
    AllCommitCharacters: char[] option

    /// The server supports the following `CompletionItem` specific capabilities.
    CompletionItem: CompletionItemOptions option
  }

/// Signature help options.
type SignatureHelpOptions =
  {
    /// The characters that trigger signature help automatically.
    TriggerCharacters: char[] option
    /// List of characters that re-trigger signature help.
    ///
    /// These trigger characters are only active when signature help is already showing.
    /// All trigger characters are also counted as re-trigger characters.
    RetriggerCharacters: char[] option
  }

/// Document Symbol options
type DocumentSymbolOptions =
  {
    /// A human-readable string that is shown when multiple outlines trees are
    /// shown for the same document.
    Label: string option
  }

/// Code action options.
type CodeActionOptions =
  {
    /// CodeActionKinds that this server may return.
    ///
    /// The list of kinds may be generic, such as `CodeActionKind.Refactor`,
    /// or the server may list out every specific kind they provide.
    CodeActionKinds: string[] option

    /// The server provides support to resolve additional
    /// information for a code action.
    ResolveProvider: bool option
  }

/// Code Lens options.
type CodeLensOptions =
  {
    /// Code lens has a resolve provider as well.
    ResolveProvider: bool option
  }

/// Format document on type options
type DocumentOnTypeFormattingOptions =
  {
    /// A character on which formatting should be triggered, like `}`.
    FirstTriggerCharacter: char

    /// More trigger characters.
    MoreTriggerCharacter: char[] option
  }

/// Document link options
type DocumentLinkOptions =
  {
    /// Document links have a resolve provider as well.
    ResolveProvider: bool option
  }

/// Execute command options.
type ExecuteCommandOptions =
  {
    /// The commands to be executed on the server
    commands: string[] option
  }

/// Save options.
type SaveOptions =
  {
    /// The client is supposed to include the content on save.
    IncludeText: bool option
  }

type TextDocumentSyncOptions =
  {
    /// Open and close notifications are sent to the server.
    OpenClose: bool option

    /// Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
    /// and TextDocumentSyncKind.Incremental.
    Change: TextDocumentSyncKind option

    /// Will save notifications are sent to the server.
    WillSave: bool option

    /// Will save wait until requests are sent to the server.
    WillSaveWaitUntil: bool option

    /// Save notifications are sent to the server.
    Save: SaveOptions option
  }

  static member Default =
    { OpenClose = None
      Change = None
      WillSave = None
      WillSaveWaitUntil = None
      Save = None }

type SemanticTokensLegend =
  {
    /// The token types a server uses.
    TokenTypes: string[]
    /// The token modifiers a server uses.
    TokenModifiers: string[]
  }

type SemanticTokenFullOptions =
  {
    /// The server supports deltas for full documents.
    Delta: bool option
  }

type SemanticTokensOptions =
  {
    /// The legend used by the server
    Legend: SemanticTokensLegend

    /// Server supports providing semantic tokens for a specific range of a document.
    Range: bool option

    /// Server supports providing semantic tokens for a full document.
    Full: U2<bool, SemanticTokenFullOptions> option
  }

type InlayHintOptions =
  {
    /// The server provides support to resolve additional information for an inlay hint item.
    ResolveProvider: bool option
  }

type InlineValueOptions =
  {
    /// The server provides support to resolve additional information for aniline lay hint item.
    ResolveProvider: bool option
  }

type DiagnosticOptions =
  {
    /// An optional identifier under which the diagnostics are managed by the client.
    Identifier: string option

    /// Whether the language has inter file dependencies meaning that editing code in one file can result in a different
    /// diagnostic set in another file. Inter file dependencies are common for most programming languages and typically
    /// uncommon for linters.
    InterFileDependencies: bool

    /// The server provides support for workspace diagnostics as well.
    WorkspaceDiagnostics: bool
  }

type WorkspaceFoldersServerCapabilities =
  {
    /// The server has support for workspace folders.
    Supported: bool option
    /// Whether the server wants to receive workspace folder change notifications.
    ChangeNotifications: U2<string, bool> option
  }

  static member Default = { Supported = None; ChangeNotifications = None }

module FileOperationPatternKind =
  let File = "file"
  let Folder = "folder"

type FileOperationPatternOptions =
  {
    /// The pattern should be matched ignoring casing.
    IgnoreCase: bool option
  }

  static member Default = { IgnoreCase = None }

/// See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileOperationPattern.
type FileOperationPattern =
  {
    Glob: string
    /// Whether to match files or folders with this pattern. Matches both if undefined.
    /// See FileOperationPatternKind for allowed values
    Matches: string option
    /// Additional options used during matching
    Options: FileOperationPatternOptions option
  }

type FileOperationFilter =
  {
    /// A Uri like `file` or `untitled`.
    Scheme: string option
    /// The actual file operation pattern.
    Pattern: FileOperationPattern
  }

type FileOperationRegistrationOptions = { Filters: FileOperationFilter[] }

/// The types of workspace-level file notifications the server is interested in.
type WorkspaceFileOperationsServerCapabilities =
  { DidCreate: FileOperationRegistrationOptions option
    WillCreate: FileOperationRegistrationOptions option
    DidRename: FileOperationRegistrationOptions option
    WillRename: FileOperationRegistrationOptions option
    DidDelete: FileOperationRegistrationOptions option
    WillDelete: FileOperationRegistrationOptions option }

  static member Default =
    { DidCreate = None
      WillCreate = None
      DidRename = None
      WillRename = None
      DidDelete = None
      WillDelete = None }

type WorkspaceServerCapabilities =
  {
    /// The server supports workspace folder.
    /// @since 3.6.0
    WorkspaceFolders: WorkspaceFoldersServerCapabilities option
    /// The server is interested in file notifications/requests.
    /// @since 3.16.0
    FileOperations: WorkspaceFileOperationsServerCapabilities option
  }

  static member Default = { WorkspaceFolders = None; FileOperations = None }


/// RenameOptions may only be specified if the client states that it supports prepareSupport in its
/// initial initialize request.
type RenameOptions =
  {
    /// Renames should be checked and tested before being executed.
    PrepareProvider: bool option
  }

type WorkspaceSymbolOptions =
  {
    /// The server provides support to resolve additional information for a
    /// workspace symbol.
    ResolveProvider: bool option
  }

type ServerCapabilities =
  {
    /// The position encoding the server picked from the encodings offered by
    /// the client via the client capability `general.positionEncodings`.
    /// If the client didn't provide any position encodings the only valid value
    /// that a server can return is 'utf-16'.
    /// If omitted it defaults to 'utf-16'.
    PositionEncoding: string option

    /// Defines how text documents are synced. Is either a detailed structure defining each notification or
    /// for backwards compatibility the TextDocumentSyncKind number.
    TextDocumentSync: TextDocumentSyncOptions option

    /// The server provides hover support.
    HoverProvider: bool option

    /// The server provides completion support.
    CompletionProvider: CompletionOptions option

    /// The server provides signature help support.
    SignatureHelpProvider: SignatureHelpOptions option

    /// The server provides go to declaration support.
    DeclarationProvider: bool option

    /// The server provides goto definition support.
    DefinitionProvider: bool option

    ///The server provides Goto Implementation support
    ImplementationProvider: bool option

    /// The server provides goto type definition support.
    TypeDefinitionProvider: bool option

    /// The server provides find references support.
    ReferencesProvider: bool option

    /// The server provides document highlight support.
    DocumentHighlightProvider: bool option

    /// The server provides document symbol support.
    DocumentSymbolProvider: U2<bool, DocumentSymbolOptions> option

    /// The server provides workspace symbol support.
    WorkspaceSymbolProvider: U2<bool, WorkspaceSymbolOptions> option

    /// The server provides code actions. The `CodeActionOptions` return type is
    /// only valid if the client signals code action literal support via the
    /// property `textDocument.codeAction.codeActionLiteralSupport`.
    CodeActionProvider: U2<bool, CodeActionOptions> option

    /// The server provides code lens.
    CodeLensProvider: CodeLensOptions option

    /// The server provides document formatting.
    DocumentFormattingProvider: bool option

    /// The server provides document range formatting.
    DocumentRangeFormattingProvider: bool option

    /// The server provides document formatting on typing.
    DocumentOnTypeFormattingProvider: DocumentOnTypeFormattingOptions option

    /// The server provides rename support. RenameOptions may only be specified
    /// if the client states that it supports `prepareSupport` in its initial
    /// `initialize` request.
    RenameProvider: U2<bool, RenameOptions> option

    /// The server provides document link support.
    DocumentLinkProvider: DocumentLinkOptions option

    /// The server provides color provider support.
    ColorProvider: bool option

    /// The server provides execute command support.
    ExecuteCommandProvider: ExecuteCommandOptions option

    /// Experimental server capabilities.
    Experimental: JToken option

    /// The server provides folding provider support.
    /// @since 3.10.0
    FoldingRangeProvider: bool option

    /// The server provides selection range support.
    SelectionRangeProvider: bool option

    /// The server provides linked editing range support.
    LinkedEditingRangeProvider: bool option

    /// The server provides call hierarchy support.
    CallHierarchyProvider: bool option

    /// The server provides semantic tokens support.
    SemanticTokensProvider: SemanticTokensOptions option

    /// Whether server provides moniker support.
    MonikerProvider: bool option

    /// The server provides type hierarchy support.
    TypeHierarchyProvider: bool option

    /// The server provides inlay hints.
    InlayHintProvider: InlayHintOptions option

    /// The server provides inline values.
    InlineValueProvider: InlineValueOptions option

    /// The server has support for pull model diagnostics.
    DiagnosticProvider: DiagnosticOptions option

    /// Workspace specific server capabilities.
    Workspace: WorkspaceServerCapabilities option
  }

  static member Default =
    { PositionEncoding = None
      TextDocumentSync = None
      HoverProvider = None
      CompletionProvider = None
      SignatureHelpProvider = None
      DeclarationProvider = None
      DefinitionProvider = None
      TypeDefinitionProvider = None
      ImplementationProvider = None
      ReferencesProvider = None
      DocumentHighlightProvider = None
      DocumentSymbolProvider = None
      WorkspaceSymbolProvider = None
      CodeActionProvider = None
      CodeLensProvider = None
      DocumentFormattingProvider = None
      DocumentRangeFormattingProvider = None
      DocumentOnTypeFormattingProvider = None
      RenameProvider = None
      DocumentLinkProvider = None
      ColorProvider = None
      ExecuteCommandProvider = None
      Experimental = None
      FoldingRangeProvider = None
      SelectionRangeProvider = None
      LinkedEditingRangeProvider = None
      CallHierarchyProvider = None
      SemanticTokensProvider = None
      MonikerProvider = None
      TypeHierarchyProvider = None
      InlayHintProvider = None
      InlineValueProvider = None
      DiagnosticProvider = None
      Workspace = None }

type ServerInfo =
  {
    /// The name of the server as defined by the server.
    Name: string
    /// The server's version as defined by the server.
    Version: string option
  }

type InitializeResult =
  {
    /// The capabilities the language server provides.
    Capabilities: ServerCapabilities

    /// Information about the server.
    ServerInfo: ServerInfo option
  }

  static member Default = { Capabilities = ServerCapabilities.Default; ServerInfo = None }

/// A workspace edit represents changes to many resources managed in the workspace.
/// The edit should either provide `changes` or `documentChanges`. If the client can handle versioned document
/// edits and if `documentChanges` are present, the latter are preferred over `changes`.
type WorkspaceEdit =
  {
    /// Holds changes to existing resources.
    Changes: Map<DocumentUri, TextEdit[]> option

    /// An array of `TextDocumentEdit`s to express changes to n different text documents
    /// where each text document edit addresses a specific version of a text document.
    /// Whether a client supports versioned document edits is expressed via
    /// `WorkspaceClientCapabilities.workspaceEdit.documentChanges`.
    DocumentChanges: DocumentChange[] option
  }

  static member DocumentChangesToChanges(edits: DocumentChange[]) =
    edits
    |> Array.collect (fun docChange ->
                       match docChange with
                       | TextDocumentEdit edit -> [|edit.TextDocument.Uri.ToString(), edit.Edits|]
                       | _ -> [||])
    |> Map.ofArray

  static member CanUseDocumentChanges(capabilities: ClientCapabilities) =
    (capabilities.Workspace
     |> Option.bind (fun x -> x.WorkspaceEdit)
     |> Option.bind (fun x -> x.DocumentChanges)) = Some true

  static member Create(documentChanges: DocumentChange[], capabilities: ClientCapabilities) =
    if WorkspaceEdit.CanUseDocumentChanges(capabilities) then
      { Changes = None; DocumentChanges = Some documentChanges }
    else
      { Changes = Some(WorkspaceEdit.DocumentChangesToChanges documentChanges)
        DocumentChanges = None }

type MessageType =
  | Error = 1
  | Warning = 2
  | Info = 3
  | Log = 4

type LogMessageParams = { Type: MessageType; Message: string }

type ShowMessageParams = { Type: MessageType; Message: string }

type MessageActionItem =
  {
    /// A short title like 'Retry', 'Open Log' etc.
    Title: string
  }

type ShowMessageRequestParams =
  {
    /// The message type.
    Type: MessageType

    /// The actual message
    Message: string

    /// The message action items to present.
    Actions: MessageActionItem[] option
  }

type ShowDocumentParams =
  {
    /// The uri to show.
    Uri: DocumentUri

    /// Indicates to show the resource in an external program. To show, for
    /// example, `https://code.visualstudio.com/` in the default WEB browser set
    /// `external` to `true`.
    External: bool option

    /// An optional property to indicate whether the editor showing the document
    /// should take focus or not.  Clients might ignore this property if an
    /// external program is started.
    TakeFocus: bool option

    /// An optional selection range if the document is a text document. Clients
    /// might ignore the property if an external program is started or the file
    /// is not a text file.
    Selection: Range option
  }

type ShowDocumentResult =
  {
    /// A boolean indicating if the show was successful.
    Success: bool
  }

/// General parameters to register for a capability.
type Registration =
  {
    /// The id used to register the request. The id can be used to deregister
    /// the request again.
    Id: string

    /// The method / capability to register for.
    Method: string

    /// Options necessary for the registration.
    RegisterOptions: JToken option
  }

type RegistrationParams = { Registrations: Registration[] }

type ITextDocumentRegistrationOptions =
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  abstract member DocumentSelector: DocumentSelector option

/// General parameters to unregister a capability.
type Unregistration =
  {
    /// The id used to unregister the request or notification. Usually an id
    /// provided during the register request.
    Id: string

    /// The method / capability to unregister for.
    Method: string
  }

type UnregistrationParams = { Unregisterations: Unregistration[] }

type FileChangeType =
  | Created = 1
  | Changed = 2
  | Deleted = 3

/// An event describing a file change.
type FileEvent =
  {
    /// The file's URI.
    Uri: DocumentUri

    /// The change type.
    Type: FileChangeType
  }

type DidChangeWatchedFilesParams =
  {
    /// The actual file events.
    Changes: FileEvent[]
  }

/// The workspace folder change event.
type WorkspaceFoldersChangeEvent =
  {
    /// The array of added workspace folders
    Added: WorkspaceFolder[]

    /// The array of the removed workspace folders
    Removed: WorkspaceFolder[]
  }

type DidChangeWorkspaceFoldersParams =
  {
    /// The actual workspace folder change event.
    Event: WorkspaceFoldersChangeEvent
  }

type DidChangeConfigurationParams =
  {
    /// The actual changed settings
    Settings: JToken
  }

type ConfigurationItem =
  {
    /// The scope to get the configuration section for.
    ScopeUri: string option

    /// The configuration section asked for.
    Section: string option
  }

type ConfigurationParams = { items: ConfigurationItem[] }

/// The parameters of a Workspace Symbol Request.
type WorkspaceSymbolParams =
  {
    /// A query string to filter symbols by. Clients may send an empty string
    /// here to request all symbols.
    Query: string
  }

type ExecuteCommandParams =
  {
    /// The identifier of the actual command handler.
    Command: string
    /// Arguments that the command should be invoked with.
    Arguments: JToken[] option
  }

type ApplyWorkspaceEditParams =
  {
    /// An optional label of the workspace edit. This label is
    /// presented in the user interface for example on an undo
    /// stack to undo the workspace edit.
    Label: string option

    /// The edits to apply.
    Edit: WorkspaceEdit
  }

type ApplyWorkspaceEditResponse =
  {
    /// Indicates whether the edit was applied or not.
    Applied: bool

    /// An optional textual description for why the edit was not applied. This
    /// may be used by the server for diagnostic logging or to provide a
    /// suitable error for a request that triggered the edit.
    FailureReason: string option

    /// Depending on the client's failure handling strategy `failedChange` might
    /// contain the index of the change that failed. This property is only
    /// available if the client signals a `failureHandling` strategy in its
    /// client capabilities.
    FailedChange: uint
  }

/// Represents reasons why a text document is saved.
type TextDocumentSaveReason =
  /// Manually triggered, e.g. by the user pressing save, by starting debugging,
  /// or by an API call.
  | Manual = 1

  /// Automatic after a delay.
  | AfterDelay = 2

  /// When the editor lost focus.
  | FocusOut = 3

/// The parameters send in a will save text document notification.
type WillSaveTextDocumentParams =
  {
    /// The document that will be saved.
    TextDocument: TextDocumentIdentifier

    /// The 'TextDocumentSaveReason'.
    Reason: TextDocumentSaveReason
  }

type DidSaveTextDocumentParams =
  {
    /// The document that was saved.
    TextDocument: TextDocumentIdentifier

    /// Optional the content when saved. Depends on the includeText value
    /// when the save notification was requested.
    Text: string option
  }

type DidCloseTextDocumentParams =
  {
    /// The document that was closed.
    TextDocument: TextDocumentIdentifier
  }

/// Value-object describing what options formatting should use.
type FormattingOptions =
  {
    /// Size of a tab in spaces.
    TabSize: int
    /// Prefer spaces over tabs.
    InsertSpaces: bool
    /// Trim trailing whitespace on a line.
    ///
    /// @since 3.15.0
    TrimTrailingWhitespace: bool option
    /// Insert a newline character at the end of the file if one does not exist.
    ///
    /// @since 3.15.0
    InsertFinalNewline: bool option
    /// Trim all newlines after the final newline at the end of the file.
    ///
    /// @since 3.15.0
    TrimFinalNewlines: bool option
    /// Signature for further properties.
    [<JsonExtensionData>]
    mutable AdditionalData: IDictionary<string, JToken>
  }

  [<OnDeserialized>]
  member o.OnDeserialized(context: StreamingContext) =
    if isNull o.AdditionalData then
      o.AdditionalData <- Map.empty

type DocumentFormattingParams =
  {
    /// The document to format.
    TextDocument: TextDocumentIdentifier

    /// The format options.
    Options: FormattingOptions
  }

type DocumentRangeFormattingParams =
  {
    /// The document to format.
    TextDocument: TextDocumentIdentifier

    /// The range to format
    Range: Range

    /// The format options
    Options: FormattingOptions
  }

type DocumentOnTypeFormattingParams =
  {
    /// The document to format.
    TextDocument: TextDocumentIdentifier

    /// The position at which this request was sent.
    Position: Position

    /// The character that has been typed.
    Ch: char

    /// The format options.
    Options: FormattingOptions
  }

type DocumentSymbolParams =
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier
  }

type ITextDocumentPositionParams =
  /// The text document.
  abstract member TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  abstract member Position: Position

type TextDocumentPositionParams =
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier
    /// The position inside the text document.
    Position: Position
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = this.TextDocument
    member this.Position = this.Position

type ReferenceContext =
  {
    /// Include the declaration of the current symbol.
    IncludeDeclaration: bool
  }

type ReferenceParams =
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier
    /// The position inside the text document.
    Position: Position
    Context: ReferenceContext
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = this.TextDocument
    member this.Position = this.Position

/// A `MarkupContent` literal represents a string value which content is interpreted base on its
/// kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
///
/// If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
/// See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
///
/// Here is an example how such a string can be constructed using JavaScript / TypeScript:
/// ```ts
/// let markdown: MarkdownContent = {
///     kind: MarkupKind.Markdown,
///     value: [
///         '# Header',
///         'Some text',
///         '```typescript',
///         'someCode();',
///         '```'
///     ].join('\n')
/// };
/// ```
///
/// *Please Note* that clients might sanitize the return markdown. A client could decide to
/// remove HTML from the markdown to avoid script execution.
type MarkupContent =
  {
    /// The type of the Markup
    Kind: string

    // The content itself
    Value: string
  }

type MarkedStringData = { Language: string; Value: string }

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type MarkedString =
  | String of string
  | WithLanguage of MarkedStringData

let plaintext s = { Kind = MarkupKind.PlainText; Value = s }
let markdown s = { Kind = MarkupKind.Markdown; Value = s }

[<ErasedUnion>]
type HoverContent =
  | MarkedString of MarkedString
  | MarkedStrings of MarkedString[]
  | MarkupContent of MarkupContent

/// The result of a hover request.
type Hover =
  {
    /// The hover's content
    Contents: HoverContent

    /// An optional range is a range inside a text document
    /// that is used to visualize a hover, e.g. by changing the background color.
    Range: Range option
  }

/// An item to transfer a text document from the client to the server.
type TextDocumentItem =
  {
    /// The text document's URI.
    Uri: DocumentUri

    /// The text document's language identifier.
    LanguageId: string

    /// The version number of this document (it will increase after each
    /// change, including undo/redo).
    Version: int

    /// The content of the opened text document.
    Text: string
  }

type DidOpenTextDocumentParams =
  {
    /// The document that was opened.
    TextDocument: TextDocumentItem
  }

/// An event describing a change to a text document. If range and rangeLength are omitted
/// the new text is considered to be the full content of the document.
type TextDocumentContentChangeEvent =
  {
    /// The range of the document that changed.
    Range: Range option

    /// The length of the range that got replaced.
    RangeLength: int option

    /// The new text of the range/document.
    Text: string
  }

type DidChangeTextDocumentParams =
  {
    /// The document that did change. The version number points
    /// to the version after all provided content changes have
    /// been applied.
    TextDocument: VersionedTextDocumentIdentifier

    /// The actual content changes. The content changes describe single state changes
    /// to the document. So if there are two content changes c1 and c2 for a document
    /// in state S10 then c1 move the document to S11 and c2 to S12.
    ContentChanges: TextDocumentContentChangeEvent[]
  }

[<Flags>]
type WatchKind =
  | Create = 1
  | Change = 2
  | Delete = 4

type RelativePattern =
  {
    /// A workspace folder or a base URI to which this pattern will be matched
    /// against relatively.
    BaseUri: U2<DocumentUri, WorkspaceFolder>

    /// The actual glob pattern;
    Pattern: string
  }

type GlobPattern =
  | RelativePattern of RelativePattern
  | Pattern of string

type FileSystemWatcher =
  {
    /// The  glob pattern to watch
    GlobPattern: GlobPattern

    /// The kind of events of interest. If omitted it defaults
    /// to WatchKind.Create | WatchKind.Change | WatchKind.Delete
    /// which is 7.
    Kind: WatchKind option
  }

/// Describe options to be used when registered for text document change events.
type DidChangeWatchedFilesRegistrationOptions =
  {
    /// The watchers to register.
    Watchers: FileSystemWatcher[]
  }

/// How a completion was triggered
type CompletionTriggerKind =
  /// Completion was triggered by typing an identifier (24x7 code
  /// complete), manual invocation (e.g Ctrl+Space) or via API.
  | Invoked = 1
  /// Completion was triggered by a trigger character specified by
  /// the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
  | TriggerCharacter = 2

type CompletionContext =
  {
    ///  How the completion was triggered.
    triggerKind: CompletionTriggerKind

    /// The trigger character (a single character) that has trigger code complete.
    /// Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`
    triggerCharacter: char option
  }

type CompletionParams =
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier

    /// The position inside the text document.
    Position: Position

    /// The completion context. This is only available it the client specifies
    /// to send this using `ClientCapabilities.textDocument.completion.contextSupport === true`
    Context: CompletionContext option
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = this.TextDocument
    member this.Position = this.Position

/// Represents a reference to a command. Provides a title which will be used to represent a command in the UI.
/// Commands are identified by a string identifier. The protocol currently doesn't specify a set of well-known
/// commands. So executing a command requires some tool extension code.
type Command =
  {
    /// Title of the command, like `save`.
    Title: string

    /// The identifier of the actual command handler.
    Command: string

    /// Arguments that the command handler should be
    /// invoked with.
    Arguments: JToken[] option
  }

type CompletionItemLabelDetails =
  {
    /// An optional string which is rendered less prominently directly after
    /// {@link CompletionItem.label label}, without any spacing. Should be used
    /// for function signatures or type annotations.
    Detail: string option

    /// An optional string which is rendered less prominently after
    /// {@link CompletionItemLabelDetails.detail}. Should be used for fully
    /// qualified names or file path.
    Description: string option
  }

type InsertReplaceEdit =
  {
    /// The string to be inserted.
    NewText: string

    /// The range if the insert is requested
    Insert: Range

    /// The range if the replace is requested.
    Replace: Range
  }

/// Defines whether the insert text in a completion item should be interpreted as
/// plain text or a snippet.
type InsertTextFormat =
  /// The primary text to be inserted is treated as a plain string.
  | PlainText = 1
  /// The primary text to be inserted is treated as a snippet.
  ///
  /// A snippet can define tab stops and placeholders with `$1`, `$2`
  /// and `${3:foo}`. `$0` defines the final tab stop, it defaults to
  /// the end of the snippet. Placeholders with equal identifiers are linked,
  /// that is typing in one will update others too.
  | Snippet = 2

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type Documentation =
  | String of string
  | Markup of MarkupContent

type CompletionItem =
  {
    /// The label of this completion item. By default
    /// also the text that is inserted when selecting
    /// this completion.
    Label: string

    /// Additional details for the label
    LabelDetails: CompletionItemLabelDetails option

    /// The kind of this completion item. Based of the kind
    /// an icon is chosen by the editor.
    Kind: CompletionItemKind option

    /// Tags for this completion item.
    Tags: CompletionItemTag[] option

    /// A human-readable string with additional information
    /// about this item, like type or symbol information.
    Detail: string option

    /// A human-readable string that represents a doc-comment.
    Documentation: Documentation option

    /// Indicates if this item is deprecated.
    /// @deprecated Use `tags` instead if supported.
    Deprecated: bool option

    /// Select this item when showing.
    /// *Note* that only one completion item can be selected and that the
    /// tool / client decides which item that is. The rule is that the *first*
    /// item of those that match best is selected.
    Preselect: bool option

    /// A string that should be used when comparing this item
    /// with other items. When `falsy` the label is used.
    SortText: string option

    /// A string that should be used when filtering a set of
    /// completion items. When `falsy` the label is used.
    FilterText: string option

    /// A string that should be inserted into a document when selecting
    /// this completion. When `falsy` the label is used.
    ///
    /// The `insertText` is subject to interpretation by the client side.
    /// Some tools might not take the string literally. For example
    /// VS Code when code complete is requested in this example `con<cursor position>`
    /// and a completion item with an `insertText` of `console` is provided it
    /// will only insert `sole`. Therefore it is recommended to use `textEdit` instead
    /// since it avoids additional client side interpretation.
    ///
    /// @deprecated Use textEdit instead.
    InsertText: string option

    /// The format of the insert text. The format applies to both the `insertText` property
    /// and the `newText` property of a provided `textEdit`.
    InsertTextFormat: InsertTextFormat option

    /// How whitespace and indentation is handled during completion item
    /// insertion. If not provided the client's default value depends on the
    /// `textDocument.completion.insertTextMode` client capability.
    InsertTextMode: InsertTextMode option

    /// An edit which is applied to a document when selecting this completion. When an edit is provided the value of
    /// `insertText` is ignored.
    ///
    /// *Note:* The range of the edit must be a single line range and it must contain the position at which completion
    /// has been requested.
    TextEdit: U2<TextEdit, InsertReplaceEdit> option

    // The edit text used if the completion item is part of a CompletionList and
    /// CompletionList defines an item default for the text edit range.
    /// Clients will only honor this property if they opt into completion list
    /// item defaults using the capability `completionList.itemDefaults`.
    /// If not provided and a list's default range is provided the label
    /// property is used as a text.
    TextEditText: string option

    /// An optional array of additional text edits that are applied when
    /// selecting this completion. Edits must not overlap with the main edit
    /// nor with themselves.
    AdditionalTextEdits: TextEdit[] option

    /// An optional set of characters that when pressed while this completion is active will accept it first and
    /// then type that character. *Note* that all commit characters should have `length=1` and that superfluous
    /// characters will be ignored.
    CommitCharacters: char[] option

    /// An optional command that is executed *after* inserting this completion. *Note* that
    /// additional modifications to the current document should be described with the
    /// additionalTextEdits-property.
    Command: Command option

    /// An data entry field that is preserved on a completion item between
    /// a completion and a completion resolve request.
    Data: JToken option
  }

  static member Create(label: string) =
    { Label = label
      LabelDetails = None
      Kind = None
      Tags = None
      Detail = None
      Documentation = None
      Deprecated = None
      Preselect = None
      SortText = None
      FilterText = None
      InsertText = None
      InsertTextFormat = None
      InsertTextMode = None
      TextEdit = None
      TextEditText = None
      AdditionalTextEdits = None
      CommitCharacters = None
      Command = None
      Data = None }

type ReplaceEditRange = { Insert: Range; Replace: Range }

type ItemDefaults =
  {
    /// A default commit character set.
    CommitCharacters: char[] option

    /// A default edit range
    EditRange: U2<Range, ReplaceEditRange> option

    /// A default insert text format
    InsertTextFormat: InsertTextFormat option

    /// A default insert text mode
    InsertTextMode: InsertTextMode option

    /// A default data value.
    Data: LSPAny option
  }

type CompletionList =
  {
    /// This list it not complete. Further typing should result in recomputing
    /// this list.
    IsIncomplete: bool

    /// In many cases the items of an actual completion result share the same
    /// value for properties like `commitCharacters` or the range of a text
    /// edit. A completion list can therefore define item defaults which will be
    /// used if a completion item itself doesn't specify the value.
    /// If a completion list specifies a default value and a completion item
    /// also specifies a corresponding value the one from the item is used.
    /// Servers are only allowed to return default values if the client signals
    /// support for this via the `completionList.itemDefaults` capability.
    ItemDefaults: ItemDefaults option

    /// The completion items.
    Items: CompletionItem[]
  }

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type DiagnosticCode =
  | Number of int
  | String of string

[<RequireQualifiedAccess>]
type DiagnosticSeverity =
  ///  Reports an error.
  | Error = 1
  ///  Reports a warning.
  | Warning = 2
  ///  Reports an information.
  | Information = 3
  ///  Reports a hint.
  | Hint = 4

/// Represents a related message and source code location for a diagnostic. This should be
/// used to point to code locations that cause or related to a diagnostics, e.g when duplicating
/// a symbol in a scope.
type DiagnosticRelatedInformation = { Location: Location; Message: string }

// Structure to capture a description for an error code.
type CodeDescription =
  {
    // An URI to open with more information about the diagnostic error.
    Href: Uri option }

/// Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid in the
/// scope of a resource.
[<StructuredFormatDisplay("{DebuggerDisplay}")>]
type Diagnostic =
  {
    /// The range at which the message applies.
    Range: Range

    /// The diagnostic's severity. Can be omitted. If omitted it is up to the
    /// client to interpret diagnostics as error, warning, info or hint.
    Severity: DiagnosticSeverity option

    /// The diagnostic's code. Can be omitted.
    Code: string option
    /// An optional property to describe the error code.
    CodeDescription: CodeDescription option

    /// A human-readable string describing the source of this
    /// diagnostic, e.g. 'typescript' or 'super lint'.
    Source: string option

    /// The diagnostic's message.
    Message: string
    RelatedInformation: DiagnosticRelatedInformation[] option
    Tags: DiagnosticTag[] option
    /// A data entry field that is preserved between a
    /// `textDocument/publishDiagnostics` notification and
    /// `textDocument/codeAction` request.
    Data: JToken option
  }

  [<DebuggerBrowsable(DebuggerBrowsableState.Never); JsonIgnore>]
  member x.DebuggerDisplay =
    $"[{defaultArg x.Severity DiagnosticSeverity.Error}] ({x.Range.DebuggerDisplay}) {x.Message} ({defaultArg x.Code String.Empty})"

  override x.ToString() = x.DebuggerDisplay

type PublishDiagnosticsParams =
  {
    /// The URI for which diagnostic information is reported.
    Uri: DocumentUri

    /// Optional the version number of the document the diagnostics are published
    /// for.
    Version: int option

    /// An array of diagnostic information items.
    Diagnostics: Diagnostic[]
  }


type DocumentDiagnosticParams =
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier

    /// The additional identifier provided during registration.
    Identifier: string option

    /// The result id of a previous response if provided.
    PreviousResultId: string option
  }

module DocumentDiagnosticReportKind =
  /// A diagnostic report with a full set of problems.
  let Full = "full"
  /// A report indicating that the last returned report is still accurate.
  let Unchanged = "unchanged"

type FullDocumentDiagnosticReport =
  {
    /// A full document diagnostic report.
    Kind: string

    /// An optional result id. If provided it will be sent on the next
    /// diagnostic request for the same document.
    ResultId: string option

    /// The actual items.
    Items: Diagnostic[]
  }

type UnchangedDocumentDiagnosticReport =
  {
    ///  A document diagnostic report indicating no changes to the last result.
    /// A server can only return `unchanged` if result ids are provided.
    Kind: string

    /// A result id which will be sent on the next diagnostic request for the
    /// same document.
    ResultId: string
  }

type RelatedFullDocumentDiagnosticReport =
  {
    /// A full document diagnostic report.
    Kind: string

    /// An optional result id. If provided it will be sent on the next
    /// diagnostic request for the same document.
    ResultId: string option

    /// The actual items.
    Items: Diagnostic[]

    /// Diagnostics of related documents. This information is useful in
    /// programming languages where code in a file A can generate diagnostics in
    /// a file B which A depends on. An example of such a language is C/C++
    /// where marco definitions in a file a.cpp and result in errors in a header
    /// file b.hpp.
    RelatedDocuments: Map<DocumentUri, U2<FullDocumentDiagnosticReport, UnchangedDocumentDiagnosticReport>> option
  }

type RelatedUnchangedDocumentDiagnosticReport =
  {
    ///  A document diagnostic report indicating no changes to the last result.
    /// A server can only return `unchanged` if result ids are provided.
    Kind: string

    /// A result id which will be sent on the next diagnostic request for the
    /// same document.
    ResultId: string

    /// Diagnostics of related documents. This information is useful in
    /// programming languages where code in a file A can generate diagnostics in
    /// a file B which A depends on. An example of such a language is C/C++
    /// where marco definitions in a file a.cpp and result in errors in a header
    /// file b.hpp.
    RelatedDocuments: Map<DocumentUri, U2<FullDocumentDiagnosticReport, UnchangedDocumentDiagnosticReport>> option
  }

type DocumentDiagnosticReport =
  | RelatedFullDocumentDiagnosticReport of RelatedFullDocumentDiagnosticReport
  | RelatedUnchangedDocumentDiagnosticReport of RelatedUnchangedDocumentDiagnosticReport

type PreviousResultId =
  {
    /// The URI for which the client knows a result id.
    Uri: DocumentUri

    /// The value of the previous result id.
    Value: string
  }

type WorkspaceDiagnosticParams =
  {
    /// The additional identifier provided during registration.
    Identifier: string option

    /// The currently known diagnostic reports with their previous result ids.
    PreviousResultIds: PreviousResultId[]
  }

type WorkspaceFullDocumentDiagnosticReport =
  {
    /// A full document diagnostic report.
    Kind: string

    /// An optional result id. If provided it will be sent on the next
    /// diagnostic request for the same document.
    ResultId: string option

    /// The actual items.
    Items: Diagnostic[]

    /// The URI for which diagnostic information is reported.
    Uri: DocumentUri

    /// The version number for which the diagnostics are reported. If the
    /// document is not marked as open `null` can be provided.
    Version: int option
  }

type WorkspaceUnchangedDocumentDiagnosticReport =
  {
    ///  A document diagnostic report indicating no changes to the last result.
    /// A server can only return `unchanged` if result ids are provided.
    Kind: string

    /// A result id which will be sent on the next diagnostic request for the
    /// same document.
    ResultId: string

    /// The URI for which diagnostic information is reported.
    Uri: DocumentUri

    /// The version number for which the diagnostics are reported. If the
    /// document is not marked as open `null` can be provided.
    Version: int option
  }

type WorkspaceDocumentDiagnosticReport =
  | WorkspaceFullDocumentDiagnosticReport of WorkspaceFullDocumentDiagnosticReport
  | WorkspaceUnchangedDocumentDiagnosticReport of WorkspaceUnchangedDocumentDiagnosticReport

type WorkspaceDiagnosticReport = { Items: WorkspaceDocumentDiagnosticReport[] }


type CodeActionDisabled =
  {
    /// Human readable description of why the code action is currently
    /// disabled.
    ///
    /// This is displayed in the code actions UI.
    Reason: string
  }

/// A code action represents a change that can be performed in code, e.g. to fix a problem or
/// to refactor code.
///
/// A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed.
type CodeAction =
  {

    /// A short, human-readable, title for this code action.
    Title: string

    /// The kind of the code action.
    /// Used to filter code actions.
    Kind: string option

    /// The diagnostics that this code action resolves.
    Diagnostics: Diagnostic[] option

    /// Marks this as a preferred action. Preferred actions are used by the
    /// `auto fix` command and can be targeted by keybindings.
    ///
    /// * A quick fix should be marked preferred if it properly addresses the
    /// underlying error. A refactoring should be marked preferred if it is the
    /// most reasonable choice of actions to take.
    ///
    /// * @since 3.15.0
    IsPreferred: bool option

    /// Marks that the code action cannot currently be applied.
    ///
    /// Clients should follow the following guidelines regarding disabled code
    /// actions:
    ///
    /// - Disabled code actions are not shown in automatic lightbulbs code
    /// action menus.
    ///
    /// - Disabled actions are shown as faded out in the code action menu when
    /// the user request a more specific type of code action, such as
    /// refactorings.
    ///
    /// - If the user has a keybinding that auto applies a code action and only
    /// a disabled code actions are returned, the client should show the user
    /// an error message with `reason` in the editor.
    ///
    /// @since 3.16.0
    Disabled: CodeActionDisabled option

    /// The workspace edit this code action performs.
    Edit: WorkspaceEdit option

    /// A command this code action executes. If a code action
    /// provides an edit and a command, first the edit is
    /// executed and then the command.
    Command: Command option

    /// A data entry field that is preserved on a code action between
    /// a `textDocument/codeAction` and a `codeAction/resolve` request.
    Data: JToken option
  }

type TextDocumentCodeActionResult = U2<Command, CodeAction>[]

type RenameParams =
  {
    /// The document to rename.
    TextDocument: TextDocumentIdentifier

    /// The position at which this request was sent.
    Position: Position

    /// The new name of the symbol. If the given name is not valid the
    /// request must return a **ResponseError** with an
    /// appropriate message set.
    NewName: string
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = this.TextDocument
    member this.Position = this.Position

type PrepareRenameParams =
  {
    /// The document to rename.
    TextDocument: TextDocumentIdentifier

    /// The position at which this request was sent.
    Position: Position
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = this.TextDocument
    member this.Position = this.Position

type DefaultBehavior = { DefaultBehavior: bool }

type RangeWithPlaceholder = { Range: Range; Placeholder: string }

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type PrepareRenameResult =
  /// A range of the string to rename.
  | Range of Range
  /// A range of the string to rename and a placeholder text of the string content to be renamed.
  | RangeWithPlaceholder of RangeWithPlaceholder
  /// The rename position is valid and the client should use its default behavior to compute the rename range.
  | Default of DefaultBehavior

type LinkedEditingRanges =
  {
    /// A list of ranges that can be renamed together. The ranges must have
    /// identical length and contain identical text content. The ranges cannot
    /// overlap.
    Ranges: Range[]

    /// An optional word pattern (regular expression) that describes valid
    /// contents for the given ranges. If no pattern is provided, the client
    /// configuration's word pattern will be used.
    WordPattern: string option
  }

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type GotoResult =
  | Single of Location
  | Multiple of Location[]

/// A document highlight kind.
[<RequireQualifiedAccess>]
type DocumentHighlightKind =
  /// A textual occurrence.
  | Text = 1

  /// Read-access of a symbol, like reading a variable.
  | Read = 2

  /// Write-access of a symbol, like writing to a variable.
  | Write = 3

/// A document highlight is a range inside a text document which deserves
/// special attention. Usually a document highlight is visualized by changing
/// the background color of its range.
type DocumentHighlight =
  {
    /// The range this highlight applies to.
    Range: Range

    /// The highlight kind, default is DocumentHighlightKind.Text.
    Kind: DocumentHighlightKind option
  }

type DocumentLinkParams =
  {
    /// The document to provide document links for.
    TextDocument: TextDocumentIdentifier
  }

/// A document link is a range in a text document that links to an internal or external resource, like another
/// text document or a web site.
type DocumentLink =
  {
    /// The range this link applies to.
    Range: Range

    /// The uri this link points to. If missing a resolve request is sent later.
    Target: DocumentUri option

    /// The tooltip text when you hover over this link.
    /// If a tooltip is provided, is will be displayed in a string that includes
    /// instructions on how to trigger the link, such as `{0} (ctrl + click)`.
    /// The specific instructions vary depending on OS, user settings, and
    /// localization.
    Tooltip: string option

    /// A data entry field that is preserved on a document link between a
    /// DocumentLinkRequest and a DocumentLinkResolveRequest.
    Data: JToken option
  }

type DocumentColorParams =
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier
  }

/// Represents a color in RGBA space.
type Color =
  {
    /// The red component of this color in the range [0-1].
    Red: float

    /// The green component of this color in the range [0-1].
    Green: float

    /// The blue component of this color in the range [0-1].
    Blue: float

    /// The alpha component of this color in the range [0-1].
    Alpha: float
  }

type ColorInformation =
  {
    /// The range in the document where this color appears.
    Range: Range

    /// The actual color value for this color range.
    Color: Color
  }

type ColorPresentationParams =
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier

    /// The color information to request presentations for.
    ColorInfo: Color

    /// The range where the color would be inserted. Serves as a context.
    Range: Range
  }

type ColorPresentation =
  {
    /// The label of this color presentation. It will be shown on the color
    /// picker header. By default this is also the text that is inserted when selecting
    /// this color presentation.
    Label: string

    /// An edit which is applied to a document when selecting
    /// this presentation for the color.  When `falsy` the label
    /// is used.
    TextEdit: TextEdit option

    /// An optional array of additional text edits that are applied when
    /// selecting this color presentation. Edits must not overlap with the main edit nor with themselves.
    AdditionalTextEdits: TextEdit[] option
  }

type CodeActionKind = string

type CodeActionTriggerKind =
  /// Code actions were explicitly requested by the user or by an extension.
  | Invoked = 1
  /// Code actions were requested automatically.
  /// This typically happens when current selection in a file changes, but can also be triggered when file content changes.
  | Automatic = 2

/// Contains additional diagnostic information about the context in which
/// a code action is run.
type CodeActionContext =
  {
    /// An array of diagnostics.
    Diagnostics: Diagnostic[]
    /// Requested kind of actions to return.
    /// Actions not of this kind are filtered out by the client before being
    /// shown. So servers can omit computing them.
    Only: CodeActionKind[] option
    /// The reason why code actions were requested.
    /// @since 3.17.0
    TriggerKind: CodeActionTriggerKind option
  }

/// Params for the CodeActionRequest
type CodeActionParams =
  {
    /// The document in which the command was invoked.
    TextDocument: TextDocumentIdentifier

    /// The range for which the command was invoked.
    Range: Range

    /// Context carrying additional information.
    Context: CodeActionContext
  }

type CodeLensParams =
  {
    /// The document to request code lens for.
    TextDocument: TextDocumentIdentifier
  }

/// A code lens represents a command that should be shown along with
/// source text, like the number of references, a way to run tests, etc.
///
/// A code lens is _unresolved_ when no command is associated to it. For performance
/// reasons the creation of a code lens and resolving should be done in two stages.
type CodeLens =
  {
    /// The range in which this code lens is valid. Should only span a single line.
    Range: Range

    /// The command this code lens represents.
    Command: Command option

    /// A data entry field that is preserved on a code lens item between
    /// a code lens and a code lens resolve request.
    Data: JToken option
  }

/// Represents a parameter of a callable-signature. A parameter can
/// have a label and a doc-comment.
type ParameterInformation =
  {
    /// The label of this parameter information.
    /// Either a string or an inclusive start and exclusive end offsets within
    /// its containing signature label. (see SignatureInformation.label). The
    /// offsets are based on a UTF-16 string representation as `Position` and
    /// `Range` does.
    /// *Note*: a label of type string should be a substring of its containing
    /// signature label. Its intended use case is to highlight the parameter
    /// label part in the `SignatureInformation.label`.
    Label: U2<string, (uint * uint)>

    /// The human-readable doc-comment of this parameter. Will be shown
    /// in the UI but can be omitted.
    Documentation: Documentation option
  }

///Represents the signature of something callable. A signature
/// can have a label, like a function-name, a doc-comment, and
/// a set of parameters.
type SignatureInformation =
  {
    /// The label of this signature. Will be shown in
    /// the UI.
    Label: string

    /// The human-readable doc-comment of this signature. Will be shown
    /// in the UI but can be omitted.
    Documentation: Documentation option

    /// The parameters of this signature.
    Parameters: ParameterInformation[] option

    /// The index of the active parameter.
    /// If provided, this is used in place of `SignatureHelp.activeParameter`.
    ActiveParameter: uint option
  }

/// Signature help represents the signature of something
/// callable. There can be multiple signature but only one
/// active and only one active parameter.
type SignatureHelp =
  {
    /// One or more signatures.
    Signatures: SignatureInformation[]

    /// The active signature. If omitted or the value lies outside the
    /// range of `signatures` the value defaults to zero or is ignored if
    /// `signatures.length === 0`. Whenever possible implementors should
    /// make an active decision about the active signature and shouldn't
    /// rely on a default value.
    /// In future version of the protocol this property might become
    /// mandatory to better express this.
    ActiveSignature: int option

    /// The active parameter of the active signature. If omitted or the value
    /// lies outside the range of `signatures[activeSignature].parameters`
    /// defaults to 0 if the active signature has parameters. If
    /// the active signature has no parameters it is ignored.
    /// In future version of the protocol this property might become
    /// mandatory to better express the active parameter if the
    /// active signature does have any.
    ActiveParameter: uint option
  }

type SignatureHelpTriggerKind =
  /// manually invoked via command
  | Invoked = 1
  /// trigger by a configured trigger character
  | TriggerCharacter = 2
  /// triggered by cursor movement or document content changing
  | ContentChange = 3

type SignatureHelpContext =
  {
    /// action that caused signature help to be triggered
    TriggerKind: SignatureHelpTriggerKind
    /// character that caused signature help to be triggered. None when kind is not TriggerCharacter.
    TriggerCharacter: char option
    /// true if signature help was already showing when this was triggered
    IsRetrigger: bool
    /// the current active SignatureHelp
    ActiveSignatureHelp: SignatureHelp option

  }

type SignatureHelpParams =
  {
    /// the text document
    TextDocument: TextDocumentIdentifier
    /// the position inside the text document
    Position: Position
    /// Additional information about the context in which a signature help request was triggered.
    Context: SignatureHelpContext option
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = this.TextDocument
    member this.Position = this.Position

type FoldingRangeParams =
  {
    /// the document to generate ranges for
    TextDocument: TextDocumentIdentifier
  }

module FoldingRangeKind =
  let Comment = "comment"
  let Imports = "imports"
  let Region = "region"

type FoldingRange =
  {
    /// The zero-based line number from where the folded range starts.
    StartLine: int

    /// The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.
    StartCharacter: int option

    /// The zero-based line number where the folded range ends.
    EndLine: int

    /// The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.
    EndCharacter: int option

    /// Describes the kind of the folding range such as 'comment' or 'region'. The kind
    /// is used to categorize folding ranges and used by commands like 'Fold all comments'. See
    /// [FoldingRangeKind](#FoldingRangeKind) for an enumeration of standardized kinds.
    Kind: string option

    /// The text that the client should show when the specified range is
    /// collapsed. If not defined or not supported by the client, a default will
    /// be chosen by the client.
    CollapsedText: string option
  }

type SelectionRangeParams =
  {
    /// The document to generate ranges for
    TextDocument: TextDocumentIdentifier

    /// The positions inside the text document.
    Positions: Position[]
  }

type SelectionRange =
  {
    /// The range of this selection range.
    Range: Range

    /// The parent selection range containing this range. Therefore `parent.range` must contain `this.range`.
    Parent: SelectionRange option
  }

type CallHierarchyPrepareParams =
  {
    TextDocument: TextDocumentIdentifier

    /// The position at which this request was sent.
    Position: Position
  }

type HierarchyItem =
  {
    /// The name of this item.
    Name: string

    /// The kind of this item.
    Kind: SymbolKind

    /// Tags for this item.
    Tags: SymbolTag[] option

    /// More detail for this item, e.g., the signature of a function.
    Detail: string option

    /// The resource identifier of this item.
    Uri: DocumentUri

    /// The range enclosing this symbol not including leading/trailing
    /// whitespace but everything else, e.g., comments and code.
    Range: Range

    /// The range that should be selected and revealed when this symbol is being
    /// picked, e.g., the name of a function. Must be contained by the [`range`].
    SelectionRange: Range

    /// A data entry field that is preserved between a call hierarchy prepare
    /// and incoming calls or outgoing calls requests.
    Data: JToken option
  }

type CallHierarchyItem = HierarchyItem

type CallHierarchyIncomingCallsParams = { Item: CallHierarchyItem }

type CallHierarchyIncomingCall =
  {
    /// The item that makes the call.
    From: CallHierarchyItem

    /// The ranges at which the calls appear. This is relative to the caller
    /// denoted by [`this.From`].
    FromRanges: Range[]
  }

type CallHierarchyOutgoingCallsParams = { Item: CallHierarchyItem }

type CallHierarchyOutgoingCall =
  {
    /// The item that is called
    To: CallHierarchyItem

    /// The range at which this item is called. This is the range relative to
    /// the caller, e.g., the item passed to `callHierarchy/outgoingCalls`
    /// request.
    FromRanges: Range[]
  }

type TypeHierarchyPrepareParams =
  {
    TextDocument: TextDocumentIdentifier

    /// The position at which this request was sent.
    Position: Position
  }

type TypeHierarchyItem = HierarchyItem

type TypeHierarchySupertypesParams = { Item: TypeHierarchyItem }

type TypeHierarchySubtypesParams = { Item: TypeHierarchyItem }

type SemanticTokensParams = { TextDocument: TextDocumentIdentifier }

type SemanticTokensDeltaParams =
  {
    TextDocument: TextDocumentIdentifier
    /// The result id of a previous response. The result Id can either point to
    /// a full response or a delta response depending on what was received last.
    PreviousResultId: string
  }

type SemanticTokensRangeParams = { TextDocument: TextDocumentIdentifier; Range: Range }

type SemanticTokens =
  {
    /// An optional result id. If provided and clients support delta updating
    /// the client will include the result id in the next semantic token request.
    /// A server can then instead of computing all semantic tokens again simply
    /// send a delta.
    ResultId: string option
    Data: uint32[]
  }

type SemanticTokensEdit =
  {
    /// The start offset of the edit.
    Start: uint32

    /// The count of elements to remove.
    DeleteCount: uint32

    /// The elements to insert.
    Data: uint32[] option
  }

type SemanticTokensDelta =
  {
    ResultId: string option

    /// The semantic token edits to transform a previous result into a new result.
    Edits: SemanticTokensEdit[]
  }

/// Represents information on a file/folder create.
///@since 3.16.0
type FileCreate =
  {
    /// A file:// URI for the location of the file/folder being created.
    Uri: string
  }

/// The parameters sent in notifications/requests for user-initiated creation of files.
/// @since 3.16.0
type CreateFilesParams =
  {
    /// An array of all files/folders created in this operation.
    Files: FileCreate[]
  }

/// Represents information on a file/folder rename.
/// @since 3.16.0
type FileRename =
  {
    /// A file:// URI for the original location of the file/folder being renamed.
    OldUri: string
    /// A file:// URI for the new location of the file/folder being renamed.
    NewUri: string
  }

/// The parameters sent in notifications/requests for user-initiated renames of files.
/// @since 3.16.0
type RenameFilesParams =
  {
    /// An array of all files/folders renamed in this operation. When a folder is renamed,
    /// only the folder will be included, and not its children.
    Files: FileRename[]
  }

/// Represents information on a file/folder create.
///@since 3.16.0
type FileDelete =
  {
    /// A file:// URI for the location of the file/folder being deleted.
    Uri: string
  }

/// The parameters sent in notifications/requests for user-initiated deletes of files.
/// @since 3.16.0
type DeleteFilesParams =
  {
    /// An array of all files/folders deleted in this operation.
    Files: FileDelete[]
  }


/// A parameter literal used in inlay hint requests.
type InlayHintParams = (*WorkDoneProgressParams &*)
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier
    /// The visible document range for which inlay hints should be computed.
    Range: Range
  }

/// Inlay hint kinds.
[<RequireQualifiedAccess>]
type InlayHintKind =
  /// An inlay hint that for a type annotation.
  | Type = 1
  /// An inlay hint that is for a parameter.
  | Parameter = 2

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type InlayHintTooltip =
  | String of string
  | Markup of MarkupContent

/// An inlay hint label part allows for interactive and composite labels
/// of inlay hints.
type InlayHintLabelPart =
  {
    /// The value of this label part.
    Value: string
    /// The tooltip text when you hover over this label part. Depending on
    /// the client capability `inlayHint.resolveSupport` clients might resolve
    /// this property late using the resolve request.
    Tooltip: InlayHintTooltip option
    /// An optional source code location that represents this
    /// label part.
    ///
    /// The editor will use this location for the hover and for code navigation
    /// features: This part will become a clickable link that resolves to the
    /// definition of the symbol at the given location (not necessarily the
    /// location itself), it shows the hover that shows at the given location,
    /// and it shows a context menu with further code navigation commands.
    ///
    /// Depending on the client capability `inlayHint.resolveSupport` clients
    /// might resolve this property late using the resolve request.
    Location: Location option
    /// An optional command for this label part.
    ///
    /// Depending on the client capability `inlayHint.resolveSupport` clients
    /// might resolve this property late using the resolve request.
    Command: Command option
  }

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type InlayHintLabel =
  | String of string
  | Parts of InlayHintLabelPart[]

/// Inlay hint information.
type InlayHint =
  {
    /// The position of this hint.
    Position: Position
    /// The label of this hint. A human readable string or an array of
    /// InlayHintLabelPart label parts.
    ///
    /// *Note* that neither the string nor the label part can be empty.
    Label: InlayHintLabel
    /// he kind of this hint. Can be omitted in which case the client
    /// should fall back to a reasonable default.
    Kind: InlayHintKind option
    /// Optional text edits that are performed when accepting this inlay hint.
    ///
    /// *Note* that edits are expected to change the document so that the inlay
    /// hint (or its nearest variant) is now part of the document and the inlay
    /// hint itself is now obsolete.
    ///
    /// Depending on the client capability `inlayHint.resolveSupport` clients
    /// might resolve this property late using the resolve request.
    TextEdits: TextEdit[] option
    /// The tooltip text when you hover over this item.
    ///
    /// Depending on the client capability `inlayHint.resolveSupport` clients
    /// might resolve this property late using the resolve request.
    Tooltip: InlayHintTooltip option
    /// Render padding before the hint.
    ///
    /// Note: Padding should use the editor's background color, not the
    /// background color of the hint itself. That means padding can be used
    /// to visually align/separate an inlay hint.
    PaddingLeft: bool option
    /// Render padding after the hint.
    ///
    /// Note: Padding should use the editor's background color, not the
    /// background color of the hint itself. That means padding can be used
    /// to visually align/separate an inlay hint.
    PaddingRight: bool option

    /// A data entry field that is preserved on a inlay hint between
    /// a `textDocument/inlayHint` and a `inlayHint/resolve` request.
    Data: LSPAny option
  }

/// InlineValue Context
type InlineValueContext =
  {
    /// The stack frame (as a DAP Id) where the execution has stopped.
    FrameId: int

    /// The document range where execution has stopped.
    /// Typically the end position of the range denotes the line where the inline values are shown.
    StoppedLocation: Range
  }

/// A parameter literal used in inline value requests.
type InlineValueParams = (*WorkDoneProgressParams &*)
  {
    /// The text document.
    TextDocument: TextDocumentIdentifier
    /// The visible document range for which inline values should be computed.
    Range: Range
    /// Additional information about the context in which inline values were requested
    Context: InlineValueContext
  }

/// Provide inline value as text.
type InlineValueText =
  {
    /// The document range for which the inline value applies.
    Range: Range
    /// The text of the inline value.
    Text: String
  }

type InlineValueVariableLookup =
  {
    /// The document range for which the inline value applies. The range is used
    /// to extract the variable name from the underlying document.
    Range: Range

    /// If specified the name of the variable to look up.
    VariableName: string option

    /// How to perform the lookup.
    CaseSensitiveLookup: bool
  }

type InlineValueEvaluatableExpression =
  {
    /// The document range for which the inline value applies. The range is used
    /// to extract the evaluatable expression from the underlying document.
    Range: Range

    /// If specified the expression overrides the extracted expression.
    Expression: string option
  }

[<ErasedUnion>]
[<RequireQualifiedAccess>]
type InlineValue =
  | InlineValueText of InlineValueText
  | InlineValueVariableLookup of InlineValueVariableLookup
  | InlineValueEvaluatableExpression of InlineValueEvaluatableExpression


module UniquenessLevel =
  /// The moniker is only unique inside a document
  let Document = "document"
  /// The moniker is unique inside a project for which a dump got created
  let Project = "project"
  /// The moniker is unique inside the group to which a project belongs
  let Group = "group"
  /// The moniker is unique inside the moniker scheme.
  let Scheme = "scheme"
  /// The moniker is globally unique
  let Global = "global"

module MonikerKind =
  /// The moniker represent a symbol that is imported into a project
  let Import = "import"
  /// The moniker represents a symbol that is exported from a project
  let Export = "export"
  /// The moniker represents a symbol that is local to a project (e.g. a local
  /// variable of a function, a class not visible outside the project, ...)
  let Local = "local"

type Moniker =
  {
    /// The scheme of the moniker. For example tsc or .Net
    Scheme: string

    /// The identifier of the moniker. The value is opaque in LSIF however schema owners are allowed
    /// to define the structure if they want.
    Identifier: string

    /// The scope in which the moniker is unique
    Unique: string

    /// The moniker kind if known.
    Kind: string option
  }


type ProgressToken = U2<int, string>

type WorkDoneProgressCreateParams =
  {
    ///The token to be used to report progress.
    token: ProgressToken
  }

/// The base protocol offers also support to report progress in a generic fashion.
/// This mechanism can be used to report any kind of progress including work done progress
/// (usually used to report progress in the user interface using a progress bar) and partial
/// result progress to support streaming of results.
type ProgressParams<'T> =
  {
    /// The progress token provided by the client or server.
    token: ProgressToken
    /// The progress data.
    value: 'T
  }

type WorkDoneProgressKind =
  | Begin
  | Report
  | End

  override x.ToString() =
    match x with
    | Begin -> "begin"
    | Report -> "report"
    | End -> "end"

type WorkDoneProgressEnd =
  {
    /// WorkDoneProgressKind.End
    kind: string
    /// Optional, a final message indicating to for example indicate the outcome of the operation.
    message: string option
  }

  static member Create(?message) = { kind = WorkDoneProgressKind.End.ToString(); message = message }

type WorkDoneProgressBegin =
  {
    /// WorkDoneProgressKind.Begin
    kind: string

    /// Mandatory title of the progress operation. Used to briefly inform about
    /// the kind of operation being performed.
    ///
    /// Examples: "Indexing" or "Linking dependencies".
    title: string option
    ///  Controls if a cancel button should show to allow the user to cancel the
    ///  long running operation. Clients that don't support cancellation are allowed
    ///  to ignore the setting.
    cancellable: bool option
    /// Optional, more detailed associated progress message. Contains
    /// complementary information to the `title`.
    ///
    /// Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
    /// If unset, the previous progress message (if any) is still valid.
    message: string option
    /// Optional progress percentage to display (value 100 is considered 100%).
    /// If not provided infinite progress is assumed and clients are allowed
    /// to ignore the `percentage` value in subsequent in report notifications.
    ///
    /// The value should be steadily rising. Clients are free to ignore values
    /// that are not following this rule. The value range is [0, 100].
    percentage: uint option
  }

  static member Create(title, ?cancellable, ?message, ?percentage) =
    { kind = WorkDoneProgressKind.Begin.ToString()
      title = Some title
      cancellable = cancellable
      message = message
      percentage = percentage }

type WorkDoneProgressReport =
  {
    /// WorkDoneProgressKind.Report
    kind: string
    /// Controls enablement state of a cancel button.
    ///
    /// Clients that don't support cancellation or don't support controlling the button's
    /// enablement state are allowed to ignore the property.
    cancellable: bool option
    /// Optional, more detailed associated progress message. Contains
    /// complementary information to the `title`.
    ///
    /// Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
    /// If unset, the previous progress message (if any) is still valid.
    message: string option
    /// Optional progress percentage to display (value 100 is considered 100%).
    /// If not provided infinite progress is assumed and clients are allowed
    /// to ignore the `percentage` value in subsequent in report notifications.
    ///
    /// The value should be steadily rising. Clients are free to ignore values
    /// that are not following this rule. The value range is [0, 100].
    percentage: uint option
  }

  static member Create(?cancellable, ?message, ?percentage) =
    { kind = WorkDoneProgressKind.Report.ToString()
      cancellable = cancellable
      message = message
      percentage = percentage }

/// The token to be used to report progress.
type WorkDoneProgressCancelParams = { token: ProgressToken }