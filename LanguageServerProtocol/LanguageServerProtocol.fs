module Ionide.LanguageServerProtocol

open System.Diagnostics


[<AutoOpen>]
module LspJsonConverters =
    open Microsoft.FSharp.Reflection
    open Newtonsoft.Json
    open System
    open System.Collections.Concurrent

    let inline memorise (f: 'a -> 'b) : ('a -> 'b) =
        let d = ConcurrentDictionary<'a, 'b>()
        fun key ->
            d.GetOrAdd(key, f)

    type ErasedUnionAttribute() =
        inherit Attribute()

    [<ErasedUnion>]
    type U2<'a, 'b> = First of 'a | Second of 'b

    type ErasedUnionConverter() =
        inherit JsonConverter()

        let canConvert =
            memorise (fun t ->
                if not (FSharpType.IsUnion t) then
                    false
                else
                    t.BaseType.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0)

        override __.CanConvert(t) = canConvert t

        override __.WriteJson(writer, value, serializer) =
            let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
            let unionField = fields.[0]
            serializer.Serialize(writer, unionField)

        override __.ReadJson(_reader, _t, _existingValue, _serializer) =
            failwith "Not implemented"

    /// converter that can convert enum-style DUs
    type SingleCaseUnionConverter() =
      inherit JsonConverter()


      let canConvert =
        let allCases (t: System.Type) =
          FSharpType.GetUnionCases t
        memorise (fun t ->
          FSharpType.IsUnion t
          && allCases t |> Array.forall (fun c -> c.GetFields().Length = 0)
        )

      override _.CanConvert t = canConvert t

      override _.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: obj, serializer: Newtonsoft.Json.JsonSerializer) =
        serializer.Serialize(writer, string value)

      override _.ReadJson(reader: Newtonsoft.Json.JsonReader, t, _existingValue, serializer) =
        let caseName = string reader.Value
        match FSharpType.GetUnionCases(t) |> Array.tryFind (fun c -> c.Name.Equals(caseName, StringComparison.OrdinalIgnoreCase)) with
        | Some caseInfo ->
          FSharpValue.MakeUnion(caseInfo, [||])
        | None ->
          failwith $"Could not create an instance of the type '%s{t.Name}' with the name '%s{caseName}'"

    type U2BoolObjectConverter() =
      inherit JsonConverter()

      let canConvert =
        memorise (fun (t: System.Type) ->
          t.IsGenericType
          && t.GetGenericTypeDefinition() = typedefof<U2<_, _>>
          && t.GetGenericArguments().Length = 2
          && t.GetGenericArguments().[0] = typeof<bool>
          && not (t.GetGenericArguments().[1].IsValueType)
        )

      override _.CanConvert t = canConvert t

      override _.WriteJson(writer, value, serializer) =
        let case, fields = FSharpValue.GetUnionFields(value, value.GetType())
        match case.Name with
        | "First" ->
          writer.WriteValue(value :?> bool)
        | "Second" ->
          serializer.Serialize(writer, fields.[0])
        | _ ->
          failwith $"Unrecognized case '{case.Name}' for union type '{value.GetType().FullName}'."

      override _.ReadJson(reader, t, _existingValue, serializer) =
        let cases = FSharpType.GetUnionCases(t)
        match reader.TokenType with
        | JsonToken.Boolean ->
          // 'First' side
          FSharpValue.MakeUnion(cases.[0], [| box(reader.Value :?> bool) |])
        | JsonToken.StartObject ->
          // Second side
          let value = serializer.Deserialize(reader, (t.GetGenericArguments().[1]))
          FSharpValue.MakeUnion(cases.[1], [| value |])
        | _ ->
          failwithf $"Unrecognized json TokenType '%s{string reader.TokenType}' when reading value of type '{t.FullName}'"

    type OptionConverter() =
        inherit JsonConverter()

        override __.CanConvert(t) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

        override __.WriteJson(writer, value, serializer) =
            let value =
                if isNull value then null
                else
                    let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                    fields.[0]
            serializer.Serialize(writer, value)

        override __.ReadJson(reader, t, _existingValue, serializer) =
            let innerType = t.GetGenericArguments().[0]
            let innerType =
                if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
                else innerType
            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases(t)
            if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
            else FSharpValue.MakeUnion(cases.[1], [|value|])

module Types =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open System

    type TextDocumentSyncKind =
        | None = 0
        | Full = 1
        | Incremental = 2

    type DocumentFilter = {
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
    [<DebuggerDisplay("{DebuggerDisplay}")>]
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
      [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
      member x.DebuggerDisplay =
        $"({x.Line},{x.Character})"

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
    [<DebuggerDisplay("{DebuggerDisplay}")>]
    type Range =
      {
          /// The range's start position.
          Start: Position

          /// The range's end position.
          End: Position
      }
      [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
      member x.DebuggerDisplay =
        $"{x.Start.DebuggerDisplay}-{x.End.DebuggerDisplay}"

    type DocumentUri = string

    /// Represents a location inside a resource, such as a line inside a text file.
    type Location = {
        Uri: DocumentUri
        Range: Range
    }

    type ITextDocumentIdentifier =
        /// Warning: normalize this member by UrlDecoding it before use
        abstract member Uri : DocumentUri with get

    type TextDocumentIdentifier =
        {
            /// The text document's URI.
            Uri: DocumentUri
        }
        interface ITextDocumentIdentifier with
            member this.Uri with get() = this.Uri

    type VersionedTextDocumentIdentifier =
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
            member this.Uri with get() = this.Uri

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

    /// Represents information about programming constructs like variables, classes,
    /// interfaces etc.
    type SymbolInformation = {
        /// The name of this symbol.
        Name: string

        /// The kind of this symbol.
        Kind: SymbolKind

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

    /// A textual edit applicable to a text document.
    type TextEdit = {
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
    type TextDocumentEdit = {
        /// The text document to change.
        TextDocument: VersionedTextDocumentIdentifier

        /// The edits to be applied.
        Edits: TextEdit[]
    }

    type TraceSetting =
        | Off = 0
        | Messages = 1
        | Verbose = 2

    /// Capabilities for methods that support dynamic registration.
    type DynamicCapabilities = {
        /// Method supports dynamic registration.
        DynamicRegistration: bool option
    }

    type ResourceOperationKind = Create | Rename | Delete

    type FailureHandlingKind = Abort | Transactional | Undo | TextOnlyTransactional

    type ChangeAnnotationSupport = {
      GroupsOnLabel: bool option
    }

    /// Capabilities specific to `WorkspaceEdit`s
    type WorkspaceEditCapabilities = {
        /// The client supports versioned document changes in `WorkspaceEdit`s
        DocumentChanges: bool option
        /// The resource operations the client supports. Clients should at least
        /// support 'create', 'rename' and 'delete' files and folders.
        ResourceOperations: ResourceOperationKind [] option
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
    type SymbolKindCapabilities = {
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
    with
        static member DefaultValueSet =
            [|
                SymbolKind.File
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
                SymbolKind.Array
            |]

    /// Capabilities specific to the `workspace/symbol` request.
    type SymbolCapabilities = {
        /// Symbol request supports dynamic registration.
        DynamicRegistration: bool option

        /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
        SymbolKind: SymbolKindCapabilities option
    }

    type SemanticTokensWorkspaceClientCapabilities = {
      /// Whether the client implementation supports a refresh request sent from
      /// the server to the client.
      ///
      /// Note that this event is global and will force the client to refresh all
      /// semantic tokens currently shown. It should be used with absolute care
      /// and is useful for situation where a server for example detect a project
      /// wide change that requires such a calculation.
      RefreshSupport: bool option
    }

    /// Workspace specific client capabilities.
    type WorkspaceClientCapabilities = {
        /// The client supports applying batch edits to the workspace by supporting
        /// the request 'workspace/applyEdit'
        ApplyEdit: bool option

        /// Capabilities specific to `WorkspaceEdit`s
        WorkspaceEdit: WorkspaceEditCapabilities option

        /// Capabilities specific to the `workspace/didChangeConfiguration` notification.
        DidChangeConfiguration: DynamicCapabilities option

        /// Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
        DidChangeWatchedFiles: DynamicCapabilities option

        /// Capabilities specific to the `workspace/symbol` request.
        Symbol: SymbolCapabilities option

        SemanticTokens: SemanticTokensWorkspaceClientCapabilities option
    }

    type SynchronizationCapabilities = {
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

    type HoverCapabilities = {
        /// Whether hover synchronization supports dynamic registration.
        DynamicRegistration: bool option

        /// Client supports the follow content formats for the content
        /// property. The order describes the preferred format of the client.
        /// See `MarkupKind` for common values
        ContentFormat: string[] option
    }

    type CompletionItemCapabilities = {
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

    type CompletionItemKindCapabilities = {
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
    with
        static member DefaultValueSet =
            [|
                CompletionItemKind.Text
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
                CompletionItemKind.Reference
            |]

    /// Capabilities specific to the `textDocument/completion`
    type CompletionCapabilities = {
        /// Whether completion supports dynamic registration.
        DynamicRegistration: bool option

        /// The client supports the following `CompletionItem` specific
        /// capabilities.
        CompletionItem: CompletionItemCapabilities option

        CompletionItemKind: CompletionItemKindCapabilities option

        /// The client supports to send additional context information for a
        /// `textDocument/completion` request.
        ContextSupport: bool option
    }

    type SignatureInformationCapabilities = {
        /// Client supports the follow content formats for the documentation
        /// property. The order describes the preferred format of the client.
        /// See `MarkupKind` for common values
        DocumentationFormat: string[] option
    }

    type SignatureHelpCapabilities = {
        /// Whether signature help supports dynamic registration.
        DynamicRegistration: bool option

        /// The client supports the following `SignatureInformation`
        /// specific properties.
        SignatureInformation: SignatureInformationCapabilities option
    }

    /// capabilities specific to the `textDocument/documentSymbol`
    type DocumentSymbolCapabilities = {
        /// Whether document symbol supports dynamic registration.
        DynamicRegistration: bool option

        /// Specific capabilities for the `SymbolKind`.
        SymbolKind: SymbolKindCapabilities option
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

    type CodeActionClientCapabilityLiteralSupportCodeActionKind = {
        /// The code action kind values the client supports. When this
        /// property exists the client also guarantees that it will
        /// handle values outside its set gracefully and falls back
        /// to a default value when unknown.
        /// See `CodeActionKind` for common values
        ValueSet: string[]
    }

    type CodeActionClientCapabilityLiteralSupport = {
        /// The code action kind is supported with the following value set.
        CodeActionKind: CodeActionClientCapabilityLiteralSupportCodeActionKind
    }

    type CodeActionClientCapabilityResolveSupport = {
        /// The properties that a client can resolve lazily.
        Properties: string[]
    };

    /// capabilities specific to the `textDocument/codeAction`
    type CodeActionClientCapabilities = {
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
        ResolveSupport: CodeActionClientCapabilityResolveSupport option

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

    type DiagnosticTagSupport = {

        /// Represents the tags supported by the client
        ValueSet: DiagnosticTag[]
    }

    /// Capabilities specific to `textDocument/publishDiagnostics`.
    type PublishDiagnosticsCapabilites = {

        /// Whether the clients accepts diagnostics with related information.
        RelatedInformation: bool option

        /// Client supports the tag property to provide meta data about a diagnostic.
        TagSupport: DiagnosticTagSupport option
    }

    type FoldingRangeCapabilities =  {
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
    }

    type SemanticTokenFullRequestType = {
      /// The client will send the `textDocument/semanticTokens/full/delta`
      /// request if the server provides a corresponding handler.
      Delta: bool option
    }

    type SemanticTokensRequests = {
        /// The client will send the `textDocument/semanticTokens/range` request
        /// if the server provides a corresponding handler.
        Range: U2<bool, obj> option

        /// The client will send the `textDocument/semanticTokens/full` request
        /// if the server provides a corresponding handler.
        Full: U2<bool, SemanticTokenFullRequestType> option
    }

    type TokenFormat =
    | Relative

    type SemanticTokensClientCapabilities = {
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
    }

    /// Text document specific client capabilities.
    type TextDocumentClientCapabilities = {
        Synchronization: SynchronizationCapabilities option

        /// Capabilities specific to `textDocument/publishDiagnostics`.
        PublishDiagnostics:PublishDiagnosticsCapabilites

        /// Capabilities specific to the `textDocument/completion`
        Completion: CompletionCapabilities option

        /// Capabilities specific to the `textDocument/hover`
        Hover: HoverCapabilities option

        /// Capabilities specific to the `textDocument/signatureHelp`
        SignatureHelp: SignatureHelpCapabilities option

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
        Definition: DynamicCapabilities option

        /// Capabilities specific to the `textDocument/codeAction`
        CodeAction: CodeActionClientCapabilities option

        /// Capabilities specific to the `textDocument/codeLens`
        CodeLens: DynamicCapabilities option

        /// Capabilities specific to the `textDocument/documentLink`
        DocumentLink: DynamicCapabilities option

        /// Capabilities specific to the `textDocument/rename`
        Rename: DynamicCapabilities option

        /// Capabilities for the `textDocument/foldingRange`
        FoldingRange: FoldingRangeCapabilities option

        /// Capabilities for the `textDocument/selectionRange`
        SelectionRange: DynamicCapabilities option

        /// Capabilities specific to the various semantic token requests.
        /// @since 3.16.0
        SemanticTokens: SemanticTokensClientCapabilities option
    }

    type ClientCapabilities = {
        /// Workspace specific client capabilities.
        Workspace: WorkspaceClientCapabilities option

        /// Text document specific client capabilities.
        TextDocument: TextDocumentClientCapabilities option

        /// Experimental client capabilities.
        Experimental: JToken option
    }
    
    type WorkspaceFolder = {
        /// The associated URI for this workspace folder.
        Uri: DocumentUri;

        /// The name of the workspace folder. Defaults to the
        /// uri's basename.
        Name: string;
    }

    type InitializeParams = {
        ProcessId: int option
        RootPath: string option
        RootUri: string option
        InitializationOptions: JToken option
        Capabilities: ClientCapabilities option
        trace: string option
        /// The workspace folders configured in the client when the server starts.
        /// This property is only available if the client supports workspace folders.
        /// It can be `null` if the client supports workspace folders but none are configured.
        /// @since 3.6.0
        WorkspaceFolders: WorkspaceFolder[] option
    }

    type InitializedParams() =
        class end

    /// Completion options.
    type CompletionOptions = {
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
    }

    /// Signature help options.
    type SignatureHelpOptions = {
        /// The characters that trigger signature help automatically.
        TriggerCharacters: char[] option
        /// List of characters that re-trigger signature help.
        ///
        /// These trigger characters are only active when signature help is already showing.
        /// All trigger characters are also counted as re-trigger characters.
        RetriggerCharacters: char[] option
    }

    /// Code action options.
    type CodeActionOptions = {
        /// CodeActionKinds that this server may return.
        ///
        /// The list of kinds may be generic, such as `CodeActionKind.Refactor`,
        /// or the server may list out every specific kind they provide.
        CodeActionKinds: string[] option;

        /// The server provides support to resolve additional
        /// information for a code action.
        ResolveProvider: bool option;
    }

    /// Code Lens options.
    type CodeLensOptions = {
        /// Code lens has a resolve provider as well.
        ResolveProvider: bool option
    }

    /// Format document on type options
    type DocumentOnTypeFormattingOptions = {
        /// A character on which formatting should be triggered, like `}`.
        FirstTriggerCharacter: char

        /// More trigger characters.
        MoreTriggerCharacter: char[] option
    }

    /// Document link options
    type DocumentLinkOptions = {
        /// Document links have a resolve provider as well.
        ResolveProvider: bool option
    }

    /// Execute command options.
    type ExecuteCommandOptions = {
        /// The commands to be executed on the server
        commands: string[] option
    }

    /// Save options.
    type SaveOptions = {
        /// The client is supposed to include the content on save.
        IncludeText: bool option
    }

    type TextDocumentSyncOptions = {
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
    with
        static member Default =
            {
                OpenClose = None
                Change = None
                WillSave = None
                WillSaveWaitUntil = None
                Save = None
            }

    type SemanticTokensLegend = {
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
    type SemanticTokensOptions = {
      /// The legend used by the server
      Legend: SemanticTokensLegend

      /// Server supports providing semantic tokens for a specific range of a document.
      Range: U2<bool, obj> option

      /// Server supports providing semantic tokens for a full document.
      Full: U2<bool, SemanticTokenFullOptions> option
    }
    
    type WorkspaceFoldersServerCapabilities = {
        /// The server has support for workspace folders.
        Supported: bool option
        /// Whether the server wants to receive workspace folder change notifications.
        /// NOTE: the spec allows a string value here too. Good opportunity for further modeling.
        ChangeNotifications: bool option
    }
    with
        static member Default =
            {
                Supported = None
                ChangeNotifications = None
            }
    
    module FileOperationPatternKind =
        let File = "file"
        let Folder = "folder"
    
    type FileOperationPatternOptions = {
        /// The pattern should be matched ignoring casing.
        IgnoreCase: bool option
    }
    with
        static member Default =
            { IgnoreCase = None }
    
    /// See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileOperationPattern.
    type FileOperationPattern = {
        Glob: string
        /// Whether to match files or folders with this pattern. Matches both if undefined.
        /// See FileOperationPatternKind for allowed values
        Matches: string option
        /// Additional options used during matching
        Options: FileOperationPatternOptions option
    }
    
    type FileOperationFilter = {
        /// A Uri like `file` or `untitled`.
        Scheme: string option
        /// The actual file operation pattern.
        Pattern: FileOperationPattern
    }
    
    type FileOperationRegistrationOptions = {
        Filters: FileOperationFilter[]
    }
    
    /// The types of workspace-level file notifications the server is interested in.
    type WorkspaceFileOperationsServerCapabilities = {
        DidCreate: FileOperationRegistrationOptions option
        WillCreate: FileOperationRegistrationOptions option
        DidRename: FileOperationRegistrationOptions option
        WillRename: FileOperationRegistrationOptions option
        DidDelete: FileOperationRegistrationOptions option
        WillDelete: FileOperationRegistrationOptions option
    }
    with
        static member Default =
            {
                DidCreate = None
                WillCreate = None
                DidRename = None
                WillRename = None
                DidDelete = None
                WillDelete = None
            }
    
    type WorkspaceServerCapabilities = {
        /// The server supports workspace folder.
        /// @since 3.6.0
        WorkspaceFolders: WorkspaceFoldersServerCapabilities option
        /// The server is interested in file notifications/requests.
        /// @since 3.16.0
        FileOperations: WorkspaceFileOperationsServerCapabilities option
    }
    with
        static member Default =
            {
                WorkspaceFolders = None
                FileOperations = None
            }

    type ServerCapabilities = {
        /// Defines how text documents are synced. Is either a detailed structure defining each notification or
        /// for backwards compatibility the TextDocumentSyncKind number.
        TextDocumentSync: TextDocumentSyncOptions option

        /// The server provides hover support.
        HoverProvider: bool option

        /// The server provides completion support.
        CompletionProvider: CompletionOptions option

        /// The server provides signature help support.
        SignatureHelpProvider: SignatureHelpOptions option

        /// The server provides goto definition support.
        DefinitionProvider: bool option

        ///The server provides Goto Implementation support
        ImplementationProvider :bool option

        /// The server provides goto type definition support.
        TypeDefinitionProvider: bool option

        /// The server provides find references support.
        ReferencesProvider: bool option

        /// The server provides document highlight support.
        DocumentHighlightProvider: bool option

        /// The server provides document symbol support.
        DocumentSymbolProvider: bool option

        /// The server provides workspace symbol support.
        WorkspaceSymbolProvider: bool option

        /// The server provides code actions.
        CodeActionProvider: CodeActionOptions option

        /// The server provides code lens.
        CodeLensProvider: CodeLensOptions option

        /// The server provides document formatting.
        DocumentFormattingProvider: bool option

        /// The server provides document range formatting.
        DocumentRangeFormattingProvider: bool option

        /// The server provides document formatting on typing.
        DocumentOnTypeFormattingProvider: DocumentOnTypeFormattingOptions option

        /// The server provides rename support.
        RenameProvider: bool option

        /// The server provides document link support.
        DocumentLinkProvider: DocumentLinkOptions option

        /// The server provides execute command support.
        ExecuteCommandProvider: ExecuteCommandOptions option

        /// Experimental server capabilities.
        Experimental: JToken option

        /// The server provides folding provider support.
        /// @since 3.10.0
        FoldingRangeProvider: bool option

        SelectionRangeProvider: bool option

        SemanticTokensProvider: SemanticTokensOptions option
        
        /// Workspace specific server capabilities.
        Workspace: WorkspaceServerCapabilities option

    }
    with
        static member Default =
            {
                HoverProvider = None
                TextDocumentSync = None
                CompletionProvider = None
                SignatureHelpProvider = None
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
                ExecuteCommandProvider = None
                Experimental = None
                FoldingRangeProvider = None
                SelectionRangeProvider = None
                SemanticTokensProvider = None
                Workspace = None
            }

    type InitializeResult = {
        Capabilities: ServerCapabilities
    }
    with
        static member Default =
            {
                Capabilities = ServerCapabilities.Default
            }

    /// A workspace edit represents changes to many resources managed in the workspace.
    /// The edit should either provide `changes` or `documentChanges`. If the client can handle versioned document
    /// edits and if `documentChanges` are present, the latter are preferred over `changes`.
    type WorkspaceEdit = {
        /// Holds changes to existing resources.
        Changes: Map<string, TextEdit[]> option

        /// An array of `TextDocumentEdit`s to express changes to n different text documents
        /// where each text document edit addresses a specific version of a text document.
        /// Whether a client supports versioned document edits is expressed via
        /// `WorkspaceClientCapabilities.workspaceEdit.documentChanges`.
        DocumentChanges: TextDocumentEdit[] option
    }
    with
        static member DocumentChangesToChanges(edits: TextDocumentEdit[]) =
            edits
            |> Array.map (fun edit -> edit.TextDocument.Uri.ToString(), edit.Edits)
            |> Map.ofArray

        static member CanUseDocumentChanges(capabilities: ClientCapabilities) =
            (capabilities.Workspace
            |> Option.bind(fun x -> x.WorkspaceEdit)
            |> Option.bind (fun x -> x.DocumentChanges))
                = Some true

        static member Create(edits: TextDocumentEdit[], capabilities: ClientCapabilities ) =
            if WorkspaceEdit.CanUseDocumentChanges(capabilities) then
                {
                    Changes = None
                    DocumentChanges = Some edits
                }
            else
                {
                    Changes = Some (WorkspaceEdit.DocumentChangesToChanges edits)
                    DocumentChanges = None
                }

    type MessageType =
        | Error = 1
        | Warning = 2
        | Info = 3
        | Log = 4

    type LogMessageParams = {
        Type: MessageType
        Message: string
    }

    type ShowMessageParams = {
        Type: MessageType
        Message: string
    }

    type MessageActionItem = {
        /// A short title like 'Retry', 'Open Log' etc.
        Title: string;
    }

    type ShowMessageRequestParams = {
        /// The message type.
        Type: MessageType

        /// The actual message
        Message: string

        /// The message action items to present.
        Actions: MessageActionItem[] option
    }

    /// General parameters to register for a capability.
    type Registration = {
        /// The id used to register the request. The id can be used to deregister
        /// the request again.
        Id: string

        /// The method / capability to register for.
        Method: string

        /// Options necessary for the registration.
        RegisterOptions: JToken option
    }

    type RegistrationParams = {
        Registrations: Registration[]
    }

    type ITextDocumentRegistrationOptions =
        /// A document selector to identify the scope of the registration. If set to null
        /// the document selector provided on the client side will be used.
        abstract member DocumentSelector : DocumentSelector option with get

    /// General parameters to unregister a capability.
    type Unregistration = {
        /// The id used to unregister the request or notification. Usually an id
        /// provided during the register request.
        Id: string

        /// The method / capability to unregister for.
        Method: string
    }

    type UnregistrationParams = {
        Unregisterations: Unregistration[]
    }

    type FileChangeType =
        | Created = 1
        | Changed = 2
        | Deleted = 3

    /// An event describing a file change.
    type FileEvent ={
        /// The file's URI.
        Uri: DocumentUri

        /// The change type.
        Type: FileChangeType
    }

    type DidChangeWatchedFilesParams = {
        /// The actual file events.
        Changes: FileEvent[]
    }

    /// The workspace folder change event.
    type WorkspaceFoldersChangeEvent = {
        /// The array of added workspace folders
        Added: WorkspaceFolder[];

        /// The array of the removed workspace folders
        Removed: WorkspaceFolder[];
    }

    type DidChangeWorkspaceFoldersParams = {
        /// The actual workspace folder change event.
        Event: WorkspaceFoldersChangeEvent
    }

    type DidChangeConfigurationParams = {
        /// The actual changed settings
        Settings: JToken;
    }

    type ConfigurationItem = {
        /// The scope to get the configuration section for.
        ScopeUri: string option

        /// The configuration section asked for.
        Section: string option
    }

    type ConfigurationParams = {
        items: ConfigurationItem[]
    }

    /// The parameters of a Workspace Symbol Request.
    type WorkspaceSymbolParams = {
        /// A non-empty query string
        Query: string
    }

    type ExecuteCommandParams = {
        /// The identifier of the actual command handler.
        Command: string
        /// Arguments that the command should be invoked with.
        Arguments: JToken[] option
    }

    type ApplyWorkspaceEditParams = {
        /// An optional label of the workspace edit. This label is
        /// presented in the user interface for example on an undo
        /// stack to undo the workspace edit.
        Label: string option

        /// The edits to apply.
        Edit: WorkspaceEdit
    }

    type ApplyWorkspaceEditResponse = {
        /// Indicates whether the edit was applied or not.
        Applied: bool
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
    type WillSaveTextDocumentParams = {
        /// The document that will be saved.
        TextDocument: TextDocumentIdentifier

        /// The 'TextDocumentSaveReason'.
        Reason: TextDocumentSaveReason
    }

    type DidSaveTextDocumentParams = {
        /// The document that was saved.
        TextDocument: TextDocumentIdentifier

        /// Optional the content when saved. Depends on the includeText value
        /// when the save notification was requested.
        Text: string option
    }

    type DidCloseTextDocumentParams = {
        /// The document that was closed.
        TextDocument: TextDocumentIdentifier
    }

    /// Value-object describing what options formatting should use.
    type FormattingOptions() =
        /// Size of a tab in spaces.
        member val TabSize : int = 0 with get, set

        /// Prefer spaces over tabs.
        member val InsertSpaces: bool = false with get, set

        /// Further properties.
        [<JsonExtensionData>]
        member val AdditionalData: System.Collections.Generic.IDictionary<string, JToken> = new System.Collections.Generic.Dictionary<_,_>() :> _ with get, set

    type DocumentFormattingParams = {
        /// The document to format.
        TextDocument: TextDocumentIdentifier

        /// The format options.
        Options: FormattingOptions
    }

    type DocumentRangeFormattingParams = {
        /// The document to format.
        TextDocument: TextDocumentIdentifier

        /// The range to format
        Range: Range

        /// The format options
        Options: FormattingOptions
    }

    type DocumentOnTypeFormattingParams = {
        /// The document to format.
        TextDocument: TextDocumentIdentifier

        /// The position at which this request was sent.
        Position: Position

        /// The character that has been typed.
        Ch: char

        /// The format options.
        Options: FormattingOptions
    }

    type DocumentSymbolParams = {
        /// The text document.
        TextDocument: TextDocumentIdentifier
    }

    type ITextDocumentPositionParams =
        /// The text document.
        abstract member TextDocument : TextDocumentIdentifier with get
        /// The position inside the text document.
        abstract member Position : Position with get

    type TextDocumentPositionParams =
        {
            /// The text document.
            TextDocument: TextDocumentIdentifier
            /// The position inside the text document.
            Position: Position
        }
        interface ITextDocumentPositionParams with
            member this.TextDocument with get() = this.TextDocument
            member this.Position with get() = this.Position

    type ReferenceContext = {
        /// Include the declaration of the current symbol.
        IncludeDeclaration: bool
    }

    type ReferenceParams  =
        {
            /// The text document.
            TextDocument: TextDocumentIdentifier
            /// The position inside the text document.
            Position: Position
            Context: ReferenceContext
        }
        interface ITextDocumentPositionParams with
            member this.TextDocument with get() = this.TextDocument
            member this.Position with get() = this.Position

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
    type MarkupContent = {
        /// The type of the Markup
        Kind: string

        // The content itself
        Value: string
    }

    type MarkedStringData = {
        Language: string
        Value: string
    }

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
        | MarkedStrings of MarkedString []
        | MarkupContent of MarkupContent

    /// The result of a hover request.
    type Hover = {
        /// The hover's content
        Contents: HoverContent

        /// An optional range is a range inside a text document
        /// that is used to visualize a hover, e.g. by changing the background color.
        Range: Range option
    }

    /// An item to transfer a text document from the client to the server.
    type TextDocumentItem = {
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

    type DidOpenTextDocumentParams = {
        /// The document that was opened.
        TextDocument: TextDocumentItem
    }

    /// An event describing a change to a text document. If range and rangeLength are omitted
    /// the new text is considered to be the full content of the document.
    type TextDocumentContentChangeEvent = {
        /// The range of the document that changed.
        Range: Range option

        /// The length of the range that got replaced.
        RangeLength: int option

        /// The new text of the range/document.
        Text: string
    }

    type DidChangeTextDocumentParams = {
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

    type FileSystemWatcher = {
        /// The  glob pattern to watch
        GlobPattern: string

        /// The kind of events of interest. If omitted it defaults
        /// to WatchKind.Create | WatchKind.Change | WatchKind.Delete
        /// which is 7.
        Kind: WatchKind option
    }

    /// Describe options to be used when registered for text document change events.
    type DidChangeWatchedFilesRegistrationOptions = {
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

    type CompletionContext = {
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
            member this.TextDocument with get() = this.TextDocument
            member this.Position with get() = this.Position

    /// Represents a reference to a command. Provides a title which will be used to represent a command in the UI.
    /// Commands are identified by a string identifier. The protocol currently doesn't specify a set of well-known
    /// commands. So executing a command requires some tool extension code.
    type Command = {
        /// Title of the command, like `save`.
        Title: string

        /// The identifier of the actual command handler.
        Command: string

        /// Arguments that the command handler should be
        /// invoked with.
        Arguments: JToken[] option
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

    type CompletionItem = {
        /// The label of this completion item. By default
        /// also the text that is inserted when selecting
        /// this completion.
        Label: string

        /// The kind of this completion item. Based of the kind
        /// an icon is chosen by the editor.
        Kind: CompletionItemKind option

        /// A human-readable string with additional information
        /// about this item, like type or symbol information.
        Detail: string option

        /// A human-readable string that represents a doc-comment.
        Documentation: Documentation option

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

        /// An edit which is applied to a document when selecting this completion. When an edit is provided the value of
        /// `insertText` is ignored.
        ///
        /// *Note:* The range of the edit must be a single line range and it must contain the position at which completion
        /// has been requested.
        TextEdit: TextEdit option

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
    with
        static member Create(label:string) =
            {
                Label = label
                Kind = None
                Detail = None
                Documentation = None
                SortText = None
                FilterText = None
                InsertText = None
                InsertTextFormat = None
                TextEdit = None
                AdditionalTextEdits = None
                CommitCharacters = None
                Command = None
                Data = None
            }

    type CompletionList = {
        /// This list it not complete. Further typing should result in recomputing
        /// this list.
        IsIncomplete: bool

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
    type DiagnosticRelatedInformation = {
        Location: Location
        Message: string
    }

    // Structure to capture a description for an error code.
    type CodeDescription =
      {
        // An URI to open with more information about the diagnostic error.
        Href: Uri option
      }

    /// Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid in the
    /// scope of a resource.
    [<DebuggerDisplay("{DebuggerDisplay}")>]
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
          Source: string

          /// The diagnostic's message.
          Message: string
          RelatedInformation: DiagnosticRelatedInformation [] option
          Tags: DiagnosticTag[] option
          /// A data entry field that is preserved between a
          /// `textDocument/publishDiagnostics` notification and
          /// `textDocument/codeAction` request.
          Data: obj option

      }
      [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
      member x.DebuggerDisplay = $"[{defaultArg x.Severity DiagnosticSeverity.Error}] ({x.Range.DebuggerDisplay}) {x.Message} ({defaultArg x.Code String.Empty})"

    type PublishDiagnosticsParams = {
        /// The URI for which diagnostic information is reported.
        Uri: DocumentUri

        /// An array of diagnostic information items.
        Diagnostics: Diagnostic[]
    }

    type CodeActionDisabled = {
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
    type CodeAction = {

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
        Data: obj option
    }

    [<ErasedUnion>]
    [<RequireQualifiedAccess>]
    type TextDocumentCodeActionResult =
    | Commands of Command []
    | CodeActions of CodeAction []

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
            member this.TextDocument with get() = this.TextDocument
            member this.Position with get() = this.Position

    [<ErasedUnion>]
    [<RequireQualifiedAccess>]
    type GotoResult =
    | Single of Location
    | Multiple of Location []

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
    type DocumentHighlight = {
        /// The range this highlight applies to.
        Range: Range

        /// The highlight kind, default is DocumentHighlightKind.Text.
        Kind: DocumentHighlightKind option
    }

    type DocumentLinkParams = {
        /// The document to provide document links for.
        TextDocument: TextDocumentIdentifier
    }

    /// A document link is a range in a text document that links to an internal or external resource, like another
    /// text document or a web site.
    type DocumentLink = {
        /// The range this link applies to.
        Range: Range

        /// The uri this link points to. If missing a resolve request is sent later.
        Target: DocumentUri option
    }

    type DocumentColorParams = {
        /// The text document.
        TextDocument: TextDocumentIdentifier
    }

    /// Represents a color in RGBA space.
    type Color = {
        /// The red component of this color in the range [0-1].
        Red: float

        /// The green component of this color in the range [0-1].
        Green: float

        /// The blue component of this color in the range [0-1].
        Blue: float

        /// The alpha component of this color in the range [0-1].
        Alpha: float
    }

    type ColorInformation = {
        /// The range in the document where this color appears.
        Range: Range

        /// The actual color value for this color range.
        Color: Color
    }

    type ColorPresentationParams = {
        /// The text document.
        TextDocument: TextDocumentIdentifier

        /// The color information to request presentations for.
        ColorInfo: Color

        /// The range where the color would be inserted. Serves as a context.
        Range: Range
    }

    type ColorPresentation = {
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

    /// Contains additional diagnostic information about the context in which
    /// a code action is run.
    type CodeActionContext = {
        /// An array of diagnostics.
        Diagnostics: Diagnostic[]
    }

    /// Params for the CodeActionRequest
    type CodeActionParams = {
        /// The document in which the command was invoked.
        TextDocument: TextDocumentIdentifier

        /// The range for which the command was invoked.
        Range: Range

        /// Context carrying additional information.
        Context: CodeActionContext
    }

    type CodeLensParams = {
        /// The document to request code lens for.
        TextDocument: TextDocumentIdentifier;
    }

    /// A code lens represents a command that should be shown along with
    /// source text, like the number of references, a way to run tests, etc.
    ///
    /// A code lens is _unresolved_ when no command is associated to it. For performance
    /// reasons the creation of a code lens and resolving should be done in two stages.
    type CodeLens = {
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
    type ParameterInformation = {
        /// The label of this parameter. Will be shown in
        /// the UI.
        Label: string

        /// The human-readable doc-comment of this parameter. Will be shown
        /// in the UI but can be omitted.
        Documentation: Documentation option
    }

    ///Represents the signature of something callable. A signature
    /// can have a label, like a function-name, a doc-comment, and
    /// a set of parameters.
    type SignatureInformation = {
        /// The label of this signature. Will be shown in
        /// the UI.
        Label: string

        /// The human-readable doc-comment of this signature. Will be shown
        /// in the UI but can be omitted.
        Documentation: Documentation option

        /// The parameters of this signature.
        Parameters: ParameterInformation[] option
    }

    /// Signature help represents the signature of something
    /// callable. There can be multiple signature but only one
    /// active and only one active parameter.
    type SignatureHelp = {
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
        ActiveParameter: int option
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
        member this.TextDocument with get () = this.TextDocument
        member this.Position with get () = this.Position

    type FoldingRangeParams = {
        /// the document to generate ranges for
        TextDocument: TextDocumentIdentifier
    }

    module FoldingRangeKind =
        let Comment = "comment"
        let Imports = "imports"
        let Region = "region"

    type FoldingRange = {
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
    }

    type SelectionRangeParams = {
        /// The document to generate ranges for
        TextDocument: TextDocumentIdentifier

        /// The positions inside the text document.
        Positions: Position[]
    }

    type SelectionRange = {
        /// The range of this selection range.
        Range: Range

        /// The parent selection range containing this range. Therefore `parent.range` must contain `this.range`.
        Parent: SelectionRange option
    }

    type SemanticTokensParams = {
      TextDocument: TextDocumentIdentifier
    }

    type SemanticTokensDeltaParams = {
      TextDocument: TextDocumentIdentifier
      /// The result id of a previous response. The result Id can either point to
      /// a full response or a delta response depending on what was received last.
      PreviousResultId: string
    }

    type SemanticTokensRangeParams = {
      TextDocument: TextDocumentIdentifier
      Range: Range
    }

    type SemanticTokens = {
      /// An optional result id. If provided and clients support delta updating
      /// the client will include the result id in the next semantic token request.
      /// A server can then instead of computing all semantic tokens again simply
      /// send a delta.
      ResultId: string option
      Data: uint32[]
    }

    type SemanticTokensEdit = {
      /// The start offset of the edit.
      Start: uint32

      /// The count of elements to remove.
      DeleteCount: uint32

      /// The elements to insert.
      Data: uint32[] option
    }

    type SemanticTokensDelta = {
      ResultId: string option

      /// The semantic token edits to transform a previous result into a new
      /// result.
      Edits: SemanticTokensEdit[];
    }

module LowLevel =
    open System
    open System.IO
    open System.Text

    let headerBufferSize = 300
    let minimumHeaderLength = 21
    let cr = byte '\r'
    let lf = byte '\f'
    let headerEncoding = Encoding.ASCII

    let private readLine (stream: Stream) =
        let buffer = Array.zeroCreate<byte> headerBufferSize
        let readCount = stream.Read(buffer, 0, 2)
        let mutable count = readCount
        if count < 2 then
            None
        else
            // TODO: Check that we don't over-fill headerBufferSize
            while count < headerBufferSize && (buffer.[count-2] <> cr && buffer.[count-1] <> lf) do
                 let additionalBytesRead  = stream.Read(buffer, count, 1)
                 // TODO: exit when additionalBytesRead = 0, end of stream
                 count <- count + additionalBytesRead

            if count >= headerBufferSize then
                None
            else
                Some (headerEncoding.GetString(buffer, 0, count - 2))

    let rec private readHeaders (stream: Stream) =
        let line = readLine stream
        match line with
        | Some "" -> []
        | Some line ->
            let separatorPos = line.IndexOf(": ")
            if separatorPos = -1 then
                raise (Exception(sprintf "Separator not found in header '%s'" line))
            else
                let name = line.Substring(0, separatorPos)
                let value = line.Substring(separatorPos + 2)
                let otherHeaders = readHeaders stream
                (name,value) :: otherHeaders
        | None ->
            raise (EndOfStreamException())

    let read (stream: Stream) =
        let headers = readHeaders stream

        let contentLength =
            headers
            |> List.tryFind(fun (name, _) -> name = "Content-Length")
            |> Option.map snd
            |> Option.bind (fun s -> match Int32.TryParse(s) with | true, x -> Some x | _ -> None)

        if contentLength = None then
            failwithf "Content-Length header not found"
        else
            let result = Array.zeroCreate<byte> contentLength.Value
            let mutable readCount = 0
            while readCount < contentLength.Value do
                let toRead = contentLength.Value - readCount
                let readInCurrentBatch = stream.Read(result, readCount, toRead)
                readCount <- readCount + readInCurrentBatch
            let str = Encoding.UTF8.GetString(result, 0, readCount)
            headers, str

    let write (stream: Stream) (data: string) =
        let bytes = Encoding.UTF8.GetBytes(data)
        let header = sprintf "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: %d\r\n\r\n" bytes.Length
        let headerBytes = Encoding.ASCII.GetBytes header
        use ms = new MemoryStream(headerBytes.Length + bytes.Length)
        ms.Write(headerBytes, 0, headerBytes.Length)
        ms.Write(bytes, 0, bytes.Length)
        stream.Write(ms.ToArray(), 0, int ms.Position)

module JsonRpc =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    type MessageTypeTest = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Method: string option
    }
    [<RequireQualifiedAccess>]
    type MessageType =
      | Notification
      | Request
      | Response
      | Error

    let getMessageType messageTest =
      match messageTest with
      | { Version = "2.0"; Id = Some _; Method = Some _; } -> MessageType.Request
      | { Version = "2.0"; Id = Some _; Method = None; } -> MessageType.Response
      | { Version = "2.0"; Id = None; Method = Some _; } -> MessageType.Notification
      | _ -> MessageType.Error

    type Request = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int
        Method: string
        Params: JToken option
    }
    with
        static member Create(id: int, method': string, rpcParams: JToken option) =
            { Version = "2.0"; Id = id; Method = method'; Params = rpcParams }

    type Notification = {
        [<JsonProperty("jsonrpc")>] Version: string
        Method: string
        Params: JToken option
    }
    with
        static member Create(method': string, rpcParams: JToken option) =
            { Version = "2.0"; Method = method'; Params = rpcParams }

    module ErrorCodes =
        let parseError = -32700
        let invalidRequest = -32600
        let methodNotFound = -32601
        let invalidParams = -32602
        let internalError = -32603
        let serverErrorStart = -32000
        let serverErrorEnd = -32099

    type Error = {
        Code: int
        Message: string
        Data: JToken option
    }
    with
        static member Create(code: int, message: string) =
            { Code = code; Message = message; Data = None }
        static member ParseError = Error.Create(ErrorCodes.parseError, "Parse error")
        static member InvalidRequest = Error.Create(ErrorCodes.invalidRequest, "Invalid Request")
        static member MethodNotFound = Error.Create(ErrorCodes.methodNotFound, "Method not found")
        static member InvalidParams = Error.Create(ErrorCodes.invalidParams, "Invalid params")
        static member InternalError = Error.Create(ErrorCodes.internalError, "Internal error")
        static member InternalErrorMessage message = Error.Create(ErrorCodes.internalError, message)

    type Response = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Error: Error option
        [<JsonProperty(NullValueHandling=NullValueHandling.Include)>]
        Result: JToken option
    }
    with
        /// Json.NET conditional property serialization, controlled by naming convention
        member x.ShouldSerializeResult() = x.Error.IsNone
        static member Success(id: int, result: JToken option) =
            { Version = "2.0"; Id = Some id; Result = result; Error = None }
        static member Failure(id: int, error: Error) =
            { Version = "2.0"; Id = Some id; Result = None; Error = Some error }

type LspResult<'t> = Result<'t, JsonRpc.Error>
type AsyncLspResult<'t> = Async<LspResult<'t>>

module LspResult =
    let success x : LspResult<_> =
        Result.Ok x

    let invalidParams s : LspResult<_> =
        Result.Error (JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, s))

    let internalError<'a> (s: string): LspResult<'a> =
        Result.Error (JsonRpc.Error.Create(JsonRpc.ErrorCodes.internalError, s))

    let notImplemented<'a> : LspResult<'a> =
        Result.Error (JsonRpc.Error.MethodNotFound)

module AsyncLspResult =
    let success x : AsyncLspResult<_> =
        async.Return (Result.Ok x)

    let invalidParams s : AsyncLspResult<_> =
        async.Return (Result.Error (JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, s)))

    let internalError s : AsyncLspResult<_> =
        async.Return (Result.Error (JsonRpc.Error.Create(JsonRpc.ErrorCodes.internalError, s)))

    let notImplemented<'a> : AsyncLspResult<'a> =
        async.Return (Result.Error (JsonRpc.Error.MethodNotFound))

/// Return the JSON-RPC "not implemented" error
let private notImplemented<'t> = async.Return LspResult.notImplemented<'t>

/// Do nothing and ignore the notification
let private ignoreNotification = async.Return(())

open Types
open Newtonsoft.Json.Linq

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
    abstract member WorkspaceConfiguration : ConfigurationParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken[]>
    default __.WorkspaceConfiguration(_) = notImplemented

    abstract member WorkspaceApplyEdit : ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResponse>
    default __.WorkspaceApplyEdit(_) = notImplemented

    /// The workspace/semanticTokens/refresh request is sent from the server to the client.
    /// Servers can use it to ask clients to refresh the editors for which this server provides semantic tokens.
    /// As a result the client should ask the server to recompute the semantic tokens for these editors.
    /// This is useful if a server detects a project wide configuration change which requires a re-calculation
    /// of all semantic tokens. Note that the client still has the freedom to delay the re-calculation of
    /// the semantic tokens if for example an editor is currently not visible.
    abstract member WorkspaceSemanticTokensRefresh: unit -> Async<unit>
    default __.WorkspaceSemanticTokensRefresh() = ignoreNotification

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



[<AbstractClass>]
type LspServer() =
    interface System.IDisposable with
      member x.Dispose() = x.Dispose()

    abstract member Dispose : unit -> unit

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
    abstract member Shutdown : unit -> Async<unit>
    default __.Shutdown() = ignoreNotification

    /// A notification to ask the server to exit its process.
    abstract member Exit : unit -> Async<unit>
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

    /// The goto definition request is sent from the client to the server to resolve the definition location of
    /// a symbol at a given text document position.
    abstract member TextDocumentDefinition: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>
    default __.TextDocumentDefinition(_) = notImplemented

    /// The references request is sent from the client to the server to resolve project-wide references for
    /// the symbol denoted by the given text document position.
    abstract member TextDocumentReferences: ReferenceParams -> AsyncLspResult<Location[] option>
    default __.TextDocumentReferences(_) = notImplemented

    /// The document highlight request is sent from the client to the server to resolve a document highlights
    /// for a given text document position. For programming languages this usually highlights all references
    /// to the symbol scoped to this file.
    ///
    /// However we kept `textDocument/documentHighlight` and `textDocument/references` separate requests since
    /// the first one is allowed to be more fuzzy. Symbol matches usually have a DocumentHighlightKind of Read
    /// or Write whereas fuzzy or textual matches use Text as the kind.
    abstract member TextDocumentDocumentHighlight: TextDocumentPositionParams -> AsyncLspResult<DocumentHighlight[] option>
    default __.TextDocumentDocumentHighlight(_) = notImplemented

    /// The document links request is sent from the client to the server to request the location of links
    /// in a document.
    abstract member TextDocumentDocumentLink: DocumentLinkParams -> AsyncLspResult<DocumentLink[] option>
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
    abstract member TextDocumentCodeLens: CodeLensParams -> AsyncLspResult<CodeLens[] option>
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
    abstract member TextDocumentDocumentColor: DocumentColorParams -> AsyncLspResult<ColorInformation[]>
    default __.TextDocumentDocumentColor(_) = notImplemented

    /// The color presentation request is sent from the client to the server to obtain a list of
    /// presentations for a color value at a given location. Clients can use the result to
    abstract member TextDocumentColorPresentation: ColorPresentationParams -> AsyncLspResult<ColorPresentation[]>
    default __.TextDocumentColorPresentation(_) = notImplemented

    /// The document formatting request is sent from the client to the server to format a whole document.
    abstract member TextDocumentFormatting: DocumentFormattingParams -> AsyncLspResult<TextEdit[] option>
    default __.TextDocumentFormatting(_) = notImplemented

    /// The document range formatting request is sent from the client to the server to format a given
    /// range in a document.
    abstract member TextDocumentRangeFormatting: DocumentRangeFormattingParams -> AsyncLspResult<TextEdit[] option>
    default __.TextDocumentRangeFormatting(_) = notImplemented

    /// The document on type formatting request is sent from the client to the server to format parts
    /// of the document during typing.
    abstract member TextDocumentOnTypeFormatting: DocumentOnTypeFormattingParams -> AsyncLspResult<TextEdit[] option>
    default __.TextDocumentOnTypeFormatting(_) = notImplemented

    /// The document symbol request is sent from the client to the server to return a flat list of all symbols
    /// found in a given text document. Neither the symbol’s location range nor the symbol’s container name
    /// should be used to infer a hierarchy.
    abstract member TextDocumentDocumentSymbol: DocumentSymbolParams -> AsyncLspResult<SymbolInformation[] option>
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

    /// The workspace symbol request is sent from the client to the server to list project-wide symbols matching
    /// the query string.
    abstract member WorkspaceSymbol: WorkspaceSymbolParams -> AsyncLspResult<SymbolInformation[] option>
    default __.WorkspaceSymbol(_) = notImplemented

    /// The `workspace/executeCommand` request is sent from the client to the server to trigger command execution
    /// on the server. In most cases the server creates a `WorkspaceEdit` structure and applies the changes to the
    /// workspace using the request `workspace/applyEdit` which is sent from the server to the client.
    abstract member WorkspaceExecuteCommand : ExecuteCommandParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken>
    default __.WorkspaceExecuteCommand(_) = notImplemented

    /// The document will save notification is sent from the client to the server before the document is
    /// actually saved.
    abstract member TextDocumentWillSave: WillSaveTextDocumentParams -> Async<unit>
    default __.TextDocumentWillSave(_) = ignoreNotification

    /// The document will save request is sent from the client to the server before the document is actually saved.
    /// The request can return an array of TextEdits which will be applied to the text document before it is saved.
    /// Please note that clients might drop results if computing the text edits took too long or if a server
    /// constantly fails on this request. This is done to keep the save fast and reliable.
    abstract member TextDocumentWillSaveWaitUntil: WillSaveTextDocumentParams -> AsyncLspResult<TextEdit[] option>
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

    abstract member TextDocumentSemanticTokensFullDelta: SemanticTokensDeltaParams -> AsyncLspResult<U2<SemanticTokens, SemanticTokensDelta> option>
    default __.TextDocumentSemanticTokensFullDelta(_) = notImplemented

    abstract member TextDocumentSemanticTokensRange: SemanticTokensRangeParams -> AsyncLspResult<SemanticTokens option>
    default __.TextDocumentSemanticTokensRange(_) = notImplemented

module Server =
    open System.IO
    open LanguageServerProtocol.Logging
    open Newtonsoft.Json
    open Newtonsoft.Json.Serialization

    open JsonRpc

    let logger = LogProvider.getLoggerByName "LSP Server"

    let jsonSettings =
        let result = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        result.Converters.Add(SingleCaseUnionConverter())
        result.Converters.Add(U2BoolObjectConverter())
        result.Converters.Add(OptionConverter())
        result.Converters.Add(ErasedUnionConverter())
        result.ContractResolver <- CamelCasePropertyNamesContractResolver()
        result

    let jsonSerializer = JsonSerializer.Create(jsonSettings)

    type RequestHandling<'server when 'server :> LspServer> = {
        Run: 'server -> JToken option -> AsyncLspResult<JToken option>
    }

    let deserialize<'t> (token: JToken) = token.ToObject<'t>(jsonSerializer)

    let serialize<'t> (o: 't) = JToken.FromObject(o, jsonSerializer)

    let requestHandling<'param, 'result, 'server when 'server :> LspServer> (run: 'server -> 'param -> AsyncLspResult<'result>): RequestHandling<'server> =
        let tokenRun (lspServer: 'server) (paramsToken: JToken option): AsyncLspResult<JToken option> =
            try
                let p =
                    match paramsToken with
                    | Some paramsToken ->
                        let typedParams = paramsToken.ToObject<'param>(jsonSerializer)
                        Some typedParams
                    | None ->
                        if typeof<'param> = typeof<unit> then
                            Some (unbox ())
                        else
                            None
                match p with
                | Some p ->
                    async {
                        try
                            let! result = run lspServer p
                            match result with
                            | Result.Ok ok ->
                                if isNull (box ok) then
                                    return Result.Ok None
                                else
                                    let resultToken = JToken.FromObject(ok, jsonSerializer)
                                    return Result.Ok (Some resultToken)
                            | Result.Error err ->
                                return Result.Error err
                        with
                        | ex ->
                            return Result.Error (Error.Create(ErrorCodes.internalError, ex.ToString()))
                    }
                | None ->
                     async.Return (Result.Error (Error.Create(ErrorCodes.invalidRequest, "No params found")))
            with
            | :? JsonException as ex ->
                async.Return (Result.Error (Error.Create(ErrorCodes.parseError, ex.ToString())))

        { Run = tokenRun }

    /// Notifications don't generate a response or error, but to unify things we consider them as always successful.
    /// They will still not send any response because their ID is null.
    let private notificationSuccess (response: Async<unit>) = async {
        do! response
        return Result.Ok ()
    }

    let defaultRequestHandlings<'server when 'server :> LspServer> () : Map<string, RequestHandling<'server>> =
        [
            "initialize", requestHandling (fun s p -> s.Initialize(p))
            "initialized", requestHandling (fun s p -> s.Initialized(p) |> notificationSuccess)
            "textDocument/hover", requestHandling (fun s p -> s.TextDocumentHover(p))
            "textDocument/didOpen", requestHandling (fun s p -> s.TextDocumentDidOpen(p) |> notificationSuccess)
            "textDocument/didChange", requestHandling (fun s p -> s.TextDocumentDidChange(p) |> notificationSuccess)
            "textDocument/completion", requestHandling (fun s p -> s.TextDocumentCompletion(p))
            "completionItem/resolve", requestHandling (fun s p -> s.CompletionItemResolve(p))
            "textDocument/rename", requestHandling (fun s p -> s.TextDocumentRename(p))
            "textDocument/definition", requestHandling (fun s p -> s.TextDocumentDefinition(p))
            "textDocument/typeDefinition", requestHandling (fun s p -> s.TextDocumentTypeDefinition(p))
            "textDocument/implementation", requestHandling (fun s p -> s.TextDocumentImplementation(p))
            "textDocument/codeAction", requestHandling (fun s p -> s.TextDocumentCodeAction(p))
            "codeAction/resolve", requestHandling (fun s p -> s.CodeActionResolve(p))
            "textDocument/codeLens", requestHandling (fun s p -> s.TextDocumentCodeLens(p))
            "codeLens/resolve", requestHandling (fun s p -> s.CodeLensResolve(p))
            "textDocument/references", requestHandling (fun s p -> s.TextDocumentReferences(p))
            "textDocument/documentHighlight", requestHandling (fun s p -> s.TextDocumentDocumentHighlight(p))
            "textDocument/documentLink", requestHandling (fun s p -> s.TextDocumentDocumentLink(p))
            "textDocument/signatureHelp", requestHandling (fun s p -> s.TextDocumentSignatureHelp(p))
            "documentLink/resolve", requestHandling (fun s p -> s.DocumentLinkResolve(p))
            "textDocument/documentColor", requestHandling (fun s p -> s.TextDocumentDocumentColor(p))
            "textDocument/colorPresentation", requestHandling (fun s p -> s.TextDocumentColorPresentation(p))
            "textDocument/formatting", requestHandling (fun s p -> s.TextDocumentFormatting(p))
            "textDocument/rangeFormatting", requestHandling (fun s p -> s.TextDocumentRangeFormatting(p))
            "textDocument/onTypeFormatting", requestHandling (fun s p -> s.TextDocumentOnTypeFormatting(p))
            "textDocument/willSave", requestHandling (fun s p -> s.TextDocumentWillSave(p) |> notificationSuccess)
            "textDocument/willSaveWaitUntil", requestHandling (fun s p -> s.TextDocumentWillSaveWaitUntil(p))
            "textDocument/didSave", requestHandling (fun s p -> s.TextDocumentDidSave(p) |> notificationSuccess)
            "textDocument/didClose", requestHandling (fun s p -> s.TextDocumentDidClose(p) |> notificationSuccess)
            "textDocument/documentSymbol", requestHandling (fun s p -> s.TextDocumentDocumentSymbol(p))
            "textDocument/foldingRange", requestHandling (fun s p -> s.TextDocumentFoldingRange(p))
            "textDocument/selectionRange", requestHandling (fun s p -> s.TextDocumentSelectionRange(p))
            "textDocument/semanticTokens/full", requestHandling(fun s p -> s.TextDocumentSemanticTokensFull(p))
            "textDocument/semanticTokens/full/delta", requestHandling(fun s p -> s.TextDocumentSemanticTokensFullDelta(p))
            "textDocument/semanticTokens/range", requestHandling(fun s p -> s.TextDocumentSemanticTokensRange(p))
            "workspace/didChangeWatchedFiles", requestHandling (fun s p -> s.WorkspaceDidChangeWatchedFiles(p) |> notificationSuccess)
            "workspace/didChangeWorkspaceFolders", requestHandling (fun s p -> s.WorkspaceDidChangeWorkspaceFolders (p) |> notificationSuccess)
            "workspace/didChangeConfiguration", requestHandling (fun s p -> s.WorkspaceDidChangeConfiguration (p) |> notificationSuccess)
            "workspace/symbol", requestHandling (fun s p -> s.WorkspaceSymbol (p))
            "workspace/executeCommand ", requestHandling (fun s p -> s.WorkspaceExecuteCommand (p))
            "shutdown", requestHandling (fun s () -> s.Shutdown() |> notificationSuccess)
            "exit", requestHandling (fun s () -> s.Exit() |> notificationSuccess)
        ]
        |> Map.ofList

    let handleRequest<'server when 'server :> LspServer>  (requestHandlings : Map<string,RequestHandling<'server>>) (request: JsonRpc.Request) (lspServer: 'server): Async<JsonRpc.Response option> =
        async {
            let mutable methodCallResult = Result.Error (Error.MethodNotFound)
            match requestHandlings |> Map.tryFind request.Method with
            | Some handling ->
                try
                    let! result = handling.Run lspServer request.Params
                    methodCallResult <- result
                with
                | ex ->
                    methodCallResult <- Result.Error (Error.Create(ErrorCodes.internalError, ex.ToString()))
            | None -> ()
            match methodCallResult with
            | Result.Ok ok ->
                return Some (JsonRpc.Response.Success(request.Id, ok))
            | Result.Error err ->
                return Some (JsonRpc.Response.Failure(request.Id, err))
        }

    let handleNotification<'server when 'server :> LspServer>  (requestHandlings : Map<string,RequestHandling<'server>>) (notification: JsonRpc.Notification) (lspServer: 'server): Async<Result<unit, _>> =
        async {
            let mutable methodCallResult = Result.Error (Error.MethodNotFound)
            match requestHandlings |> Map.tryFind notification.Method with
            | Some handling ->
                try
                    let! _ = handling.Run lspServer notification.Params
                    methodCallResult <- Result.Ok ()
                with
                | ex ->
                    methodCallResult <- Result.Error (Error.Create(ErrorCodes.internalError, ex.ToString()))
            | None -> ()
            return methodCallResult
        }

    type ClientNotificationSender = string -> obj -> AsyncLspResult<unit>

    type ClientRequestSender =
        abstract member Send<'a> : string -> obj -> AsyncLspResult<'a>

    type private MessageHandlingResult =
        | Normal
        | WasExit
        | WasShutdown

    type LspCloseReason =
        | RequestedByClient = 0
        | ErrorExitWithoutShutdown = 1
        | ErrorStreamClosed = 2

    type ResponseMailboxMsg =
        | Request of int * AsyncReplyChannel<JToken option>
        | Response of int option * Response

    let getNextRequestId =
      let mutable counter = 0
      fun () ->
        counter <- counter + 1
        counter

    let start<'a, 'b when 'a :> LspClient and 'b :> LspServer> (requestHandlings : Map<string,RequestHandling<'b>>) (input: Stream) (output: Stream) (clientCreator: (ClientNotificationSender * ClientRequestSender) -> 'a) (serverCreator: 'a -> 'b) =
        let sender = MailboxProcessor<string>.Start(fun inbox ->
            let rec loop () = async {
                let! str = inbox.Receive()
                LowLevel.write output str
                // do! Async.Sleep 1000
                return! loop ()
            }
            loop ())

        let responseAgent = MailboxProcessor<ResponseMailboxMsg>.Start(fun agent ->
          let rec loop state =
              async{
                  let! msg = agent.Receive()
                  match msg with
                  | Request (rid, reply) ->
                      return! loop (state |> Map.add rid reply)
                  | Response (Some rid, value) ->
                      let result = state |> Map.tryFind rid
                      match result with
                      |Some(reply) ->
                          reply.Reply(value.Result)
                      |None ->
                          logger.warn (Log.setMessage "ResponseAgent - Unexpected response (id {rid}) {response}" >> Log.addContextDestructured "response" value >> Log.addContext "rid" rid)
                      return! loop (state |> Map.remove rid)
                  | Response (None, response) ->
                      logger.error (Log.setMessage "ResponseAgent - Client reports we sent a corrupt request id, error: {error}" >> Log.addContextDestructured "error" (response.Error))
                      return! loop state
              }
          loop Map.empty)


        /// When the server wants to send a notification to the client
        let sendServerNotification (rpcMethod: string) (notificationObj: obj): AsyncLspResult<unit> =
            let serializedNotification = JToken.FromObject(notificationObj, jsonSerializer)
            let notification = JsonRpc.Notification.Create(rpcMethod, Some serializedNotification)
            let notString = JsonConvert.SerializeObject(notification, jsonSettings)
            sender.Post(notString)
            async.Return (LspResult.Ok ())

        /// When the server wants to send a request to the client
        let sendServerRequest (rpcMethod: string) (requestObj: obj): AsyncLspResult<'response> =
            async {
              let serializedRequest =
                if isNull requestObj then
                  None
                else
                  Some (JToken.FromObject(requestObj, jsonSerializer))
              let req = JsonRpc.Request.Create(getNextRequestId(), rpcMethod, serializedRequest)
              let reqString = JsonConvert.SerializeObject(req, jsonSettings)
              sender.Post(reqString)
              let! response = responseAgent.PostAndAsyncReply((fun replyChannel -> Request(req.Id, replyChannel)))
              try
                match response with
                | Some responseToken ->
                    let typedResponse = responseToken.ToObject<'response>(jsonSerializer)
                    return (LspResult.Ok typedResponse)
                | None ->
                    if typeof<'response> = typeof<unit> then
                        return (LspResult.Ok (unbox ()))
                    else
                      logger.error (Log.setMessage "ResponseHandling - Invalid response type for response {response}, expected {expectedType}, got unit" >> Log.addContextDestructured "response" response >> Log.addContext "expectedType" (typeof<'response>).Name)
                      return (LspResult.invalidParams (sprintf "Response params expected for %i but missing" req.Id))
              with
              | :? JsonException as ex ->
                logger.error (Log.setMessage "ResponseHandling - Parsing failed for response {response}" >> Log.addContextDestructured "response" response >> Log.addExn ex)
                return (LspResult.invalidParams (sprintf "Response params invalid for %i" req.Id))
            }

        let lspClient = clientCreator (sendServerNotification, { new ClientRequestSender with member __.Send x t  = sendServerRequest x t})
        use lspServer = serverCreator lspClient

        let handleClientMessage (messageString: string): MessageHandlingResult =
            let messageTypeTest = JsonConvert.DeserializeObject<JsonRpc.MessageTypeTest>(messageString, jsonSettings)
            match getMessageType messageTypeTest with
            | MessageType.Response ->
                let response = JsonConvert.DeserializeObject<JsonRpc.Response>(messageString, jsonSettings)
                responseAgent.Post(Response(response.Id, response))
                MessageHandlingResult.Normal
            | MessageType.Notification ->
                let notification = JsonConvert.DeserializeObject<JsonRpc.Notification>(messageString, jsonSettings)
                async {
                  let! result = handleNotification requestHandlings notification lspServer
                  match result with
                  | Result.Ok _ -> ()
                  | Result.Error ( { Code = code }) when code = JsonRpc.Error.MethodNotFound.Code ->
                    logger.trace (Log.setMessage "don't know how to handle method {messageType}" >> Log.addContext "messageType" notification.Method)
                  | Result.Error (error) ->
                    logger.error (Log.setMessage "HandleClientMessage - Error {error} when handling notification {notification}" >> Log.addContext "error" error >> Log.addContextDestructured "notification" notification)
                    //TODO: Handle error on receiving notification, send message to user?
                    ()
                }
                |> Async.StartAsTask
                |> ignore

                match notification.Method with
                | "exit" -> MessageHandlingResult.WasExit
                | _ -> MessageHandlingResult.Normal
            | MessageType.Request ->
                let request = JsonConvert.DeserializeObject<JsonRpc.Request>(messageString, jsonSettings)
                async {
                  let! result = handleRequest requestHandlings request lspServer
                  match result with
                  | Some response ->
                      let responseString = JsonConvert.SerializeObject(response, jsonSettings)
                      sender.Post(responseString)
                  | None -> ()
                }
                |> Async.StartAsTask
                |> ignore

                match request.Method with
                | "shutdown" -> MessageHandlingResult.WasShutdown
                | _ -> MessageHandlingResult.Normal
            | MessageType.Error ->
                logger.error (Log.setMessage "HandleClientMessage - Message had invalid jsonrpc version: {messageTypeTest}" >> Log.addContextDestructured "messageTypeTest" messageTypeTest)
                MessageHandlingResult.Normal

        let mutable shutdownReceived = false
        let mutable quitReceived = false
        let mutable quit = false
        while not quit do
            try
                let _, requestString = LowLevel.read input

                match handleClientMessage requestString with
                | MessageHandlingResult.WasShutdown -> shutdownReceived <- true
                | MessageHandlingResult.WasExit ->
                    quitReceived <- true
                    quit <- true
                | MessageHandlingResult.Normal -> ()
            with
            | :? EndOfStreamException ->
                quit <- true
            | ex ->
                ()

        match shutdownReceived, quitReceived with
        | true, true -> LspCloseReason.RequestedByClient
        | false, true -> LspCloseReason.ErrorExitWithoutShutdown
        | _ -> LspCloseReason.ErrorStreamClosed

module Client =
    open System
    open System.IO
    open LanguageServerProtocol.Logging
    open Newtonsoft.Json
    open Newtonsoft.Json.Serialization

    open JsonRpc

    let logger = LogProvider.getLoggerByName "LSP Client"

    let internal jsonSettings =
            let result = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
            result.Converters.Add(OptionConverter())
            result.Converters.Add(ErasedUnionConverter())
            result.ContractResolver <- CamelCasePropertyNamesContractResolver()
            result

    let internal jsonSerializer = JsonSerializer.Create(jsonSettings)

    let internal deserialize (token: JToken) = token.ToObject<'t>(jsonSerializer)

    let internal serialize (o: 't) = JToken.FromObject(o, jsonSerializer)

    type NotificationHandler = {
        Run: JToken -> Async<JToken option>
    }

    let notificationHandling<'p, 'r> (handler: 'p -> Async<'r option>) : NotificationHandler =
        let run (token: JToken) =
            async {
                try
                    let p = token.ToObject<'p>(jsonSerializer)
                    let! res = handler p
                    return res |> Option.map (fun n -> JToken.FromObject(n, jsonSerializer))
                with
                | _ -> return None
            }
        {Run = run}

    type Client(exec: string, args: string, notificationHandlings: Map<string, NotificationHandler>) =

        let mutable outuptStream : StreamReader option = None
        let mutable inputStream : StreamWriter option = None

        let sender = MailboxProcessor<string>.Start(fun inbox ->
            let rec loop () = async {
                let! str = inbox.Receive()
                inputStream |> Option.iter (fun input ->
                    // fprintfn stderr "[CLIENT] Writing: %s" str
                    LowLevel.write input.BaseStream str
                    input.BaseStream.Flush()
                    )
                // do! Async.Sleep 1000
                return! loop ()
            }
            loop ())

        let handleRequest (request: JsonRpc.Request) =
            async {
                let mutable methodCallResult = None
                match notificationHandlings |> Map.tryFind request.Method with
                | Some handling ->
                    try
                        match request.Params with
                        | None -> ()
                        | Some prms ->
                            let! result = handling.Run prms
                            methodCallResult <- result
                    with
                    | ex ->
                        methodCallResult <- None
                | None -> ()

                match methodCallResult with
                | Some ok ->
                    return Some (JsonRpc.Response.Success(request.Id, Some ok))
                | None ->
                    return None
            }

        let handleNotification (notification: JsonRpc.Notification) =
            async {
                match notificationHandlings |> Map.tryFind notification.Method with
                | Some handling ->
                    try
                        match notification.Params with
                        | None -> return Result.Error (Error.InvalidParams)
                        | Some prms ->
                            let! result = handling.Run prms
                            return Result.Ok ()
                    with
                    | ex ->
                        return Result.Error (Error.Create(ErrorCodes.internalError, ex.ToString()))
                | None ->
                  return Result.Error (Error.MethodNotFound)
            }

        let messageHanlder str =
            let messageTypeTest = JsonConvert.DeserializeObject<JsonRpc.MessageTypeTest>(str, jsonSettings)
            match getMessageType messageTypeTest with
            | MessageType.Notification ->
                let notification = JsonConvert.DeserializeObject<JsonRpc.Notification>(str, jsonSettings)
                async {
                  let! result = handleNotification notification
                  match result with
                  | Result.Ok _ -> ()
                  | Result.Error error ->
                    logger.error (Log.setMessage "HandleServerMessage - Error {error} when handling notification {notification}" >> Log.addContextDestructured "error" error >> Log.addContextDestructured "notification" notification)
                    //TODO: Handle error on receiving notification, send message to user?
                    ()
                }
                |> Async.StartAsTask
                |> ignore
            | MessageType.Request ->
                let request = JsonConvert.DeserializeObject<JsonRpc.Request>(str, jsonSettings)
                async {
                  let! result = handleRequest request
                  match result with
                  | Some response ->
                      let responseString = JsonConvert.SerializeObject(response, jsonSettings)
                      sender.Post(responseString)
                  | None -> ()
                }
                |> Async.StartAsTask
                |> ignore
            | MessageType.Response
            | MessageType.Error ->
                logger.error (Log.setMessage "HandleServerMessage - Message had invalid jsonrpc version: {messageTypeTest}" >> Log.addContextDestructured "messageTypeTest" messageTypeTest)
                ()

            let request = JsonConvert.DeserializeObject<JsonRpc.Request>(str, jsonSettings)
            async {
                let! result = handleRequest request
                match result with
                | Some response ->
                    let responseString = JsonConvert.SerializeObject(response, jsonSettings)
                    sender.Post(responseString)
                | None -> ()
            }
            |> Async.StartAsTask
            |> ignore

        member __.SendNotification (rpcMethod: string) (requestObj: obj) =
            let serializedResponse = JToken.FromObject(requestObj, jsonSerializer)
            let notification = JsonRpc.Notification.Create(rpcMethod, Some serializedResponse)
            let notString = JsonConvert.SerializeObject(notification, jsonSettings)
            sender.Post(notString)

        member __.Start() =
            async {
                let si = ProcessStartInfo()
                si.RedirectStandardOutput <- true
                si.RedirectStandardInput <- true
                si.RedirectStandardError <- true
                si.UseShellExecute <- false
                si.WorkingDirectory <- Environment.CurrentDirectory
                si.FileName <- exec
                si.Arguments <- args
                let proc =
                    try
                        Process.Start(si)
                    with ex ->
                        let newEx = System.Exception(sprintf "%s on %s" ex.Message exec, ex)
                        raise newEx

                inputStream <- Some (proc.StandardInput)
                outuptStream <- Some (proc.StandardOutput)

                let mutable quit = false
                let outStream = proc.StandardOutput.BaseStream
                while not quit do
                    try
                        let _, notificationString = LowLevel.read outStream
                        // fprintfn stderr "[CLIENT] READING: %s" notificationString
                        messageHanlder notificationString
                    with
                    | :? EndOfStreamException ->
                        quit <- true
                    | ex ->
                        ()

                return ()
            } |> Async.Start