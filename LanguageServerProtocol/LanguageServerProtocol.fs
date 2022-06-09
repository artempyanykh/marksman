namespace Ionide.LanguageServerProtocol

module LspJsonConverters =
  open Microsoft.FSharp.Reflection
  open Newtonsoft.Json
  open System
  open System.Collections.Concurrent
  open Ionide.LanguageServerProtocol.Types

  let inline memorise (f: 'a -> 'b) : ('a -> 'b) =
    let d = ConcurrentDictionary<'a, 'b>()
    fun key -> d.GetOrAdd(key, f)

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

    override __.ReadJson(_reader, _t, _existingValue, _serializer) = failwith "Not implemented"

  /// converter that can convert enum-style DUs
  type SingleCaseUnionConverter() =
    inherit JsonConverter()


    let canConvert =
      let allCases (t: System.Type) = FSharpType.GetUnionCases t

      memorise (fun t ->
        FSharpType.IsUnion t
        && allCases t
           |> Array.forall (fun c -> c.GetFields().Length = 0))

    override _.CanConvert t = canConvert t

    override _.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: obj, serializer: Newtonsoft.Json.JsonSerializer) =
      serializer.Serialize(writer, string value)

    override _.ReadJson(reader: Newtonsoft.Json.JsonReader, t, _existingValue, serializer) =
      let caseName = string reader.Value

      match
        FSharpType.GetUnionCases(t)
        |> Array.tryFind (fun c -> c.Name.Equals(caseName, StringComparison.OrdinalIgnoreCase))
        with
      | Some caseInfo -> FSharpValue.MakeUnion(caseInfo, [||])
      | None -> failwith $"Could not create an instance of the type '%s{t.Name}' with the name '%s{caseName}'"

  type U2BoolObjectConverter() =
    inherit JsonConverter()

    let canConvert =
      memorise (fun (t: System.Type) ->
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<U2<_, _>>
        && t.GetGenericArguments().Length = 2
        && t.GetGenericArguments().[0] = typeof<bool>
        && not (t.GetGenericArguments().[1].IsValueType))

    override _.CanConvert t = canConvert t

    override _.WriteJson(writer, value, serializer) =
      let case, fields = FSharpValue.GetUnionFields(value, value.GetType())

      match case.Name with
      | "First" -> writer.WriteValue(value :?> bool)
      | "Second" -> serializer.Serialize(writer, fields.[0])
      | _ -> failwith $"Unrecognized case '{case.Name}' for union type '{value.GetType().FullName}'."

    override _.ReadJson(reader, t, _existingValue, serializer) =
      let cases = FSharpType.GetUnionCases(t)

      match reader.TokenType with
      | JsonToken.Boolean ->
        // 'First' side
        FSharpValue.MakeUnion(cases.[0], [| box (reader.Value :?> bool) |])
      | JsonToken.StartObject ->
        // Second side
        let value = serializer.Deserialize(reader, (t.GetGenericArguments().[1]))
        FSharpValue.MakeUnion(cases.[1], [| value |])
      | _ ->
        failwithf $"Unrecognized json TokenType '%s{string reader.TokenType}' when reading value of type '{t.FullName}'"

  type OptionConverter() =
    inherit JsonConverter()

    override __.CanConvert(t) =
      t.IsGenericType
      && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override __.WriteJson(writer, value, serializer) =
      let value =
        if isNull value then
          null
        else
          let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
          fields.[0]

      serializer.Serialize(writer, value)

    override __.ReadJson(reader, t, _existingValue, serializer) =
      let innerType = t.GetGenericArguments().[0]

      let innerType =
        if innerType.IsValueType then
          (typedefof<Nullable<_>>).MakeGenericType([| innerType |])
        else
          innerType

      let value = serializer.Deserialize(reader, innerType)
      let cases = FSharpType.GetUnionCases(t)

      if isNull value then
        FSharpValue.MakeUnion(cases.[0], [||])
      else
        FSharpValue.MakeUnion(cases.[1], [| value |])


module Server =
  open System
  open System.IO
  open Ionide.LanguageServerProtocol.Logging
  open Ionide.LanguageServerProtocol.Types
  open System.Threading
  open System.Threading.Tasks
  open System.Reflection
  open StreamJsonRpc
  open Newtonsoft.Json
  open Newtonsoft.Json.Serialization
  open LspJsonConverters
  open Newtonsoft.Json.Linq

  let logger = LogProvider.getLoggerByName "LSP Server"

  let jsonRpcFormatter = new JsonMessageFormatter()
  jsonRpcFormatter.JsonSerializer.NullValueHandling <- NullValueHandling.Ignore
  jsonRpcFormatter.JsonSerializer.ConstructorHandling <- ConstructorHandling.AllowNonPublicDefaultConstructor
  jsonRpcFormatter.JsonSerializer.MissingMemberHandling <- MissingMemberHandling.Ignore
  jsonRpcFormatter.JsonSerializer.Converters.Add(SingleCaseUnionConverter())
  jsonRpcFormatter.JsonSerializer.Converters.Add(U2BoolObjectConverter())
  jsonRpcFormatter.JsonSerializer.Converters.Add(OptionConverter())
  jsonRpcFormatter.JsonSerializer.Converters.Add(ErasedUnionConverter())
  jsonRpcFormatter.JsonSerializer.ContractResolver <- CamelCasePropertyNamesContractResolver()

  let deserialize<'t> (token: JToken) = token.ToObject<'t>(jsonRpcFormatter.JsonSerializer)
  let serialize<'t> (o: 't) = JToken.FromObject(o, jsonRpcFormatter.JsonSerializer)

  let requestHandling<'param, 'result> (run: 'param -> AsyncLspResult<'result>) : Delegate =
    let runAsTask param ct =
      let asyncLspResult =
        try
          // Although `run` returns an async result, its body may not be fully in an async context.
          // Here we make sure that we catch and properly handle any exception before the first async.
          run param
        with
        | ex ->
          let rpcException = LocalRpcException(ex.Message)
          rpcException.ErrorCode <- JsonRpc.ErrorCodes.internalError
          rpcException.ErrorData <- ex.Data
          raise rpcException

      let asyncContinuation =
        async {
          let! lspResult = asyncLspResult

          return
            match lspResult with
            | Ok result -> result
            | Error error ->
              let rpcException = LocalRpcException(error.Message)
              rpcException.ErrorCode <- error.Code
              rpcException.ErrorData <- error.Data |> Option.defaultValue null
              raise rpcException
        }

      Async.StartAsTask(asyncContinuation, cancellationToken = ct)

    Func<'param, CancellationToken, Task<'result>>(runAsTask) :> Delegate

  /// Notifications don't generate a response or error, but to unify things we consider them as always successful.
  /// They will still not send any response because their ID is null.
  let private notificationSuccess (response: Async<unit>) =
    async {
      do! response
      return Result.Ok()
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

  let startWithSetup<'client when 'client :> Ionide.LanguageServerProtocol.LspClient>
    (setupRequestHandlings: 'client -> Map<string, Delegate>)
    (input: Stream)
    (output: Stream)
    (clientCreator: (ClientNotificationSender * ClientRequestSender) -> 'client)
    =

    use jsonRpcHandler = new HeaderDelimitedMessageHandler(output, input, jsonRpcFormatter)
    use jsonRpc = new JsonRpc(jsonRpcHandler)

    /// When the server wants to send a notification to the client
    let sendServerNotification (rpcMethod: string) (notificationObj: obj) : AsyncLspResult<unit> =
      async {
        do!
          jsonRpc.NotifyWithParameterObjectAsync(rpcMethod, notificationObj)
          |> Async.AwaitTask

        return () |> LspResult.success
      }

    /// When the server wants to send a request to the client
    let sendServerRequest (rpcMethod: string) (requestObj: obj) : AsyncLspResult<'response> =
      async {
        let! response =
          jsonRpc.InvokeWithParameterObjectAsync<'response>(rpcMethod, requestObj)
          |> Async.AwaitTask

        return response |> LspResult.success
      }

    let lspClient =
      clientCreator (
        sendServerNotification,
        { new ClientRequestSender with
            member __.Send x t = sendServerRequest x t }
      )

    let mutable shutdownReceived = false
    let mutable quitReceived = false
    use quitSemaphore = new SemaphoreSlim(0, 1)

    let onShutdown () =
      logger.trace (Log.setMessage "Shutdown received")
      shutdownReceived <- true
      // VSCode Language Client has a bug that causes it to NOT send an `exit` notification when stopping a server:
      // https://github.com/microsoft/vscode-languageserver-node/pull/776 This may result in a bunch of zombie language
      // servers just hanging around after a few reloads/restarts.  Although the fix was merged a while ago, the new
      // client is yet to be released.  As a workaround let's forcefully exit after 10s after receiving a `shutdown`
      // request.
      task {
        do! Task.Delay(10_000)
        logger.trace (Log.setMessage "No `exit` notification within 10s after `shutdown` request. Exiting now.")
        quitSemaphore.Release() |> ignore
      }
      |> ignore

    jsonRpc.AddLocalRpcMethod("shutdown", Action(onShutdown))

    let onExit () =
      logger.trace (Log.setMessage "Exit received")
      quitReceived <- true
      quitSemaphore.Release() |> ignore

    jsonRpc.AddLocalRpcMethod("exit", Action(onExit))

    for handling in setupRequestHandlings lspClient do
      let rpcMethodName = handling.Key
      let rpcDelegate = handling.Value

      let rpcAttribute = JsonRpcMethodAttribute(rpcMethodName)
      rpcAttribute.UseSingleObjectParameterDeserialization <- true

      jsonRpc.AddLocalRpcMethod(rpcDelegate.GetMethodInfo(), rpcDelegate.Target, rpcAttribute)

    jsonRpc.StartListening()

    quitSemaphore.Wait()

    match shutdownReceived, quitReceived with
    | true, true -> LspCloseReason.RequestedByClient
    | false, true -> LspCloseReason.ErrorExitWithoutShutdown
    | _ -> LspCloseReason.ErrorStreamClosed

  type ServerRequestHandling<'server when 'server :> Ionide.LanguageServerProtocol.LspServer> =
    { Run: 'server -> Delegate }

  let serverRequestHandling<'server, 'param, 'result when 'server :> Ionide.LanguageServerProtocol.LspServer>
    (run: 'server -> 'param -> AsyncLspResult<'result>)
    : ServerRequestHandling<'server> =
    { Run = fun s -> requestHandling (run s) }

  let defaultRequestHandlings () : Map<string, ServerRequestHandling<'server>> =
    let requestHandling = serverRequestHandling

    [ "initialize", requestHandling (fun s p -> s.Initialize(p))
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
      "textDocument/semanticTokens/full", requestHandling (fun s p -> s.TextDocumentSemanticTokensFull(p))
      "textDocument/semanticTokens/full/delta", requestHandling (fun s p -> s.TextDocumentSemanticTokensFullDelta(p))
      "textDocument/semanticTokens/range", requestHandling (fun s p -> s.TextDocumentSemanticTokensRange(p))
      "textDocument/inlayHint", requestHandling (fun s p -> s.TextDocumentInlayHint(p))
      "inlayHint/resolve", requestHandling (fun s p -> s.InlayHintResolve(p))
      "workspace/didChangeWatchedFiles",
      requestHandling (fun s p -> s.WorkspaceDidChangeWatchedFiles(p) |> notificationSuccess)
      "workspace/didChangeWorkspaceFolders",
      requestHandling (fun s p ->
        s.WorkspaceDidChangeWorkspaceFolders(p)
        |> notificationSuccess)
      "workspace/didChangeConfiguration",
      requestHandling (fun s p -> s.WorkspaceDidChangeConfiguration(p) |> notificationSuccess)
      "workspace/willCreateFiles", requestHandling (fun s p -> s.WorkspaceWillCreateFiles(p))
      "workspace/didCreateFiles", requestHandling (fun s p -> s.WorkspaceDidCreateFiles(p) |> notificationSuccess)
      "workspace/willRenameFiles", requestHandling (fun s p -> s.WorkspaceWillRenameFiles(p))
      "workspace/didRenameFiles", requestHandling (fun s p -> s.WorkspaceDidRenameFiles(p) |> notificationSuccess)
      "workspace/willDeleteFiles", requestHandling (fun s p -> s.WorkspaceWillDeleteFiles(p))
      "workspace/didDeleteFiles", requestHandling (fun s p -> s.WorkspaceDidDeleteFiles(p) |> notificationSuccess)
      "workspace/symbol", requestHandling (fun s p -> s.WorkspaceSymbol(p))
      "workspace/executeCommand", requestHandling (fun s p -> s.WorkspaceExecuteCommand(p))
      "shutdown", requestHandling (fun s () -> s.Shutdown() |> notificationSuccess)
      "exit", requestHandling (fun s () -> s.Exit() |> notificationSuccess) ]
    |> Map.ofList

  let start<'client, 'server when 'client :> Ionide.LanguageServerProtocol.LspClient and 'server :> Ionide.LanguageServerProtocol.LspServer>
    (requestHandlings: Map<string, ServerRequestHandling<'server>>)
    (input: Stream)
    (output: Stream)
    (clientCreator: (ClientNotificationSender * ClientRequestSender) -> 'client)
    (serverCreator: 'client -> 'server)
    =
    let requestHandlingSetup client =
      let server = serverCreator client

      requestHandlings
      |> Map.map (fun _ requestHandling -> requestHandling.Run server)

    startWithSetup requestHandlingSetup input output clientCreator

module Client =
  open System
  open System.Diagnostics
  open System.IO
  open Ionide.LanguageServerProtocol
  open Ionide.LanguageServerProtocol.JsonRpc
  open Ionide.LanguageServerProtocol.Logging
  open LspJsonConverters
  open Newtonsoft.Json
  open Newtonsoft.Json.Serialization
  open Newtonsoft.Json.Linq


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

  type NotificationHandler = { Run: JToken -> Async<JToken option> }

  let notificationHandling<'p, 'r> (handler: 'p -> Async<'r option>) : NotificationHandler =
    let run (token: JToken) =
      async {
        try
          let p = token.ToObject<'p>(jsonSerializer)
          let! res = handler p

          return
            res
            |> Option.map (fun n -> JToken.FromObject(n, jsonSerializer))
        with
        | _ -> return None
      }

    { Run = run }

  // TODO: replace this module with StreamJsonRpc like we did for the server
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
        while count < headerBufferSize
              && (buffer.[count - 2] <> cr && buffer.[count - 1] <> lf) do
          let additionalBytesRead = stream.Read(buffer, count, 1)
          // TODO: exit when additionalBytesRead = 0, end of stream
          count <- count + additionalBytesRead

        if count >= headerBufferSize then
          None
        else
          Some(headerEncoding.GetString(buffer, 0, count - 2))

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
          (name, value) :: otherHeaders
      | None -> raise (EndOfStreamException())

    let read (stream: Stream) =
      let headers = readHeaders stream

      let contentLength =
        headers
        |> List.tryFind (fun (name, _) -> name = "Content-Length")
        |> Option.map snd
        |> Option.bind (fun s ->
          match Int32.TryParse(s) with
          | true, x -> Some x
          | _ -> None)

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

      let header =
        sprintf "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: %d\r\n\r\n" bytes.Length

      let headerBytes = Encoding.ASCII.GetBytes header
      use ms = new MemoryStream(headerBytes.Length + bytes.Length)
      ms.Write(headerBytes, 0, headerBytes.Length)
      ms.Write(bytes, 0, bytes.Length)
      stream.Write(ms.ToArray(), 0, int ms.Position)

  type Client(exec: string, args: string, notificationHandlings: Map<string, NotificationHandler>) =

    let mutable outuptStream: StreamReader option = None
    let mutable inputStream: StreamWriter option = None

    let sender =
      MailboxProcessor<string>.Start
        (fun inbox ->
          let rec loop () =
            async {
              let! str = inbox.Receive()

              inputStream
              |> Option.iter (fun input ->
                // fprintfn stderr "[CLIENT] Writing: %s" str
                LowLevel.write input.BaseStream str
                input.BaseStream.Flush())
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
          | ex -> methodCallResult <- None
        | None -> ()

        match methodCallResult with
        | Some ok -> return Some(JsonRpc.Response.Success(request.Id, Some ok))
        | None -> return None
      }

    let handleNotification (notification: JsonRpc.Notification) =
      async {
        match notificationHandlings |> Map.tryFind notification.Method with
        | Some handling ->
          try
            match notification.Params with
            | None -> return Result.Error(JsonRpc.Error.InvalidParams)
            | Some prms ->
              let! result = handling.Run prms
              return Result.Ok()
          with
          | ex -> return Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.internalError, ex.ToString()))
        | None -> return Result.Error(JsonRpc.Error.MethodNotFound)
      }

    let messageHandler str =
      let messageTypeTest = JsonConvert.DeserializeObject<JsonRpc.MessageTypeTest>(str, jsonSettings)

      match getMessageType messageTypeTest with
      | MessageType.Notification ->
        let notification = JsonConvert.DeserializeObject<JsonRpc.Notification>(str, jsonSettings)

        async {
          let! result = handleNotification notification

          match result with
          | Result.Ok _ -> ()
          | Result.Error error ->
            logger.error (
              Log.setMessage "HandleServerMessage - Error {error} when handling notification {notification}"
              >> Log.addContextDestructured "error" error
              >> Log.addContextDestructured "notification" notification
            )
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
        logger.error (
          Log.setMessage "HandleServerMessage - Message had invalid jsonrpc version: {messageTypeTest}"
          >> Log.addContextDestructured "messageTypeTest" messageTypeTest
        )

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
          with
          | ex ->
            let newEx = System.Exception(sprintf "%s on %s" ex.Message exec, ex)
            raise newEx

        inputStream <- Some(proc.StandardInput)
        outuptStream <- Some(proc.StandardOutput)

        let mutable quit = false
        let outStream = proc.StandardOutput.BaseStream

        while not quit do
          try
            let _, notificationString = LowLevel.read outStream
            // fprintfn stderr "[CLIENT] READING: %s" notificationString
            messageHandler notificationString
          with
          | :? EndOfStreamException -> quit <- true
          | ex -> ()

        return ()
      }
      |> Async.Start
