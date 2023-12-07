module Ionide.LanguageServerProtocol.JsonRpc

open Newtonsoft.Json
open Newtonsoft.Json.Linq

type MessageTypeTest =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Id: int option
    Method: string option }

[<RequireQualifiedAccess>]
type MessageType =
  | Notification
  | Request
  | Response
  | Error

let getMessageType messageTest =
  match messageTest with
  | { Version = "2.0"; Id = Some _; Method = Some _ } -> MessageType.Request
  | { Version = "2.0"; Id = Some _; Method = None } -> MessageType.Response
  | { Version = "2.0"; Id = None; Method = Some _ } -> MessageType.Notification
  | _ -> MessageType.Error

type Request =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Id: int
    Method: string
    Params: JToken option }

  static member Create(id: int, method': string, rpcParams: JToken option) =
    { Version = "2.0"; Id = id; Method = method'; Params = rpcParams }

type Notification =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Method: string
    Params: JToken option }

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
  let requestCancelled = -32800

type Error =
  { Code: int
    Message: string
    Data: JToken option }

  static member Create(code: int, message: string) = { Code = code; Message = message; Data = None }
  static member ParseError = Error.Create(ErrorCodes.parseError, "Parse error")
  static member InvalidRequest = Error.Create(ErrorCodes.invalidRequest, "Invalid Request")
  static member MethodNotFound = Error.Create(ErrorCodes.methodNotFound, "Method not found")
  static member InvalidParams = Error.Create(ErrorCodes.invalidParams, "Invalid params")
  static member InternalError = Error.Create(ErrorCodes.internalError, "Internal error")
  static member InternalErrorMessage message = Error.Create(ErrorCodes.internalError, message)
  static member RequestCancelled = Error.Create(ErrorCodes.requestCancelled, "Request cancelled")
  static member RequestCancelledMessage message = Error.Create(ErrorCodes.requestCancelled, message)

type Response =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Id: int option
    Error: Error option
    [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
    Result: JToken option }

  /// Json.NET conditional property serialization, controlled by naming convention
  member x.ShouldSerializeResult() = x.Error.IsNone

  static member Success(id: int, result: JToken option) =
    { Version = "2.0"; Id = Some id; Result = result; Error = None }

  static member Failure(id: int, error: Error) = { Version = "2.0"; Id = Some id; Result = None; Error = Some error }