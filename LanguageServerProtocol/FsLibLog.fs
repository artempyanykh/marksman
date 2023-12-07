// this is from https://github.com/TheAngryByrd/FsLibLog/blob/f81cba440bf0476bb4e2262b57a067a0d6ab78a7/src/FsLibLog/FsLibLog.fs

namespace Ionide.LanguageServerProtocol.Logging


[<AutoOpen>]
module Types =
  open System

  type LogLevel =
    | Trace = 0
    | Debug = 1
    | Info = 2
    | Warn = 3
    | Error = 4
    | Fatal = 5

  /// An optional message thunk.
  ///
  /// - If `None` is provided, this typically signals to the logger to do a isEnabled check.
  /// - If `Some` is provided, this signals the logger to log.
  type MessageThunk = (unit -> string) option

  /// The signature of a log message function
  type Logger = LogLevel -> MessageThunk -> exn option -> obj array -> bool
  type MappedContext = string -> obj -> bool -> IDisposable

  /// Type representing a Log
  type Log =
    { LogLevel: LogLevel
      Message: MessageThunk
      Exception: exn option
      Parameters: obj list
      AdditionalNamedParameters: ((string * obj * bool) list) }

    static member StartLogLevel(logLevel: LogLevel) =
      { LogLevel = logLevel
        Message = None
        Exception = None
        Parameters = List.empty
        AdditionalNamedParameters = List.empty }

  /// An interface wrapper for `Logger`. Useful when using depedency injection frameworks.
  type ILog =
    abstract member Log: Logger
    abstract member MappedContext: MappedContext

  [<AutoOpen>]
  module Inner =
    open System.Collections.Generic

    type DisposableStack() =
      let stack = Stack<IDisposable>()

      interface IDisposable with
        member __.Dispose() =
          while stack.Count > 0 do
            stack.Pop().Dispose()

      member __.Push(item: IDisposable) = stack.Push item
      member __.Push(items: IDisposable list) = items |> List.iter stack.Push

      static member Create(items: IDisposable list) =
        let ds = new DisposableStack()
        ds.Push items
        ds

    type ILog with

      /// **Description**
      ///
      /// Logs a log
      ///
      /// **Parameters**
      ///   * `log` - parameter of type `Log`
      ///
      /// **Output Type**
      ///   * `bool`
      member logger.fromLog(log: Log) =
        use __ =
          log.AdditionalNamedParameters
          |> List.map (fun (key, value, destructure) -> logger.MappedContext key value destructure)
          // This stack is important, it causes us to unwind as if you have multiple uses in a row
          |> DisposableStack.Create

        log.Parameters
        |> List.toArray
        |> logger.Log log.LogLevel log.Message log.Exception

      /// **Description**
      ///
      /// Logs a fatal log message given a log configurer.  Lets caller know if log was sent with boolean return.
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `bool`
      member logger.fatal'(logConfig: Log -> Log) =
        Log.StartLogLevel LogLevel.Fatal
        |> logConfig
        |> logger.fromLog

      /// **Description**
      ///
      /// Logs a fatal log message given a log configurer
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `unit`
      member logger.fatal(logConfig: Log -> Log) = logger.fatal' logConfig |> ignore

      /// **Description**
      ///
      /// Logs a error log message given a log configurer.  Lets caller know if log was sent with boolean return.
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `bool`
      member logger.error'(logConfig: Log -> Log) =
        Log.StartLogLevel LogLevel.Error
        |> logConfig
        |> logger.fromLog

      /// **Description**
      ///
      /// Logs an error log message given a log configurer
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `unit`
      member logger.error(logConfig: Log -> Log) = logger.error' logConfig |> ignore

      /// **Description**
      ///
      /// Logs a warn log message given a log configurer.  Lets caller know if log was sent with boolean return.
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `bool`
      member logger.warn'(logConfig: Log -> Log) =
        Log.StartLogLevel LogLevel.Warn
        |> logConfig
        |> logger.fromLog

      /// **Description**
      ///
      /// Logs a warn log message given a log configurer
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `unit`
      member logger.warn(logConfig: Log -> Log) = logger.warn' logConfig |> ignore

      /// **Description**
      ///
      /// Logs a info log message given a log configurer.  Lets caller know if log was sent with boolean return.
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `bool`
      member logger.info'(logConfig: Log -> Log) =
        Log.StartLogLevel LogLevel.Info
        |> logConfig
        |> logger.fromLog

      /// **Description**
      ///
      /// Logs a info log message given a log configurer
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `unit`
      member logger.info(logConfig: Log -> Log) = logger.info' logConfig |> ignore

      /// **Description**
      ///
      /// Logs a debug log message given a log configurer.  Lets caller know if log was sent with boolean return.
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `bool`
      member logger.debug'(logConfig: Log -> Log) =
        Log.StartLogLevel LogLevel.Debug
        |> logConfig
        |> logger.fromLog

      /// **Description**
      ///
      /// Logs a debug log message given a log configurer
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `unit`
      member logger.debug(logConfig: Log -> Log) = logger.debug' logConfig |> ignore

      /// **Description**
      ///
      /// Logs a trace log message given a log configurer.  Lets caller know if log was sent with boolean return.
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `bool`
      member logger.trace'(logConfig: Log -> Log) =
        Log.StartLogLevel LogLevel.Trace
        |> logConfig
        |> logger.fromLog

      /// **Description**
      ///
      /// Logs a trace log message given a log configurer
      ///
      /// **Parameters**
      ///   * `logConfig` - parameter of type `Log -> Log`
      ///
      /// **Output Type**
      ///   * `unit`
      member logger.trace(logConfig: Log -> Log) = logger.trace' logConfig |> ignore


  /// An interface for retrieving a concrete logger such as Serilog, Nlog, etc.
  type ILogProvider =
    abstract member GetLogger: string -> Logger
    abstract member OpenNestedContext: string -> IDisposable
    abstract member OpenMappedContext: string -> obj -> bool -> IDisposable

  module Log =

    /// **Description**
    ///
    /// Amends a `Log` with a message
    ///
    /// **Parameters**
    ///   * `message` - parameter of type `string`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    let setMessage (message: string) (log: Log) = { log with Message = Some(fun () -> message) }

    /// **Description**
    ///
    /// Amends a `Log` with a message thunk.  Useful for "expensive" string construction scenarios.
    ///
    /// **Parameters**
    ///   * `messageThunk` - parameter of type `unit -> string`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let setMessageThunk (messageThunk: unit -> string) (log: Log) = { log with Message = Some messageThunk }

    /// **Description**
    ///
    /// Amends a `Log` with a parameter.
    ///
    /// **Parameters**
    ///   * `param` - parameter of type `'a`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let addParameter (param: 'a) (log: Log) = { log with Parameters = List.append log.Parameters [ (box param) ] }

    /// **Description**
    ///
    /// Amends a `Log` with a list of parameters.
    ///
    /// **Parameters**
    ///   * `params` - parameter of type `obj list`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let addParameters (``params``: obj list) (log: Log) =
      let ``params`` = ``params`` |> List.map box
      { log with Parameters = log.Parameters @ ``params`` }



    /// **Description**
    ///
    /// Amends a `Log` with additional named parameters for context. This helper adds more context to a log.
    /// This DOES NOT affect the parameters set for a message template.
    /// This is the same calling OpenMappedContext right before logging.
    ///
    /// **Parameters**
    ///   * `key` - parameter of type `string`
    ///   * `value` - parameter of type `obj`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let addContext (key: string) (value: obj) (log: Log) =
      { log with
          AdditionalNamedParameters = List.append log.AdditionalNamedParameters [ key, (box value), false ] }


    /// **Description**
    ///
    /// Amends a `Log` with additional named parameters for context. This helper adds more context to a log.
    /// This DOES NOT affect the parameters set for a message template.
    /// This is the same calling OpenMappedContext right before logging.
    /// This destructures an object rather than calling `ToString()` on it.
    /// WARNING: Destructring can be expensive.
    ///
    /// **Parameters**
    ///   * `key` - parameter of type `string`
    ///   * `value` - parameter of type `obj`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let addContextDestructured (key: string) (value: obj) (log: Log) =
      { log with
          AdditionalNamedParameters = List.append log.AdditionalNamedParameters [ key, (box value), true ] }


    /// **Description**
    ///
    /// Amends a `Log` with an `exn`. Handles nulls.
    ///
    /// **Parameters**
    ///   * `exception` - parameter of type `exn`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let addException (``exception``: exn) (log: Log) = { log with Exception = Option.ofObj ``exception`` }

    /// **Description**
    ///
    /// Amends a `Log` with an `exn`.  Handles nulls.
    ///
    /// **Parameters**
    ///   * `exception` - parameter of type `exn`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let addExn (``exception``: exn) (log: Log) = addException ``exception`` log

    /// **Description**
    ///
    /// Amends a `Log` with a given `LogLevel`
    ///
    /// **Parameters**
    ///   * `logLevel` - parameter of type `LogLevel`
    ///   * `log` - parameter of type `Log`
    ///
    /// **Output Type**
    ///   * `Log`
    ///
    /// **Exceptions**
    ///
    let setLogLevel (logLevel: LogLevel) (log: Log) = { log with LogLevel = logLevel }

module Providers =
  module SerilogProvider =
    open System
    open System.Linq.Expressions

    let getLogManagerType () = Type.GetType("Serilog.Log, Serilog")
    let isAvailable () = getLogManagerType () |> isNull |> not

    let getPushProperty () =

      let ndcContextType =
        Type.GetType("Serilog.Context.LogContext, Serilog")
        |> Option.ofObj
        |> Option.defaultWith (fun () -> Type.GetType("Serilog.Context.LogContext, Serilog.FullNetFx"))

      ()

      let pushPropertyMethod =
        ndcContextType.GetMethod("PushProperty", [| typedefof<string>; typedefof<obj>; typedefof<bool> |])

      let nameParam = Expression.Parameter(typedefof<string>, "name")
      let valueParam = Expression.Parameter(typedefof<obj>, "value")
      let destructureObjectParam = Expression.Parameter(typedefof<bool>, "destructureObjects")

      let pushPropertyMethodCall =
        Expression.Call(null, pushPropertyMethod, nameParam, valueParam, destructureObjectParam)

      let pushProperty =
        Expression
          .Lambda<Func<string, obj, bool, IDisposable>>(
            pushPropertyMethodCall,
            nameParam,
            valueParam,
            destructureObjectParam
          )
          .Compile()

      fun key value destructure -> pushProperty.Invoke(key, value, destructure)


    let getForContextMethodCall () =
      let logManagerType = getLogManagerType ()
      let method = logManagerType.GetMethod("ForContext", [| typedefof<string>; typedefof<obj>; typedefof<bool> |])
      let propertyNameParam = Expression.Parameter(typedefof<string>, "propertyName")
      let valueParam = Expression.Parameter(typedefof<obj>, "value")
      let destructureObjectsParam = Expression.Parameter(typedefof<bool>, "destructureObjects")
      let exrs: Expression[] = [| propertyNameParam; valueParam; destructureObjectsParam |]
      let methodCall = Expression.Call(null, method, exrs)

      let func =
        Expression
          .Lambda<Func<string, obj, bool, obj>>(methodCall, propertyNameParam, valueParam, destructureObjectsParam)
          .Compile()

      fun name -> func.Invoke("SourceContext", name, false)

    type SerilogGateway =
      { Write: obj -> obj -> string -> obj[] -> unit
        WriteException: obj -> obj -> exn -> string -> obj[] -> unit
        IsEnabled: obj -> obj -> bool
        TranslateLevel: LogLevel -> obj }

      static member Create() =
        let logEventLevelType = Type.GetType("Serilog.Events.LogEventLevel, Serilog")

        if (logEventLevelType |> isNull) then
          failwith ("Type Serilog.Events.LogEventLevel was not found.")

        let debugLevel = Enum.Parse(logEventLevelType, "Debug", false)
        let errorLevel = Enum.Parse(logEventLevelType, "Error", false)
        let fatalLevel = Enum.Parse(logEventLevelType, "Fatal", false)
        let informationLevel = Enum.Parse(logEventLevelType, "Information", false)
        let verboseLevel = Enum.Parse(logEventLevelType, "Verbose", false)
        let warningLevel = Enum.Parse(logEventLevelType, "Warning", false)

        let translateLevel (level: LogLevel) =
          match level with
          | LogLevel.Fatal -> fatalLevel
          | LogLevel.Error -> errorLevel
          | LogLevel.Warn -> warningLevel
          | LogLevel.Info -> informationLevel
          | LogLevel.Debug -> debugLevel
          | LogLevel.Trace -> verboseLevel
          | _ -> debugLevel

        let loggerType = Type.GetType("Serilog.ILogger, Serilog")

        if (loggerType |> isNull) then
          failwith ("Type Serilog.ILogger was not found.")

        let isEnabledMethodInfo = loggerType.GetMethod("IsEnabled", [| logEventLevelType |])
        let instanceParam = Expression.Parameter(typedefof<obj>)
        let instanceCast = Expression.Convert(instanceParam, loggerType)
        let levelParam = Expression.Parameter(typedefof<obj>)
        let levelCast = Expression.Convert(levelParam, logEventLevelType)
        let isEnabledMethodCall = Expression.Call(instanceCast, isEnabledMethodInfo, levelCast)

        let isEnabled =
          Expression.Lambda<Func<obj, obj, bool>>(isEnabledMethodCall, instanceParam, levelParam).Compile()

        let writeMethodInfo =
          loggerType.GetMethod("Write", [| logEventLevelType; typedefof<string>; typedefof<obj[]> |])

        let messageParam = Expression.Parameter(typedefof<string>)
        let propertyValuesParam = Expression.Parameter(typedefof<obj[]>)

        let writeMethodExp =
          Expression.Call(instanceCast, writeMethodInfo, levelCast, messageParam, propertyValuesParam)

        let expression =
          Expression.Lambda<Action<obj, obj, string, obj[]>>(
            writeMethodExp,
            instanceParam,
            levelParam,
            messageParam,
            propertyValuesParam
          )

        let write = expression.Compile()

        let writeExceptionMethodInfo =
          loggerType.GetMethod("Write", [| logEventLevelType; typedefof<exn>; typedefof<string>; typedefof<obj[]> |])

        let exceptionParam = Expression.Parameter(typedefof<exn>)

        let writeMethodExp =
          Expression.Call(
            instanceCast,
            writeExceptionMethodInfo,
            levelCast,
            exceptionParam,
            messageParam,
            propertyValuesParam
          )

        let writeException =
          Expression
            .Lambda<Action<obj, obj, exn, string, obj[]>>(
              writeMethodExp,
              instanceParam,
              levelParam,
              exceptionParam,
              messageParam,
              propertyValuesParam
            )
            .Compile()

        { Write =
            (fun logger level message formattedParmeters -> write.Invoke(logger, level, message, formattedParmeters))
          WriteException =
            fun logger level ex message formattedParmeters ->
              writeException.Invoke(logger, level, ex, message, formattedParmeters)
          IsEnabled = fun logger level -> isEnabled.Invoke(logger, level)
          TranslateLevel = translateLevel }

    type private SerigLogProvider() =
      let getLoggerByName = getForContextMethodCall ()
      let pushProperty = getPushProperty ()
      let serilogGatewayInit = lazy (SerilogGateway.Create())

      let writeMessage logger logLevel (messageFunc: MessageThunk) ``exception`` formatParams =
        let serilogGateway = serilogGatewayInit.Value
        let translatedValue = serilogGateway.TranslateLevel logLevel

        match messageFunc with
        | None -> serilogGateway.IsEnabled logger translatedValue
        | Some _ when serilogGateway.IsEnabled logger translatedValue |> not -> false
        | Some m ->
          match ``exception`` with
          | Some ex -> serilogGateway.WriteException logger translatedValue ex (m ()) formatParams
          | None -> serilogGateway.Write logger translatedValue (m ()) formatParams

          true

      interface ILogProvider with
        member this.GetLogger(name: string) : Logger = getLoggerByName name |> writeMessage

        member this.OpenMappedContext (key: string) (value: obj) (destructure: bool) : IDisposable =
          pushProperty key value destructure

        member this.OpenNestedContext(message: string) : IDisposable = pushProperty "NDC" message false

    let create () = SerigLogProvider() :> ILogProvider

module LogProvider =
  open System
  open Types
  open Providers
  open System.Diagnostics
  open Microsoft.FSharp.Quotations.Patterns

  let mutable private currentLogProvider = None

  let private knownProviders = [ (SerilogProvider.isAvailable, SerilogProvider.create) ]

  /// Greedy search for first available LogProvider. Order of known providers matters.
  let private resolvedLogger =
    lazy
      (knownProviders
       |> Seq.tryFind (fun (isAvailable, _) -> isAvailable ())
       |> Option.map (fun (_, create) -> create ()))

  let private noopLogger _ _ _ _ = false

  let private noopDisposable =
    { new IDisposable with
        member __.Dispose() = () }

  /// **Description**
  ///
  /// Allows custom override when `getLogger` searches for a LogProvider.
  ///
  /// **Parameters**
  ///   * `provider` - parameter of type `ILogProvider`
  ///
  /// **Output Type**
  ///   * `unit`
  let setLoggerProvider (logProvider: ILogProvider) = currentLogProvider <- Some logProvider

  let getCurrentLogProvider () =
    match currentLogProvider with
    | None -> resolvedLogger.Value
    | Some p -> Some p

  /// **Description**
  ///
  /// Opens a mapped diagnostic context.  This will allow you to set additional parameters to a log given a scope.
  ///
  /// **Parameters**
  ///   * `key` - parameter of type `string` - The name of the property.
  ///   * `value` - parameter of type `obj` - The value of the property.
  ///   * `destructureObjects` - parameter of type `bool` - If true, and the value is a non-primitive, non-array type, then the value will be converted to a structure; otherwise, unknown types will be converted to scalars, which are generally stored as strings. WARNING: Destructring can be expensive.
  ///
  /// **Output Type**
  ///   * `IDisposable`
  let openMappedContextDestucturable (key: string) (value: obj) (destructureObjects: bool) =
    let provider = getCurrentLogProvider ()

    match provider with
    | Some p -> p.OpenMappedContext key value destructureObjects
    | None -> noopDisposable

  /// **Description**
  ///
  /// Opens a mapped diagnostic context.  This will allow you to set additional parameters to a log given a scope. Sets destructureObjects to false.
  ///
  /// **Parameters**
  ///   * `key` - parameter of type `string` - The name of the property.
  ///   * `value` - parameter of type `obj` - The value of the property.
  ///
  /// **Output Type**
  ///   * `IDisposable`
  let openMappedContext (key: string) (value: obj) =
    //TODO: We should try to find out if the value is a primitive
    openMappedContextDestucturable key value false

  /// **Description**
  ///
  /// Opens a nested diagnostic context.  This will allow you to set additional parameters to a log given a scope.
  ///
  /// **Parameters**
  ///   * `value` - parameter of type `string` - The value of the property.
  ///
  /// **Output Type**
  ///   * `IDisposable`
  let openNestedContext (value: string) =
    let provider = getCurrentLogProvider ()

    match provider with
    | Some p -> p.OpenNestedContext value
    | None -> noopDisposable

  /// **Description**
  ///
  /// Creates a logger given a `string`.  This will attempt to retrieve any loggers set with `setLoggerProvider`.  It will fallback to a known list of providers.
  ///
  /// **Parameters**
  ///   * `string` - parameter of type `string`
  ///
  /// **Output Type**
  ///   * `ILog`
  let getLoggerByName (name: string) =
    let loggerProvider = getCurrentLogProvider ()

    let logFunc =
      match loggerProvider with
      | Some loggerProvider -> loggerProvider.GetLogger(name)
      | None -> noopLogger

    { new ILog with
        member x.Log = logFunc
        member x.MappedContext = openMappedContextDestucturable }

  /// **Description**
  ///
  /// Creates a logger given a `Type`.  This will attempt to retrieve any loggers set with `setLoggerProvider`.  It will fallback to a known list of providers.
  ///
  /// **Parameters**
  ///   * `type` - parameter of type `Type`
  ///
  /// **Output Type**
  ///   * `ILog`
  let getLoggerByType (``type``: Type) = ``type`` |> string |> getLoggerByName

  /// **Description**
  ///
  /// Creates a logger given a `'a` type. This will attempt to retrieve any loggers set with `setLoggerProvider`.  It will fallback to a known list of providers.
  ///
  /// **Output Type**
  ///   * `ILog`
  ///
  let getLoggerFor<'a> () = getLoggerByType (typeof<'a>)

  let rec getModuleType =
    function
    | PropertyGet(_, propertyInfo, _) -> propertyInfo.DeclaringType
    // | Call (_, methInfo, _) -> sprintf "%s.%s" methInfo.DeclaringType.FullName methInfo.Name
    // | Lambda(_, expr) -> getModuleType expr
    // | ValueWithName(_,_,instance) -> instance
    | x -> failwithf "Expression is not a property. %A" x

  /// **Description**
  ///
  /// Creates a logger given a Quotations.Expr type. This is only useful for module level declarations. It uses the DeclaringType on the PropertyInfo of the PropertyGet.
  ///
  /// It can be utilized like:
  ///
  /// `let rec logger = LogProvider.getLoggerByQuotation <@ logger @>`
  ///
  /// inside a module to get the modules full qualitfied name.
  ///
  /// **Parameters**
  ///   * `quotation` - parameter of type `Quotations.Expr`
  ///
  /// **Output Type**
  ///   * `ILog`
  ///
  /// **Exceptions**
  ///
  let getLoggerByQuotation (quotation: Quotations.Expr) = getModuleType quotation |> getLoggerByType



  /// **Description**
  ///
  /// Creates a logger based on `Reflection.MethodBase.GetCurrentMethod` call.  This is only useful for calls within functions.  This does not protect against inlined functions.
  ///
  /// **Output Type**
  ///   * `ILog`
  ///
  /// **Exceptions**
  ///
  let inline getLoggerByFunc () =
    let mi = Reflection.MethodBase.GetCurrentMethod()

    sprintf "%s.%s" mi.DeclaringType.FullName mi.Name
    |> getLoggerByName


  /// **Description**
  ///
  /// Creates a logger. It's name is based on the current StackFrame. This will attempt to retrieve any loggers set with `setLoggerProvider`.  It will fallback to a known list of providers.
  /// Obsolete: getCurrentLogger is obsolete, choose another provider factory function.
  ///
  /// **Output Type**
  ///   * `ILog`
  [<Obsolete("getCurrentLogger is obsolete, choose another provider factory function")>]
  let getCurrentLogger () =
    let stackFrame = StackFrame(2, false)
    getLoggerByType (stackFrame.GetMethod().DeclaringType)