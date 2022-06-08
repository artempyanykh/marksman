#r "nuget: Fake.Core.Process"

// Run a linter and clean up its output from "Linting: [fname] ... Finished: 0 warnings"

open System
open Fake.Core

let junkMarker = "Finished: 0 warnings"

let rec zipWithNext: 'a list -> ('a * 'a option) list =
    function
    | [] -> []
    | [ only ] -> [ only, None ]
    | fst :: (snd :: _ as tail) -> (fst, Some snd) :: zipWithNext tail

// Run linter and capture its output
// Would be cool to also preserve colored output, but I don't know how to pretend that the output is a TTY/PTY
// However, the primary use-case is CI, therefore lack of color is not a big issue
let lint =
    CreateProcess.fromRawCommand "dotnet" [ "fsharplint"; "lint"; "Marksman/Marksman.fsproj" ]
    |> CreateProcess.redirectOutput
    |> Proc.run

if lint.ExitCode <> 0 then
    let lines =
        lint.Result.Output.Split([| "\n"; "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

    zipWithNext lines
    |> List.iter (function
        | cur, None -> printfn $"{cur}"
        | cur, Some next when not (cur.Contains(junkMarker) || next.Contains(junkMarker)) ->
            printfn $"{cur}"
        | _ -> ())

Environment.Exit(lint.ExitCode)
