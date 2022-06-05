module Marksman.Helpers

open System.Runtime.InteropServices
open Snapper
open Marksman.Misc

let dummyRoot =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        "c:/"
    else
        "/"

let dummyRootPath pathComps = dummyRoot :: pathComps |> String.concat "/"

let pathToUri path = $"file://{path}"

let checkInlineSnapshot (fmt: 'a -> string) (things: seq<'a>) (snapshot: seq<string>) =
    let lines = Seq.map (fun x -> (fmt x).Lines()) things |> Seq.concat

    lines.ShouldMatchInlineSnapshot(snapshot)
