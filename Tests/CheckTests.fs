module Marksman.CheckTests

open System
open System.IO
open Xunit

open Marksman.Check

let private withTempDir f =
    let dir = Path.Combine(Path.GetTempPath(), "marksman-check-tests", Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(dir) |> ignore

    try
        f dir
    finally
        if Directory.Exists(dir) then
            Directory.Delete(dir, true)

let private writeFile (path: string) (content: string) =
    let parent = Path.GetDirectoryName(path)

    if not (String.IsNullOrEmpty(parent)) then
        Directory.CreateDirectory(parent) |> ignore

    File.WriteAllText(path, content)

[<Fact>]
let check_fileTarget_usesInferredWorkspaceRoot () =
    withTempDir (fun root ->
        writeFile (Path.Combine(root, ".marksman.toml")) ""
        writeFile (Path.Combine(root, "doc-in-root.md")) "# Root doc"

        let notePath = Path.Combine(root, "notes", "note.md")
        writeFile notePath "[[doc-in-root]]"

        let exitCode = Check.check notePath None OutputFormat.Json
        Assert.Equal(0, exitCode))

[<Fact>]
let check_fileTarget_scopesDiagnosticsToThatFile () =
    withTempDir (fun root ->
        writeFile (Path.Combine(root, ".marksman.toml")) ""
        writeFile (Path.Combine(root, "doc-in-root.md")) "# Root doc"
        writeFile (Path.Combine(root, "other.md")) "[[missing-doc]]"

        let notePath = Path.Combine(root, "notes", "note.md")
        writeFile notePath "[[doc-in-root]]"

        let exitCode = Check.check notePath None OutputFormat.Json
        Assert.Equal(0, exitCode))

[<Fact>]
let check_fileTarget_fallsBackToParentWithoutWorkspaceMarkers () =
    withTempDir (fun root ->
        writeFile (Path.Combine(root, "doc-in-root.md")) "# Root doc"

        let notePath = Path.Combine(root, "notes", "note.md")
        writeFile notePath "[[doc-in-root]]"

        let exitCode = Check.check notePath None OutputFormat.Json
        Assert.Equal(1, exitCode))

[<Fact>]
let check_rootOverride_takesPrecedenceForFileTarget () =
    withTempDir (fun root ->
        writeFile (Path.Combine(root, ".marksman.toml")) ""
        writeFile (Path.Combine(root, "doc-in-root.md")) "# Root doc"

        let notesRoot = Path.Combine(root, "notes")
        let notePath = Path.Combine(notesRoot, "note.md")
        writeFile notePath "[[doc-in-root]]"

        let exitCode = Check.check notePath (Some notesRoot) OutputFormat.Json
        Assert.Equal(1, exitCode))
