module Marksman.Check

open System
open System.IO

open Marksman.Config
open Marksman.Paths
open Marksman.Cst
open Marksman.Doc
open Marksman.Folder
open Marksman.Diag

[<RequireQualifiedAccess>]
type OutputFormat =
    | Text
    | Json

[<RequireQualifiedAccess>]
type Severity =
    | Error
    | Warning

    override this.ToString() =
        match this with
        | Error -> "error"
        | Warning -> "warning"

type DiagLine = {
    file: string
    line: int
    col: int
    severity: Severity
    code: string
    message: string
}

let private severityOfEntry (el: Element) (entry: Entry) : Severity =
    match entry with
    | NonBreakableWhitespace _ -> Severity.Warning
    | BrokenLink _
    | AmbiguousLink _ ->
        match el with
        | WL _ -> Severity.Error
        | _ -> Severity.Warning

let private messageOfEntry (entry: Entry) : string =
    match entry with
    | BrokenLink(_, ref) -> $"broken link to {refToHuman ref}"
    | AmbiguousLink(_, ref, _) -> $"ambiguous link to {refToHuman ref}"
    | NonBreakableWhitespace _ ->
        "non-breaking whitespace in heading (line won't be interpreted as a heading)"

let private elementOfEntry (entry: Entry) : option<Element> =
    match entry with
    | BrokenLink(el, _)
    | AmbiguousLink(el, _, _) -> Some el
    | NonBreakableWhitespace _ -> None

let private rangeOfEntry (entry: Entry) =
    match entry with
    | BrokenLink(el, _)
    | AmbiguousLink(el, _, _) -> Element.range el
    | NonBreakableWhitespace range -> range

let private toDiagLine (relFile: string) (entry: Entry) : DiagLine =
    let range = rangeOfEntry entry
    let line = range.Start.Line + 1
    let col = range.Start.Character + 1
    let el = elementOfEntry entry

    let severity =
        match el with
        | Some el -> severityOfEntry el entry
        | None -> Severity.Warning

    {
        file = relFile
        line = line
        col = col
        severity = severity
        code = $"MKS{code entry |> int:D3}"
        message = messageOfEntry entry
    }

let private printText (d: DiagLine) =
    printfn $"{d.file}:{d.line}:{d.col}: {d.severity}: {d.message} [{d.code}]"

let private escapeJson (s: string) =
    s
        .Replace("\\", "\\\\")
        .Replace("\"", "\\\"")
        .Replace("\n", "\\n")
        .Replace("\r", "\\r")

let private printJson (diags: DiagLine list) =
    let fields (d: DiagLine) =
        $"""  {{"file":"{escapeJson d.file}","line":{d.line},"col":{d.col},"severity":"{d.severity}","code":"{d.code}","message":"{escapeJson d.message}"}}"""

    printfn "["
    let lines = diags |> List.map fields
    printfn "%s" (String.concat ",\n" lines)
    printfn "]"

let private normalizePath (path: string) =
    if Path.IsPathRooted(path) then path else Path.GetFullPath(path)

let private findNearestAncestor (startDir: string) (pred: string -> bool) : option<string> =
    let rec loop (dir: string) =
        if pred dir then
            Some dir
        else
            let parent = Directory.GetParent(dir)

            if isNull parent then None else loop parent.FullName

    loop startDir

let private inferRootForFile (filePath: string) : string =
    let parentDir = Path.GetDirectoryName(filePath)

    let byMarksmanToml =
        findNearestAncestor parentDir (fun dir -> File.Exists(Path.Combine(dir, ".marksman.toml")))

    let byGit =
        findNearestAncestor parentDir (fun dir ->
            let gitPath = Path.Combine(dir, ".git")
            Directory.Exists(gitPath) || File.Exists(gitPath))

    byMarksmanToml
    |> Option.orElse byGit
    |> Option.defaultValue parentDir

let private pathEquals (left: string) (right: string) : bool =
    let comparison =
        if OperatingSystem.IsWindows() then
            StringComparison.OrdinalIgnoreCase
        else
            StringComparison.Ordinal

    String.Equals(left, right, comparison)

let private isUnderRoot (rootPath: string) (filePath: string) : bool =
    let rel = Path.GetRelativePath(rootPath, filePath)
    not (Path.IsPathRooted(rel) || rel.StartsWith(".."))

let check (targetPath: string) (rootOverride: option<string>) (format: OutputFormat) : int =
    let absTarget = normalizePath targetPath
    let targetIsFile = File.Exists(absTarget)
    let targetIsDir = Directory.Exists(absTarget)

    if not targetIsFile && not targetIsDir then
        eprintfn $"marksman check: path does not exist: {absTarget}"
        2
    else
        let rootPath =
            match rootOverride with
            | Some root -> normalizePath root
            | None ->
                if targetIsFile then inferRootForFile absTarget else absTarget

        if not (Directory.Exists(rootPath)) then
            eprintfn $"marksman check: root path does not exist or is not a directory: {rootPath}"
            2
        else if targetIsFile && not (isUnderRoot rootPath absTarget) then
            eprintfn $"marksman check: file is not under workspace root: file={absTarget}; root={rootPath}"
            2
        else
            let absPath = AbsPath.ofSystem rootPath
            let folderUri = AbsPath.toUri absPath
            let folderId = UriWith.mkRoot folderUri
            let folderName = Path.GetFileName(rootPath)
            let userConfig = Config.read Config.userConfigFile

            match Folder.tryLoad userConfig folderName folderId with
            | None ->
                // No markdown files found — that's fine, not an error
                match format with
                | OutputFormat.Json -> printfn "[]"
                | OutputFormat.Text -> ()

                0
            | Some folder ->
                let mutable allDiags: DiagLine list = []
                let targetFile = if targetIsFile then Some absTarget else None

                for docId, entries in Diag.checkFolder folder do
                    let doc = Folder.findDocById docId folder

                    let includeDoc =
                        match targetFile with
                        | None -> true
                        | Some target ->
                            let docPath = Doc.path doc |> AbsPath.toSystem |> normalizePath
                            pathEquals docPath target

                    if includeDoc then
                        let relFile = Doc.pathFromRoot doc |> RelPath.toSystem

                        for entry in entries do
                            allDiags <- toDiagLine relFile entry :: allDiags

                let allDiags = List.rev allDiags

                match format with
                | OutputFormat.Text ->
                    for d in allDiags do
                        printText d
                | OutputFormat.Json -> printJson allDiags

                if List.isEmpty allDiags then 0 else 1
