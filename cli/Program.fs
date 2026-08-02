module LamplightCli

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open CodeAnalysis
open Types

type ProjectRead = {
    Info: ProjectInfo
    Sources: SourceInput list
}

type AnalysisPayload = {
    Projects: ProjectInfo list
    Sources: SourceInput list
}

let private usage () =
    printfn "Usage: npm run run -- /path/to/project-a [/path/to/project-b ...]"
    printfn "       Each path may be an F# project directory or an .fsproj file."

let private normalizePath (path: string) =
    path.Replace(Path.DirectorySeparatorChar, '/').Replace(Path.AltDirectorySeparatorChar, '/')

let private ignoredDirectoryNames = set [ "obj"; "bin"; "node_modules"; "fable_modules" ]

let private isIgnoredPath (path: string) =
    path.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.exists (fun part -> Set.contains (part.ToLowerInvariant()) ignoredDirectoryNames)

let private projectReferencePattern = Regex(@"<ProjectReference\s+Include=[""']([^""']+)[""']", RegexOptions.IgnoreCase)

let private projectName (root: string) (projectFile: string option) =
    match projectFile with
    | Some file -> Path.GetFileNameWithoutExtension file
    | None -> DirectoryInfo(root).Name

let private projectFileFor root =
    if File.Exists root && Path.GetExtension(root).Equals(".fsproj", StringComparison.OrdinalIgnoreCase) then
        Some (Path.GetFullPath root)
    elif Directory.Exists root then
        Directory.EnumerateFiles(root, "*.fsproj", SearchOption.TopDirectoryOnly) |> Seq.tryHead
    else None

let private readSources root projectPath =
    [ ".fs"; ".fsx" ]
    |> List.collect (fun extension ->
        Directory.EnumerateFiles(root, "*" + extension, SearchOption.AllDirectories)
        |> Seq.filter (isIgnoredPath >> not)
        |> Seq.map (fun path ->
            let relative = Path.GetRelativePath(root, path) |> normalizePath
            { Project = projectPath; Path = relative; Text = File.ReadAllText(path) })
        |> Seq.toList)
    |> List.sortBy (fun source -> source.Path)

let private readProject path =
    let fullPath = Path.GetFullPath path
    let root, projectFile =
        if File.Exists fullPath && Path.GetExtension(fullPath).Equals(".fsproj", StringComparison.OrdinalIgnoreCase) then
            Path.GetDirectoryName fullPath, Some fullPath
        elif Directory.Exists fullPath then
            fullPath, projectFileFor fullPath
        else
            failwithf "Project path does not exist: %s" path
    let name = projectName root projectFile
    let projectId = root |> Path.GetFullPath |> normalizePath
    let references =
        match projectFile with
        | Some file ->
            projectReferencePattern.Matches(File.ReadAllText file)
            |> Seq.cast<Match>
            |> Seq.map (fun m -> Path.GetFileNameWithoutExtension m.Groups.[1].Value)
            |> Seq.toList
        | None -> []
    // Use the normalized root path as the stable identity in both project
    // metadata and source inputs. This keeps drill-down portable across OSes.
    let info = { Name = name; Path = projectId; References = references }
    { Info = info; Sources = readSources root projectId }

let private findPublicDirectory () =
    let rec search directory =
        let candidate = Path.Combine(directory, "public")
        if Directory.Exists candidate then candidate
        else
            let parent = Directory.GetParent directory
            if isNull parent then failwith "Could not locate the project's public directory. Run this command from the Lamplight project."
            else search parent.FullName
    search (Directory.GetCurrentDirectory())

let private contentType (path: string) =
    match Path.GetExtension(path).ToLowerInvariant() with
    | ".html" -> "text/html; charset=utf-8"
    | ".js" -> "text/javascript; charset=utf-8"
    | ".css" -> "text/css; charset=utf-8"
    | ".ico" -> "image/x-icon"
    | _ -> "application/octet-stream"

let private writeResponse (response: HttpListenerResponse) status contentTypeValue (bytes: byte array) =
    response.StatusCode <- status
    response.ContentType <- contentTypeValue
    response.ContentLength64 <- int64 bytes.Length
    response.OutputStream.Write(bytes, 0, bytes.Length)
    response.Close()

let private serve publicDirectory analysisJson =
    let rec startListener port =
        let candidate = new HttpListener()
        candidate.Prefixes.Add(sprintf "http://localhost:%d/" port)
        try candidate.Start(); candidate, port
        with
        | :? HttpListenerException when port < 8090 -> candidate.Close(); startListener (port + 1)
    let listener, port = startListener 8080
    let publicDirectory = Path.GetFullPath(publicDirectory).TrimEnd(Path.DirectorySeparatorChar) + string Path.DirectorySeparatorChar
    let analysisBytes = Encoding.UTF8.GetBytes(analysisJson: string)
    let mutable running = true
    Console.CancelKeyPress.Add(fun args -> args.Cancel <- true; running <- false; listener.Stop())
    let baseUrl = sprintf "http://localhost:%d" port
    printfn "Lamplight is running at %s" baseUrl
    printfn "Press Ctrl+C to stop."
    try
        let browserInfo = ProcessStartInfo("xdg-open", baseUrl)
        browserInfo.UseShellExecute <- false
        Process.Start(browserInfo) |> ignore
    with _ -> ()
    while running do
        try
            let context = listener.GetContext()
            let requestPath = Uri.UnescapeDataString(context.Request.Url.AbsolutePath)
            if requestPath = "/analysis.json" then
                writeResponse context.Response 200 "application/json; charset=utf-8" analysisBytes
            else
                let relativePath = if String.IsNullOrWhiteSpace requestPath || requestPath = "/" then "index.html" else requestPath.TrimStart('/').Replace('/', Path.DirectorySeparatorChar)
                let candidate = Path.GetFullPath(Path.Combine(publicDirectory, relativePath))
                if not (candidate.StartsWith(publicDirectory, StringComparison.OrdinalIgnoreCase)) then writeResponse context.Response 403 "text/plain; charset=utf-8" (Encoding.UTF8.GetBytes "Forbidden")
                elif File.Exists candidate then writeResponse context.Response 200 (contentType candidate) (File.ReadAllBytes candidate)
                else writeResponse context.Response 404 "text/plain; charset=utf-8" (Encoding.UTF8.GetBytes "Not found")
        with
        | :? HttpListenerException -> running <- false
        | ex -> eprintfn "Request failed: %s" ex.Message
    if listener.IsListening then listener.Stop()

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then usage (); 1
    else
        try
            let projects = argv |> Array.toList |> List.map readProject
            let projectNames = projects |> List.map (fun project -> project.Info.Name) |> Set.ofList
            let infos = projects |> List.map (fun project -> { project.Info with References = project.Info.References |> List.filter (fun name -> Set.contains name projectNames) })
            let inputs = projects |> List.collect (fun project -> project.Sources)
            let analyzed = analyzeSourceInputs inputs
            if List.isEmpty analyzed then failwith "No .fs or .fsx files were found in the selected project paths."
            let payload = { Projects = infos; Sources = inputs }
            let analysisJson = JsonSerializer.Serialize(payload, JsonSerializerOptions(WriteIndented = false))
            let publicDirectory = findPublicDirectory ()
            printfn "Analyzed %d projects, %d F# files, %d functions." infos.Length analyzed.Length (analyzed |> List.sumBy (fun file -> file.Functions.Length))
            serve publicDirectory analysisJson
            0
        with ex -> eprintfn "%s" ex.Message; 1