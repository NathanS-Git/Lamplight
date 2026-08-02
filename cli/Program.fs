module LamplightCli

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Text
open System.Text.Json
open CodeAnalysis

type SourceInput = {
    Path: string
    Text: string
}

let private usage () =
    printfn "Usage: npm run run -- /path/to/fsharp-project"
    printfn "       dotnet run --project cli/Lamplight.Cli.fsproj -- /path/to/fsharp-project"

let private normalizePath (path: string) =
    path.Replace(Path.DirectorySeparatorChar, '/').Replace(Path.AltDirectorySeparatorChar, '/')

let private ignoredDirectoryNames = set [ "obj"; "bin"; "node_modules"; "fable_modules" ]

let private isIgnoredPath (path: string) =
    path.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.exists (fun part -> Set.contains (part.ToLowerInvariant()) ignoredDirectoryNames)

let private readSources root =
    [ ".fs"; ".fsx" ]
    |> List.collect (fun extension ->
        Directory.EnumerateFiles(root, "*" + extension, SearchOption.AllDirectories)
        |> Seq.filter (isIgnoredPath >> not)
        |> Seq.map (fun path ->
            let relative = Path.GetRelativePath(root, path) |> normalizePath
            relative, File.ReadAllText(path))
        |> Seq.toList)
    |> List.sortBy fst

let private findPublicDirectory () =
    let rec search directory =
        let candidate = Path.Combine(directory, "public")
        if Directory.Exists candidate then
            candidate
        else
            let parent = Directory.GetParent directory
            if isNull parent then
                failwith "Could not locate the project's public directory. Run this command from the Lamplight project."
            else
                search parent.FullName
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

let private serve (publicDirectory: string) (analysisJson: string) =
    let rec startListener port =
        let candidate = new HttpListener()
        candidate.Prefixes.Add(sprintf "http://localhost:%d/" port)
        try
            candidate.Start()
            candidate, port
        with
        | :? HttpListenerException when port < 8090 ->
            candidate.Close()
            startListener (port + 1)

    let listener, port = startListener 8080

    let publicDirectory = Path.GetFullPath(publicDirectory).TrimEnd(Path.DirectorySeparatorChar) + string Path.DirectorySeparatorChar
    let analysisBytes = Encoding.UTF8.GetBytes(analysisJson)
    let indexPath = Path.Combine(publicDirectory, "index.html")
    let mutable running = true

    Console.CancelKeyPress.Add(fun args ->
        args.Cancel <- true
        running <- false
        listener.Stop())

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
                let relativePath =
                    if String.IsNullOrWhiteSpace requestPath || requestPath = "/" then "index.html"
                    else requestPath.TrimStart('/').Replace('/', Path.DirectorySeparatorChar)
                let candidate = Path.GetFullPath(Path.Combine(publicDirectory, relativePath))
                if not (candidate.StartsWith(publicDirectory, StringComparison.OrdinalIgnoreCase)) then
                    writeResponse context.Response 403 "text/plain; charset=utf-8" (Encoding.UTF8.GetBytes("Forbidden"))
                elif File.Exists candidate then
                    writeResponse context.Response 200 (contentType candidate) (File.ReadAllBytes candidate)
                else
                    writeResponse context.Response 404 "text/plain; charset=utf-8" (Encoding.UTF8.GetBytes("Not found"))
        with
        | :? HttpListenerException -> running <- false
        | ex ->
            eprintfn "Request failed: %s" ex.Message

    if listener.IsListening then listener.Stop()

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        usage ()
        1
    else
        try
            let root = Path.GetFullPath argv.[0]
            if not (Directory.Exists root) then
                failwithf "Source directory does not exist: %s" root

            let sources = readSources root
            let analyzed = analyzeFiles sources
            if List.isEmpty analyzed then
                failwith "No .fs or .fsx files were found in the selected directory."

            let inputs = analyzed |> List.map (fun source -> { Path = source.Path; Text = source.Text })
            let options = JsonSerializerOptions(WriteIndented = false)
            let analysisJson = JsonSerializer.Serialize(inputs, options)
            let publicDirectory = findPublicDirectory ()

            printfn "Analyzed %d F# files, %d functions." analyzed.Length (analyzed |> List.sumBy (fun file -> file.Functions.Length))
            serve publicDirectory analysisJson
            0
        with ex ->
            eprintfn "%s" ex.Message
            1
