module CodeAnalysis

open System
open System.Text.RegularExpressions
open Types

let private modulePattern = Regex(@"^\s*(?:module|namespace)\s+([A-Za-z_][A-Za-z0-9_'.]*)", RegexOptions.Multiline)
let private openPattern = Regex(@"^\s*open\s+([A-Za-z_][A-Za-z0-9_'.]*)", RegexOptions.Multiline)
let private declarationPattern = Regex(@"^\s*(?:let\s+(?:(?:private|public|internal|inline|mutable)\s+)*(?:rec\s+)?|and\s+|(?:(?:static)\s+)?member\s+(?:(?:private|public|internal|inline)\s+)*)([A-Za-z_][A-Za-z0-9_']*)", RegexOptions.Compiled)
let private tokenPattern = Regex(@"[A-Za-z_][A-Za-z0-9_']*", RegexOptions.Compiled)

let private lastPathPart (path: string) =
    path.Replace("\\", "/").Split('/')
    |> Array.tryLast
    |> Option.defaultValue path

let private fileStem (path: string) =
    let name = lastPathPart path
    let dot = name.LastIndexOf('.')
    if dot > 0 then name.Substring(0, dot) else name

let private indentOf (line: string) =
    line |> Seq.takeWhile ((=) ' ') |> Seq.length

let private declarationAt lineIndex line =
    let matchValue = declarationPattern.Match line
    if matchValue.Success then
        Some (lineIndex, indentOf line, matchValue.Groups.[1].Value)
    else None

let private extractFunctions (path: string) (moduleName: string) (text: string) =
    let lines = text.Replace("\r\n", "\n").Split('\n') |> Array.toList
    let declarations =
        lines
        |> List.mapi declarationAt
        |> List.choose id
    let topLevelIndent =
        if List.isEmpty declarations then 0
        else declarations |> List.map (fun (_, indent, _) -> indent) |> List.min
    let declarations = declarations |> List.filter (fun (_, indent, _) -> indent = topLevelIndent)

    declarations
    |> List.mapi (fun index (lineIndex, indent, name) ->
        let endIndex =
            declarations
            |> List.skip (index + 1)
            |> List.tryFind (fun (nextLine, nextIndent, _) -> nextLine > lineIndex && nextIndent <= indent)
            |> Option.map (fun (nextLine, _, _) -> nextLine)
            |> Option.defaultValue lines.Length
        let body = lines |> List.skip lineIndex |> List.take (max 1 (endIndex - lineIndex)) |> String.concat "\n"
        { Name = name
          QualifiedName = moduleName + "." + name
          FilePath = path
          Line = lineIndex + 1
          Body = body
          Calls = [] })

let private moduleName (path: string) (text: string) =
    let matchValue = modulePattern.Match text
    if matchValue.Success then matchValue.Groups.[1].Value else fileStem path

let private withCalls (allFunctions: CodeFunction list) (fn: CodeFunction) =
    let names =
        allFunctions
        |> List.filter (fun target -> target.Name <> fn.Name)
        |> List.map (fun target -> target.Name)
        |> Set.ofList
    let tokens = tokenPattern.Matches fn.Body |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Set.ofSeq
    { fn with Calls = Set.intersect names tokens |> Set.toList }

let analyzeFile (path: string) (text: string) : SourceFile =
    let moduleName = moduleName path text
    { Path = path
      Name = fileStem path
      ModuleName = moduleName
      Text = text
      Functions = extractFunctions path moduleName text }

let analyzeFiles (sources: (string * string) list) : SourceFile list =
    let files =
        sources
        |> List.filter (fun (path, _) ->
            let normalized = path.Replace("\\", "/")
            let lower = normalized.ToLowerInvariant()
            (lower.EndsWith(".fs") || lower.EndsWith(".fsx")) &&
            not (lower.Contains("/obj/")) &&
            not (lower.Contains("/bin/")) &&
            not (lower.Contains("/node_modules/")) &&
            not (lower.Contains("/fable_modules/")))
        |> List.sortBy fst
        |> List.map (fun (path, text) -> analyzeFile path text)

    let allFunctions = files |> List.collect (fun file -> file.Functions)
    let files =
        files
        |> List.map (fun file -> { file with Functions = file.Functions |> List.map (withCalls allFunctions) })

    files

let private containsSymbol (text: string) symbol =
    Regex.IsMatch(text, "(?<![A-Za-z0-9_'])" + Regex.Escape symbol + "(?![A-Za-z0-9_'])")

let private fileReferences (source: SourceFile) (target: SourceFile) =
    let targetModuleShort = target.ModuleName.Split('.') |> Array.last
    let opens =
        openPattern.Matches source.Text
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> Set.ofSeq
    let opened = Set.contains target.ModuleName opens || Set.contains targetModuleShort opens
    let moduleMentioned = containsSymbol source.Text targetModuleShort || containsSymbol source.Text target.ModuleName
    let functionMentioned = target.Functions |> List.exists (fun fn -> containsSymbol source.Text fn.Name)
    opened || moduleMentioned || functionMentioned

let private initialPosition index count width height =
    let angle = float index * Math.PI * 2.0 / float (max 1 count) - Math.PI / 2.0
    let radius = min (width * 0.33) (height * 0.33) |> max 150.0
    width / 2.0 + Math.Cos angle * radius,
    height / 2.0 + Math.Sin angle * radius

let private createNode (kind: NodeKind) id name detail path line sourceCode index count width height : CodeNode =
    let x, y = initialPosition index count width height
    { Id = id
      Kind = kind
      Name = name
      Detail = detail
      FilePath = path
      Line = line
      SourceCode = sourceCode
      X = x
      Y = y
      Vx = 0.0
      Vy = 0.0
      Radius = if kind = SourceFileNode then 54.0 else 34.0
      Fixed = false }

let buildFileGraph (files: SourceFile list) width height =
    let count = List.length files
    let nodes =
        files
        |> List.mapi (fun index file ->
            let node = createNode SourceFileNode index file.Name (sprintf "%d functions" file.Functions.Length) file.Path None None index count width height
            index, node)
        |> Map.ofList
    let edges =
        [ for sourceIndex in 0 .. count - 1 do
            for targetIndex in 0 .. count - 1 do
                if sourceIndex <> targetIndex && fileReferences files.[sourceIndex] files.[targetIndex] then
                    let source = files.[sourceIndex]
                    let target = files.[targetIndex]
                    yield sourceIndex, targetIndex, sprintf "%s references %s" source.Name target.Name ]
        |> List.mapi (fun edgeId (source, target, label) ->
            edgeId, { Id = edgeId; Kind = FileReference; Source = source; Target = target; Label = label })
        |> Map.ofList
    nodes, edges

let buildFunctionGraph (files: SourceFile list) width height =
    let functions = files |> List.collect (fun file -> file.Functions)
    let count = List.length functions
    let nodes =
        functions
        |> List.mapi (fun index fn ->
            let detail = sprintf "%s  ·  line %d" (lastPathPart fn.FilePath) fn.Line
            let node = createNode FunctionNode index fn.Name detail fn.FilePath (Some fn.Line) (Some fn.Body) index count width height
            index, node)
        |> Map.ofList
    let idsByName =
        functions
        |> List.mapi (fun index fn -> fn.Name, index)
        |> Map.ofList
    let edges =
        functions
        |> List.mapi (fun sourceIndex fn -> sourceIndex, fn)
        |> List.collect (fun (sourceIndex, fn) ->
            fn.Calls
            |> List.choose (fun name ->
                match Map.tryFind name idsByName with
                | Some targetIndex when sourceIndex <> targetIndex -> Some (sourceIndex, targetIndex, sprintf "%s calls %s" fn.Name name)
                | _ -> None))
        |> List.distinct
        |> List.mapi (fun edgeId (source, target, label) ->
            edgeId, { Id = edgeId; Kind = FunctionCall; Source = source; Target = target; Label = label })
        |> Map.ofList
    nodes, edges
