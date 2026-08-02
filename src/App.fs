module App

open System
open System.Text
open Browser.Dom
open Browser.Types
open Fable.Core
open Types
open CodeAnalysis
open GraphEditor

type GraphLevel =
    | FilesLevel
    | FunctionsLevel

type ViewTransition = {
    FromState: GraphState
    ToState: GraphState
    FocusX: float
    FocusY: float
    FocusRadius: float
    Entering: bool
    StartedAt: float
}

let mutable state = emptyState 800.0 600.0
let mutable sourceFiles: SourceFile list = []
let mutable level = FilesLevel
let mutable selectedFilePath: string option = None
let mutable viewTransition: ViewTransition option = None
let mutable animationTime = 0.0
let transitionDuration = 300.0

let private toolbar = document.createElement ("header")
let private status = document.createElement ("span") :?> HTMLSpanElement
let private inspector = document.createElement ("aside")
let private canvas = document.createElement ("canvas") :?> HTMLCanvasElement

let private setStyles (element: #Element) styles = element.setAttribute ("style", styles)

let private createButton label active onClick =
    let button = document.createElement ("button") :?> HTMLButtonElement
    button.innerText <- label
    let background, foreground, border =
        if active then "#1e293b", "#f8fafc", "#5eead4" else "rgba(15, 23, 42, 0.72)", "#cbd5e1", "rgba(148, 163, 184, 0.30)"
    setStyles button (sprintf "height:34px;padding:0 12px;margin:0 5px 0 0;cursor:pointer;border:1px solid %s;border-radius:8px;background:%s;color:%s;font:600 12px sans-serif;transition:background .15s ease;" border background foreground)
    button.onclick <- fun _ -> onClick ()
    button

let private createLabel text styles =
    let element = document.createElement ("span")
    element.innerText <- text
    setStyles element styles
    element

let private escapeHtml (text: string) =
    text.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("\"", "&quot;")

let private fsharpKeywords =
    set [
        "abstract"; "and"; "as"; "assert"; "async"; "begin"; "class"; "default"; "delegate"
        "do"; "done"; "downcast"; "elif"; "else"; "end"; "exception"; "extern"; "false"
        "finally"; "fixed"; "for"; "fun"; "function"; "if"; "in"; "inherit"; "inline"
        "interface"; "internal"; "lazy"; "let"; "match"; "member"; "module"; "mutable"
        "namespace"; "new"; "null"; "of"; "open"; "or"; "override"; "private"; "public"
        "rec"; "return"; "select"; "static"; "struct"; "then"; "this"; "throw"; "to"
        "true"; "try"; "type"; "upcast"; "use"; "val"; "void"; "when"; "while"; "with"
        "yield"
    ]

let private isIdentifierStart character =
    Char.IsLetter character || character = '_' || character = '\''

let private isIdentifierPart character =
    Char.IsLetterOrDigit character || character = '_' || character = '\''

let private appendSpan (builder: StringBuilder) className (text: string) =
    if not (String.IsNullOrEmpty text) then
        let color =
            match className with
            | "syn-keyword" -> "#5eead4"
            | "syn-string" -> "#fbbf24"
            | "syn-comment" -> "#64748b"
            | "syn-number" -> "#fb923c"
            | "syn-type" -> "#c4b5fd"
            | _ -> "#c4b5fd"
        builder.Append("<span class=\"").Append(className).Append("\" style=\"color:").Append(color).Append(";\">").Append(escapeHtml text).Append("</span>") |> ignore

let private highlightFsharp (source: string) =
    let builder = StringBuilder()
    let mutable index = 0
    let mutable inBlockComment = false
    while index < source.Length do
        if inBlockComment then
            let ending = source.IndexOf("*)", index, StringComparison.Ordinal)
            if ending < 0 then
                appendSpan builder "syn-comment" (source.Substring(index))
                index <- source.Length
            else
                appendSpan builder "syn-comment" (source.Substring(index, ending + 2 - index))
                index <- ending + 2
                inBlockComment <- false
        elif index + 1 < source.Length && source.Substring(index, 2) = "(*" then
            inBlockComment <- true
            appendSpan builder "syn-comment" "(*"
            index <- index + 2
        elif source.[index] = '/' && index + 1 < source.Length && source.[index + 1] = '/' then
            let ending = source.IndexOf('\n', index)
            let length = if ending < 0 then source.Length - index else ending - index
            appendSpan builder "syn-comment" (source.Substring(index, length))
            index <- index + length
        elif source.[index] = '"' then
            let start = index
            index <- index + 1
            let mutable escaped = false
            while index < source.Length && (escaped || source.[index] <> '"') do
                if escaped then escaped <- false
                elif source.[index] = '\\' then escaped <- true
                index <- index + 1
            if index < source.Length then index <- index + 1
            appendSpan builder "syn-string" (source.Substring(start, index - start))
        elif Char.IsDigit source.[index] then
            let start = index
            while index < source.Length && (Char.IsDigit source.[index] || source.[index] = '.' || source.[index] = '_') do
                index <- index + 1
            appendSpan builder "syn-number" (source.Substring(start, index - start))
        elif isIdentifierStart source.[index] then
            let start = index
            index <- index + 1
            while index < source.Length && isIdentifierPart source.[index] do index <- index + 1
            let token = source.Substring(start, index - start)
            if Set.contains token fsharpKeywords then appendSpan builder "syn-keyword" token
            elif Char.IsUpper token.[0] then appendSpan builder "syn-type" token
            else builder.Append(escapeHtml token) |> ignore
        else
            builder.Append(escapeHtml (string source.[index])) |> ignore
            index <- index + 1
    builder.ToString()

let private beginViewTransition fromState toState focusX focusY focusRadius entering =
    viewTransition <-
        Some {
            FromState = fromState
            ToState = toState
            FocusX = focusX
            FocusY = focusY
            FocusRadius = focusRadius
            Entering = entering
            StartedAt = animationTime
        }

let private easeInOut t =
    let t = max 0.0 (min 1.0 t)
    if t < 0.5 then 4.0 * t * t * t else 1.0 - Math.Pow (-2.0 * t + 2.0, 3.0) / 2.0

let private projectLabel () =
    match sourceFiles with
    | [] -> "No project loaded"
    | first :: _ ->
        let parts = first.Path.Replace("\\", "/").Split('/')
        if parts.Length > 1 then parts.[0] else "F# project"

let private graphStats () =
    let nodeLabel = if level = FilesLevel then "files" else "functions"
    sprintf "%d %s  /  %d edges" state.Nodes.Count nodeLabel state.Edges.Count

let private functionScope () =
    match selectedFilePath with
    | Some path -> sprintf "Opened from %s" path
    | None -> "All loaded functions"

[<Emit("(function (url) { var request = new XMLHttpRequest(); request.open('GET', url, false); request.send(null); if (request.status >= 200 && request.status < 300) return request.responseText; throw new Error('HTTP ' + request.status); })($0)")>]
let private fetchTextSync (url: string) : string = jsNative

[<Emit("JSON.parse($0)")>]
let private parseJson (json: string) : obj = jsNative

[<Emit("$0.length")>]
let private jsonLength (value: obj) : int = jsNative

[<Emit("$0[$1].Path")>]
let private jsonSourcePath (value: obj) (index: int) : string = jsNative

[<Emit("$0[$1].Text")>]
let private jsonSourceText (value: obj) (index: int) : string = jsNative

let private updateInspector () =
    inspector.innerHTML <- ""
    let selected = state.SelectedNodes |> Set.toList
    if selected.Length = 1 then
        let nodeId = List.head selected
        match Map.tryFind nodeId state.Nodes with
        | Some node ->
            inspector.appendChild (createLabel (if node.Kind = SourceFileNode then "SOURCE FILE" else "FUNCTION") "display:block;color:#5eead4;font:700 10px sans-serif;letter-spacing:1.4px;margin-bottom:9px;") |> ignore
            inspector.appendChild (createLabel node.Name "display:block;color:#f8fafc;font:600 18px sans-serif;margin-bottom:6px;overflow-wrap:anywhere;") |> ignore
            inspector.appendChild (createLabel node.Detail "display:block;color:#94a3b8;font:12px sans-serif;line-height:1.5;margin-bottom:9px;overflow-wrap:anywhere;") |> ignore
            if node.FilePath <> "" then
                inspector.appendChild (createLabel node.FilePath "display:block;color:#64748b;font:11px monospace;line-height:1.4;overflow-wrap:anywhere;") |> ignore
            match node.Line with
            | Some line -> inspector.appendChild (createLabel (sprintf "line %d" line) "display:block;color:#64748b;font:11px monospace;margin-top:5px;") |> ignore
            | None -> ()
            match node.SourceCode with
            | Some source when node.Kind = FunctionNode ->
                inspector.appendChild (createLabel "SOURCE" "display:block;color:#5eead4;font:700 10px sans-serif;letter-spacing:1.4px;margin-top:18px;margin-bottom:8px;") |> ignore
                let code = document.createElement ("pre")
                code.innerHTML <- highlightFsharp source
                setStyles code "box-sizing:border-box;margin:0;max-height:360px;overflow:auto;padding:12px;border:1px solid rgba(148,163,184,.16);border-radius:8px;background:rgba(2,6,23,.72);color:#c4b5fd;font:11px/1.5 ui-monospace,SFMono-Regular,Menlo,monospace;white-space:pre;"
                code.setAttribute ("style", "box-sizing:border-box;margin:0;max-height:360px;overflow:auto;padding:12px;border:1px solid rgba(148,163,184,.16);border-radius:8px;background:rgba(2,6,23,.72);color:#c4b5fd;font:11px/1.5 ui-monospace,SFMono-Regular,Menlo,monospace;white-space:pre;--syn-keyword:#5eead4;--syn-string:#fbbf24;--syn-comment:#64748b;--syn-number:#fb923c;--syn-type:#c4b5fd;")
                inspector.appendChild code |> ignore
            | _ -> ()
        | None -> ()
    elif not (List.isEmpty selected) then
        inspector.appendChild (createLabel (sprintf "%d nodes selected" selected.Length) "display:block;color:#f8fafc;font:600 16px sans-serif;margin-bottom:7px;") |> ignore
        inspector.appendChild (createLabel "Drag to move the selection. Double-click a file to inspect its functions." "display:block;color:#94a3b8;font:12px sans-serif;line-height:1.5;") |> ignore
    else
        inspector.appendChild (createLabel "GRAPH LEGEND" "display:block;color:#5eead4;font:700 10px sans-serif;letter-spacing:1.4px;margin-bottom:10px;") |> ignore
        inspector.appendChild (createLabel "Dashed teal edges" "display:block;color:#cbd5e1;font:600 12px sans-serif;margin-bottom:3px;") |> ignore
        inspector.appendChild (createLabel "file references" "display:block;color:#64748b;font:11px sans-serif;margin-bottom:9px;") |> ignore
        inspector.appendChild (createLabel "Solid violet edges" "display:block;color:#cbd5e1;font:600 12px sans-serif;margin-bottom:3px;") |> ignore
        inspector.appendChild (createLabel "function calls" "display:block;color:#64748b;font:11px sans-serif;margin-bottom:9px;") |> ignore
        inspector.appendChild (createLabel "Drag nodes to tune the layout. Double-click a file node to descend one layer." "display:block;color:#94a3b8;font:12px sans-serif;line-height:1.5;") |> ignore

let rec updateToolbar () =
    toolbar.innerHTML <- ""
    setStyles toolbar "position:relative;z-index:3;box-sizing:border-box;min-height:72px;padding:14px 20px;border-bottom:1px solid rgba(148,163,184,.16);background:rgba(8,17,31,.94);display:flex;align-items:center;gap:10px;font-family:sans-serif;"

    let titleGroup = document.createElement ("div")
    setStyles titleGroup "display:flex;align-items:baseline;gap:9px;margin-right:auto;min-width:245px;"
    titleGroup.appendChild (createLabel "Lamplight" "color:#f8fafc;font:700 20px Georgia,serif;letter-spacing:-.4px;") |> ignore
    titleGroup.appendChild (createLabel "F# code atlas" "color:#64748b;font:11px sans-serif;letter-spacing:1px;text-transform:uppercase;") |> ignore
    toolbar.appendChild titleGroup |> ignore

    if not (List.isEmpty sourceFiles) then
        let backButton = createButton "Back" false (fun () ->
            if level = FunctionsLevel then
                let oldState = state
                let nodes, edges = buildFileGraph sourceFiles state.CanvasWidth state.CanvasHeight
                let nextState = stateFromGraph state.CanvasWidth state.CanvasHeight nodes edges
                beginViewTransition oldState nextState (state.CanvasWidth / 2.0) (state.CanvasHeight / 2.0) 54.0 false
                state <- nextState
                level <- FilesLevel
                selectedFilePath <- None
                updateToolbar ()
                updateInspector ())
        if level = FilesLevel then
            backButton.setAttribute ("disabled", "true")
            setStyles backButton "height:34px;padding:0 12px;margin:0 5px 0 0;cursor:default;border:1px solid rgba(148,163,184,.14);border-radius:8px;background:rgba(15,23,42,.45);color:#475569;font:600 12px sans-serif;"
        toolbar.appendChild backButton |> ignore

        let currentButton = createButton (if level = FilesLevel then "Files" else "Functions") (level = FilesLevel) (fun () -> ())
        toolbar.appendChild currentButton |> ignore
        let pauseButton = createButton (if state.PhysicsPaused then "Resume layout" else "Pause layout") false (fun () -> state <- togglePhysics state; updateToolbar ())
        toolbar.appendChild pauseButton |> ignore

    let project = createLabel (sprintf "%s  ·  %s" (projectLabel ()) (graphStats ())) "color:#64748b;font:11px monospace;white-space:nowrap;margin-left:8px;"
    toolbar.appendChild project |> ignore

    status.innerText <-
        if List.isEmpty sourceFiles then "Run: npm run run -- /path/to/your/fsharp-project"
        elif level = FilesLevel then "Double-click a file node to descend into its functions."
        else sprintf "Function graph across the loaded project. %s. Use Back to return to files." (functionScope ())
    setStyles status "position:absolute;left:20px;top:76px;z-index:2;color:#64748b;font:11px sans-serif;pointer-events:none;"
    updateInspector ()

let private loadSources (sources: (string * string) list) =
    let analyzed = analyzeFiles sources
    if List.isEmpty analyzed then
        sourceFiles <- []
        state <- emptyState state.CanvasWidth state.CanvasHeight
        level <- FilesLevel
        selectedFilePath <- None
    else
        sourceFiles <- analyzed
        let nodes, edges = buildFileGraph analyzed state.CanvasWidth state.CanvasHeight
        state <- stateFromGraph state.CanvasWidth state.CanvasHeight nodes edges
        level <- FilesLevel
        selectedFilePath <- None
    viewTransition <- None
    updateToolbar ()

let private loadAnalysis () =
    status.innerText <- "Loading CLI analysis..."
    try
        let parsed = fetchTextSync "analysis.json" |> parseJson
        let count = jsonLength parsed
        let sources =
            [ for index in 0 .. count - 1 do
                yield jsonSourcePath parsed index, jsonSourceText parsed index ]
        if List.isEmpty sources then
            status.innerText <- "CLI analysis contained no F# files."
        else
            loadSources sources
    with ex ->
        status.innerText <- sprintf "No CLI analysis found: %s" ex.Message

let private enterFunctionLayer nodeId =
    match level, Map.tryFind nodeId state.Nodes with
    | FilesLevel, Some fileNode when fileNode.Kind = SourceFileNode ->
        let oldState = state
        let nodes, edges = buildFunctionGraph sourceFiles state.CanvasWidth state.CanvasHeight
        let nextState = stateFromGraph state.CanvasWidth state.CanvasHeight nodes edges
        beginViewTransition oldState nextState fileNode.X fileNode.Y fileNode.Radius true
        state <- nextState
        level <- FunctionsLevel
        selectedFilePath <- Some fileNode.FilePath
        updateToolbar ()
    | _ -> ()

let private renderCurrentView (ctx: CanvasRenderingContext2D) =
    match viewTransition with
    | None -> render ctx state
    | Some transition ->
        let progress = (animationTime - transition.StartedAt) / transitionDuration
        if progress >= 1.0 then
            viewTransition <- None
            render ctx state
        else
            let eased = easeInOut progress
            let fromScale, toScale =
                if transition.Entering then 1.0 + eased * 0.08, 0.94 + eased * 0.06
                else 1.0 - eased * 0.06, 1.08 - eased * 0.08
            renderTransition ctx transition.FromState transition.ToState (1.0 - eased) fromScale eased toScale transition.FocusX transition.FocusY transition.FocusRadius eased

let private applyResize () =
    let width = max 320.0 window.innerWidth
    let height = max 320.0 (window.innerHeight - 72.0)
    canvas.width <- int width
    canvas.height <- int height
    state <- resize width height state
    viewTransition <- None

let private getMousePos (ev: MouseEvent) =
    let rect = canvas.getBoundingClientRect ()
    ev.clientX - rect.left, ev.clientY - rect.top

let private init () =
    setStyles document.body "margin:0;overflow:hidden;background:#08111f;color:#e2e8f0;"
    setStyles toolbar "position:relative;z-index:3;"
    document.body.appendChild toolbar |> ignore
    document.body.appendChild status |> ignore

    setStyles canvas "display:block;width:100%;background:#08111f;"
    document.body.appendChild canvas |> ignore
    setStyles inspector "position:absolute;right:22px;top:92px;bottom:22px;z-index:2;box-sizing:border-box;width:340px;min-height:130px;max-height:calc(100vh - 114px);overflow:auto;padding:16px;border:1px solid rgba(148,163,184,.18);border-radius:12px;background:rgba(15,23,42,.90);backdrop-filter:blur(12px);box-shadow:0 14px 40px rgba(0,0,0,.24);"
    document.body.appendChild inspector |> ignore

    let ctx = canvas.getContext_2d ()
    canvas.onmousemove <- fun ev ->
        let x, y = getMousePos ev
        state <- handleMouseMove x y state
    canvas.onmousedown <- fun ev ->
        let x, y = getMousePos ev
        state <- handleMouseDown x y ev.shiftKey ev.ctrlKey state
        updateInspector ()
    canvas.onmouseup <- fun ev ->
        let x, y = getMousePos ev
        state <- handleMouseUp x y state
        updateInspector ()
    canvas.onmouseleave <- fun _ -> state <- { state with Hovered = None; Drag = NoDrag }
    canvas.ondblclick <- fun ev ->
        let x, y = getMousePos ev
        match hitTestNode x y state with
        | Some nodeId -> enterFunctionLayer nodeId
        | None -> ()
    document.onkeydown <- fun ev ->
        if ev.key = "Escape" && level = FunctionsLevel then
            let oldState = state
            let nodes, edges = buildFileGraph sourceFiles state.CanvasWidth state.CanvasHeight
            state <- stateFromGraph state.CanvasWidth state.CanvasHeight nodes edges
            level <- FilesLevel
            selectedFilePath <- None
            beginViewTransition oldState state (state.CanvasWidth / 2.0) (state.CanvasHeight / 2.0) 54.0 false
            updateToolbar ()
    window.onresize <- fun _ -> applyResize ()

    applyResize ()
    updateToolbar ()
    loadAnalysis ()
    let mutable previousFrameTime: float option = None
    let rec loop timestamp =
        let delta =
            match previousFrameTime with
            | Some previous -> max 0.0 (min 100.0 (timestamp - previous))
            | None -> 0.0
        previousFrameTime <- Some timestamp
        animationTime <- animationTime + delta
        state <- physicsStep state
        renderCurrentView ctx
        window.requestAnimationFrame loop |> ignore
    window.requestAnimationFrame loop |> ignore

init ()
