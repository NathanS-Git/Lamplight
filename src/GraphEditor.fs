module GraphEditor

open System
open Browser.Types
open Types

let inline color (value: string) = Fable.Core.U3<_, _, _>.Case1 value
let private background = "#08111f"
let private gridColor = "rgba(148, 163, 184, 0.10)"
let private fileAccent = "#2dd4bf"
let private functionAccent = "#a78bfa"
let private textColor = "#e2e8f0"
let private mutedText = "#94a3b8"

let private nodeWidth (node: CodeNode) =
    match node.Kind with
    | ProjectNode -> 190.0
    | SourceFileNode -> 168.0
    | FunctionNode -> max 112.0 (min 190.0 (float node.Name.Length * 8.0 + 38.0))

let private nodeHeight (node: CodeNode) =
    match node.Kind with
    | ProjectNode -> 78.0
    | SourceFileNode -> 70.0
    | FunctionNode -> 54.0

let selectedNodeId (state: GraphState) =
    state.SelectedNodes |> Set.toList |> List.tryExactlyOne

let private nodeHit (node: CodeNode) x y =
    let halfWidth = nodeWidth node / 2.0 + 8.0
    let halfHeight = nodeHeight node / 2.0 + 8.0
    abs (x - node.X) <= halfWidth && abs (y - node.Y) <= halfHeight

let private edgePoint (node: CodeNode) otherX otherY =
    let dx = otherX - node.X
    let dy = otherY - node.Y
    let distance = sqrt (dx * dx + dy * dy)
    if distance < 0.001 then
        node.X + nodeWidth node / 2.0, node.Y
    else
        let normalizedX = dx / distance
        let normalizedY = dy / distance
        let halfWidth = nodeWidth node / 2.0
        let halfHeight = nodeHeight node / 2.0
        let scaleX = if abs normalizedX < 0.0001 then Double.PositiveInfinity else halfWidth / abs normalizedX
        let scaleY = if abs normalizedY < 0.0001 then Double.PositiveInfinity else halfHeight / abs normalizedY
        let scale = min scaleX scaleY
        node.X + normalizedX * scale, node.Y + normalizedY * scale

let emptyState canvasWidth canvasHeight = {
    Nodes = Map.empty
    Edges = Map.empty
    NextNodeId = 0
    NextEdgeId = 0
    SelectedNodes = Set.empty
    SelectedEdges = Set.empty
    Hovered = None
    Drag = NoDrag
    PhysicsPaused = false
    CanvasWidth = canvasWidth
    CanvasHeight = canvasHeight
    MouseX = 0.0
    MouseY = 0.0
    Zoom = 1.0
    PanX = 0.0
    PanY = 0.0
    Layout = ForceLayout
}

let stateFromGraph width height (nodes: Map<NodeId, CodeNode>) (edges: Map<EdgeId, CodeEdge>) =
    { emptyState width height with
        Nodes = nodes
        Edges = edges
        NextNodeId = if Map.isEmpty nodes then 0 else (nodes |> Map.toSeq |> Seq.map fst |> Seq.max) + 1
        NextEdgeId = if Map.isEmpty edges then 0 else (edges |> Map.toSeq |> Seq.map fst |> Seq.max) + 1 }

let distPointToSegment px py x1 y1 x2 y2 =
    let dx = x2 - x1
    let dy = y2 - y1
    if dx = 0.0 && dy = 0.0 then
        sqrt ((px - x1) ** 2.0 + (py - y1) ** 2.0)
    else
        let lengthSquared = dx * dx + dy * dy
        let t = max 0.0 (min 1.0 ((px - x1) * dx + (py - y1) * dy) / lengthSquared)
        let projectedX = x1 + t * dx
        let projectedY = y1 + t * dy
        sqrt ((px - projectedX) ** 2.0 + (py - projectedY) ** 2.0)

let hitTestNode x y (state: GraphState) =
    state.Nodes
    |> Map.toSeq
    |> Seq.sortByDescending (fun (_, node) ->
        match node.Kind with
        | FunctionNode -> 2
        | SourceFileNode -> 1
        | ProjectNode -> 0)
    |> Seq.tryPick (fun (_, node) -> if nodeHit node x y then Some node.Id else None)

let screenToWorld x y (state: GraphState) =
    let centerX = state.CanvasWidth / 2.0
    let centerY = state.CanvasHeight / 2.0
    centerX + (x - centerX - state.PanX) / state.Zoom,
    centerY + (y - centerY - state.PanY) / state.Zoom

let zoomBy amount state =
    { state with Zoom = max 0.35 (min 2.5 (state.Zoom * amount)) }

let panBy dx dy state =
    { state with PanX = state.PanX + dx; PanY = state.PanY + dy }

let recenter state =
    { state with Zoom = 1.0; PanX = 0.0; PanY = 0.0 }

let private arrange preset (state: GraphState) =
    let nodes = state.Nodes |> Map.toList |> List.map snd
    let count = nodes.Length
    if count = 0 then state
    else
        let centerX = state.CanvasWidth / 2.0
        let centerY = state.CanvasHeight / 2.0
        let positions =
            match preset with
            | RadialLayout ->
                let radius = min (state.CanvasWidth * 0.36) (state.CanvasHeight * 0.36) |> max 170.0
                nodes
                |> List.mapi (fun index node ->
                    let angle = float index * Math.PI * 2.0 / float count - Math.PI / 2.0
                    node.Id, (centerX + Math.Cos angle * radius, centerY + Math.Sin angle * radius))
            | GridLayout ->
                let columns = max 1 (ceil (sqrt (float count)) |> int)
                let spacingX = max 190.0 (state.CanvasWidth / float (columns + 1))
                let rows = (count + columns - 1) / columns
                let spacingY = max 100.0 (state.CanvasHeight / float (rows + 1))
                nodes
                |> List.mapi (fun index node ->
                    let column = index % columns
                    let row = index / columns
                    node.Id, (spacingX * float (column + 1), spacingY * float (row + 1)))
            | ForceLayout -> []
        let arranged =
            positions
            |> Map.ofList
            |> fun positions ->
                state.Nodes
                |> Map.map (fun id node ->
                    match Map.tryFind id positions with
                    | Some (x, y) -> { node with X = x; Y = y; Vx = 0.0; Vy = 0.0 }
                    | None -> { node with Vx = 0.0; Vy = 0.0 })
        { state with Nodes = arranged; Layout = preset; PhysicsPaused = preset <> ForceLayout }

let setLayout preset state =
    arrange preset state

let hitTestEdge x y (state: GraphState) =
    state.Edges
    |> Map.tryPick (fun _ edge ->
        match Map.tryFind edge.Source state.Nodes, Map.tryFind edge.Target state.Nodes with
        | Some source, Some target ->
            let sx, sy = edgePoint source target.X target.Y
            let tx, ty = edgePoint target source.X source.Y
            if distPointToSegment x y sx sy tx ty <= 8.0 then Some edge.Id else None
        | _ -> None)

let private getHit x y state =
    match hitTestNode x y state with
    | Some id -> Some (Choice1Of2 id)
    | None -> hitTestEdge x y state |> Option.map Choice2Of2

let private nodesInRect x1 y1 x2 y2 (state: GraphState) =
    let left, right = min x1 x2, max x1 x2
    let top, bottom = min y1 y2, max y1 y2
    state.Nodes
    |> Map.toSeq
    |> Seq.choose (fun (_, node) ->
        if node.X >= left && node.X <= right && node.Y >= top && node.Y <= bottom then Some node.Id else None)
    |> Set.ofSeq

let physicsStep (state: GraphState) =
    if state.PhysicsPaused then state
    else
        let pinned =
            match state.Drag with
            | DragNode _
            | DragSelection _ -> state.SelectedNodes
            | SelectBox _
            | PanCanvas _
            | NoDrag -> Set.empty
        { state with Nodes = Physics.applyForces state pinned }

let resize width height state = { state with CanvasWidth = width; CanvasHeight = height }

let togglePhysics state = { state with PhysicsPaused = not state.PhysicsPaused }

let private roundedRect (ctx: CanvasRenderingContext2D) x y width height radius =
    let r = min radius (min (width / 2.0) (height / 2.0))
    ctx.beginPath ()
    ctx.moveTo (x + r, y)
    ctx.lineTo (x + width - r, y)
    ctx.quadraticCurveTo (x + width, y, x + width, y + r)
    ctx.lineTo (x + width, y + height - r)
    ctx.quadraticCurveTo (x + width, y + height, x + width - r, y + height)
    ctx.lineTo (x + r, y + height)
    ctx.quadraticCurveTo (x, y + height, x, y + height - r)
    ctx.lineTo (x, y + r)
    ctx.quadraticCurveTo (x, y, x + r, y)
    ctx.closePath ()

let private truncate maxLength (value: string) =
    if value.Length <= maxLength then value else value.Substring(0, maxLength - 1) + "..."

let private drawGrid (ctx: CanvasRenderingContext2D) width height =
    ctx.fillStyle <- color background
    ctx.fillRect (0.0, 0.0, width, height)
    ctx.save ()
    ctx.strokeStyle <- color gridColor
    ctx.lineWidth <- 1.0
    let step = 44.0
    let mutable x = 0.0
    while x <= width do
        ctx.beginPath ()
        ctx.moveTo (x, 0.0)
        ctx.lineTo (x, height)
        ctx.stroke ()
        x <- x + step
    let mutable y = 0.0
    while y <= height do
        ctx.beginPath ()
        ctx.moveTo (0.0, y)
        ctx.lineTo (width, y)
        ctx.stroke ()
        y <- y + step
    ctx.restore ()

let private drawArrowHead (ctx: CanvasRenderingContext2D) x1 y1 x2 y2 stroke =
    let angle = atan2 (y2 - y1) (x2 - x1)
    let size = 8.0
    ctx.save ()
    ctx.translate (x2, y2)
    ctx.rotate angle
    ctx.fillStyle <- color stroke
    ctx.beginPath ()
    ctx.moveTo (0.0, 0.0)
    ctx.lineTo (-size, -size / 2.0)
    ctx.lineTo (-size, size / 2.0)
    ctx.closePath ()
    ctx.fill ()
    ctx.restore ()

let private drawEdge (ctx: CanvasRenderingContext2D) (state: GraphState) (edge: CodeEdge) =
    match Map.tryFind edge.Source state.Nodes, Map.tryFind edge.Target state.Nodes with
    | Some source, Some target ->
        let selected = Set.contains edge.Id state.SelectedEdges
        let hovered = state.Hovered = Some (Choice2Of2 edge.Id)
        let selectedId = selectedNodeId state
        let incoming = selectedId |> Option.exists (fun id -> edge.Target = id)
        let outgoing = selectedId |> Option.exists (fun id -> edge.Source = id)
        let connected = incoming || outgoing
        let stroke =
            if incoming then "#fb7185"
            elif outgoing then "#60a5fa"
            else
                match edge.Kind with
                | ProjectReference -> "rgba(245, 158, 11, 0.52)"
                | FileReference -> if selected || hovered then "#5eead4" else "rgba(45, 212, 191, 0.48)"
                | FunctionCall -> if selected || hovered then "#c4b5fd" else "rgba(167, 139, 250, 0.52)"
        let sx, sy = edgePoint source target.X target.Y
        let tx, ty = edgePoint target source.X source.Y
        ctx.save ()
        ctx.strokeStyle <- color stroke
        ctx.lineWidth <- if selected || hovered || connected then 3.0 else 1.8
        if edge.Kind = FileReference || edge.Kind = ProjectReference then ctx.setLineDash [| 7.0; 5.0 |]
        ctx.beginPath ()
        ctx.moveTo (sx, sy)
        ctx.lineTo (tx, ty)
        ctx.stroke ()
        drawArrowHead ctx sx sy tx ty stroke
        if selected || hovered then
            let labelX = (sx + tx) / 2.0
            let labelY = (sy + ty) / 2.0
            ctx.font <- "11px sans-serif"
            ctx.textAlign <- "center"
            ctx.textBaseline <- "middle"
            let labelWidth = float edge.Label.Length * 5.5 + 14.0
            ctx.fillStyle <- color "rgba(8, 17, 31, 0.92)"
            roundedRect ctx (labelX - labelWidth / 2.0) (labelY - 10.0) labelWidth 20.0 8.0
            ctx.fill ()
            ctx.fillStyle <- color textColor
            ctx.fillText (truncate 34 edge.Label, labelX, labelY)
        ctx.restore ()
    | _ -> ()

let private connectionSets (state: GraphState) =
    match selectedNodeId state with
    | None -> Set.empty, Set.empty
    | Some selected ->
        let incoming = state.Edges |> Map.toSeq |> Seq.choose (fun (_, edge) -> if edge.Target = selected then Some edge.Source else None) |> Set.ofSeq
        let outgoing = state.Edges |> Map.toSeq |> Seq.choose (fun (_, edge) -> if edge.Source = selected then Some edge.Target else None) |> Set.ofSeq
        incoming, outgoing

let private drawNode (ctx: CanvasRenderingContext2D) (state: GraphState) (node: CodeNode) =
    let selected = Set.contains node.Id state.SelectedNodes
    let hovered = state.Hovered = Some (Choice1Of2 node.Id)
    let incoming, outgoing = connectionSets state
    let isIncoming = Set.contains node.Id incoming
    let isOutgoing = Set.contains node.Id outgoing
    let dimmed = not (Set.isEmpty state.SelectedNodes) && not selected && not isIncoming && not isOutgoing
    let width = nodeWidth node
    let height = nodeHeight node
    let x = node.X - width / 2.0
    let y = node.Y - height / 2.0
    let accent, fill =
        match node.Kind with
        | ProjectNode -> "#f59e0b", "#3b2810"
        | SourceFileNode -> fileAccent, "#102b32"
        | FunctionNode -> functionAccent, "#211c3b"
    ctx.save ()
    if dimmed then ctx.globalAlpha <- 0.22
    elif isIncoming then ctx.globalAlpha <- 0.86
    elif isOutgoing then ctx.globalAlpha <- 0.94
    if selected || hovered then
        ctx.shadowColor <- if selected then "rgba(94, 234, 212, 0.45)" else "rgba(167, 139, 250, 0.38)"
        ctx.shadowBlur <- 18.0
    roundedRect ctx x y width height 13.0
    ctx.fillStyle <- color fill
    ctx.fill ()
    ctx.shadowBlur <- 0.0
    ctx.strokeStyle <- color (
        if selected then "#f8fafc"
        elif isIncoming then "#fb7185"
        elif isOutgoing then "#60a5fa"
        elif hovered then accent
        else "rgba(148, 163, 184, 0.48)")
    ctx.lineWidth <- if selected || isIncoming || isOutgoing then 2.5 else 1.5
    ctx.stroke ()

    ctx.fillStyle <- color accent
    ctx.beginPath ()
    ctx.arc (x + 17.0, node.Y, 4.0, 0.0, Math.PI * 2.0)
    ctx.fill ()
    ctx.fillStyle <- color textColor
    ctx.font <-
        match node.Kind with
        | ProjectNode -> "600 15px sans-serif"
        | SourceFileNode -> "600 14px sans-serif"
        | FunctionNode -> "600 13px sans-serif"
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    let nameLimit, detailLimit =
        match node.Kind with
        | ProjectNode -> 22, 25
        | SourceFileNode -> 20, 25
        | FunctionNode -> 22, 27
    ctx.fillText (truncate nameLimit node.Name, x + 29.0, node.Y - 9.0)
    ctx.fillStyle <- color mutedText
    ctx.font <- "11px sans-serif"
    ctx.fillText (truncate detailLimit node.Detail, x + 29.0, node.Y + 13.0)
    if node.Fixed then
        ctx.fillStyle <- color accent
        ctx.font <- "10px sans-serif"
        ctx.fillText ("PINNED", x + width - 40.0, y + 13.0)
    ctx.restore ()

let private drawSelectionBox (ctx: CanvasRenderingContext2D) state =
    match state.Drag with
    | SelectBox (startX, startY) ->
        let x = min startX state.MouseX
        let y = min startY state.MouseY
        let width = abs (state.MouseX - startX)
        let height = abs (state.MouseY - startY)
        ctx.save ()
        ctx.strokeStyle <- color "#5eead4"
        ctx.fillStyle <- color "rgba(45, 212, 191, 0.10)"
        ctx.setLineDash [| 5.0; 5.0 |]
        ctx.fillRect (x, y, width, height)
        ctx.strokeRect (x, y, width, height)
        ctx.restore ()
    | _ -> ()

let private drawGraph (ctx: CanvasRenderingContext2D) (state: GraphState) =
    state.Edges |> Map.iter (fun _ edge -> drawEdge ctx state edge)
    state.Nodes |> Map.iter (fun _ node -> drawNode ctx state node)
    drawSelectionBox ctx state

let private renderLayer (ctx: CanvasRenderingContext2D) (state: GraphState) opacity scale focusX focusY =
    ctx.save ()
    ctx.globalAlpha <- opacity
    let centerX = state.CanvasWidth / 2.0
    let centerY = state.CanvasHeight / 2.0
    ctx.translate (centerX + state.PanX, centerY + state.PanY)
    ctx.scale (scale * state.Zoom, scale * state.Zoom)
    ctx.translate (-centerX, -centerY)
    ctx.translate (focusX, focusY)
    ctx.scale (1.0, 1.0)
    ctx.translate (-focusX, -focusY)
    drawGraph ctx state
    ctx.restore ()

let render (ctx: CanvasRenderingContext2D) (state: GraphState) =
    ctx.clearRect (0.0, 0.0, state.CanvasWidth, state.CanvasHeight)
    drawGrid ctx state.CanvasWidth state.CanvasHeight
    renderLayer ctx state 1.0 1.0 0.0 0.0

let renderTransition (ctx: CanvasRenderingContext2D) (fromState: GraphState) (toState: GraphState) fromOpacity fromScale toOpacity toScale focusX focusY focusRadius progress =
    ctx.clearRect (0.0, 0.0, toState.CanvasWidth, toState.CanvasHeight)
    drawGrid ctx toState.CanvasWidth toState.CanvasHeight
    renderLayer ctx fromState fromOpacity fromScale focusX focusY
    renderLayer ctx toState toOpacity toScale focusX focusY
    ctx.save ()
    ctx.strokeStyle <- color (sprintf "rgba(94, 234, 212, %f)" (0.30 * (1.0 - progress)))
    ctx.lineWidth <- 2.0
    ctx.beginPath ()
    ctx.arc (focusX, focusY, focusRadius + progress * 18.0, 0.0, Math.PI * 2.0)
    ctx.stroke ()
    ctx.restore ()

let moveSelectedNodes dx dy state =
    let nodes =
        state.Nodes
        |> Map.map (fun id node ->
            if Set.contains id state.SelectedNodes then { node with X = node.X + dx; Y = node.Y + dy } else node)
    { state with Nodes = nodes }

let handleMouseMove x y state =
    let state = { state with MouseX = x; MouseY = y }
    match state.Drag with
    | DragNode (id, offsetX, offsetY, _, _) ->
        match Map.tryFind id state.Nodes with
        | Some node ->
            let newX = x - offsetX
            let newY = y - offsetY
            let dx, dy = newX - node.X, newY - node.Y
            moveSelectedNodes dx dy state
        | None -> state
    | DragSelection (lastX, lastY) ->
        let moved = moveSelectedNodes (x - lastX) (y - lastY) state
        { moved with Drag = DragSelection (x, y) }
    | SelectBox _ -> state
    | PanCanvas (lastX, lastY) ->
        { state with PanX = state.PanX + (x - lastX); PanY = state.PanY + (y - lastY); Drag = PanCanvas (x, y) }
    | NoDrag -> { state with Hovered = getHit x y state }

let handleMiddleMouseDown x y state =
    { state with Drag = PanCanvas (x, y); Hovered = None }

let handleMouseDown x y shift ctrl state =
    match getHit x y state with
    | Some (Choice1Of2 id) ->
        let node = Map.find id state.Nodes
        let selected =
            if shift || ctrl then
                if Set.contains id state.SelectedNodes then Set.remove id state.SelectedNodes
                else Set.add id state.SelectedNodes
            else if Set.contains id state.SelectedNodes then state.SelectedNodes
            else Set.singleton id
        let drag =
            if Set.contains id selected then
                DragNode (id, x - node.X, y - node.Y, x, y)
            else NoDrag
        { state with SelectedNodes = selected; SelectedEdges = Set.empty; Drag = drag }
    | Some (Choice2Of2 id) ->
        let selected = if shift || ctrl then Set.add id state.SelectedEdges else Set.singleton id
        { state with SelectedNodes = Set.empty; SelectedEdges = selected; Drag = NoDrag }
    | None ->
        { state with SelectedNodes = Set.empty; SelectedEdges = Set.empty; Drag = SelectBox (x, y) }

let handleMouseUp x y state =
    match state.Drag with
    | SelectBox (startX, startY) ->
        { state with Drag = NoDrag; SelectedNodes = nodesInRect startX startY x y state; SelectedEdges = Set.empty }
    | DragNode (_, _, _, origX, origY) ->
        { state with Drag = NoDrag }
    | DragSelection _
    | PanCanvas _
    | NoDrag -> { state with Drag = NoDrag }
