module Physics

open System
open Types

// The same soft force layout works well for both the compact file graph and
// the denser function graph. It is deliberately deterministic apart from
// user movement, which makes a freshly loaded codebase settle predictably.
// Keep cards separated enough that labels remain readable, especially in the
// denser function view. The graph is still intentionally loose rather than
// trying to pack every node into the viewport.
let repulsionStrength = 7600.0
let springLength = 185.0
let springStrength = 0.035

// Function nodes belonging to one source file get a weak long-range cohesion
// force. Repulsion still controls their minimum spacing, while this force
// gently pulls a file's functions into a visible cluster.
let sameFileAttractionDistance = 145.0
let sameFileAttractionStrength = 0.008

// Canvas cards are larger than their center-to-center distance can suggest.
// This is a soft visual repulsion: it uses the rendered card dimensions and
// starts fading in before cards get close, rather than imposing a rigid wall.
// The wider influence radius creates breathing room without making the layout
// feel overly springy or explosive.
let collisionGap = 105.0
let collisionInfluenceScale = 1.35
let collisionStrength = 0.22

let damping = 0.90
let centeringStrength = 0.0025
let maxSpeed = 24.0
let timeStep = 0.5

let private clamp value minValue maxValue =
    max minValue (min maxValue value)

let private distanceAndDir x1 y1 x2 y2 =
    let dx = x2 - x1
    let dy = y2 - y1
    let distance = sqrt (dx * dx + dy * dy)
    if distance < 0.001 then
        0.001, 1.0, 0.0
    else
        distance, dx / distance, dy / distance

let private nodeWidth (node: CodeNode) =
    match node.Kind with
    | SourceFileNode -> 168.0
    | FunctionNode -> max 112.0 (min 190.0 (float node.Name.Length * 8.0 + 38.0))

let private nodeHeight (node: CodeNode) =
    match node.Kind with
    | SourceFileNode -> 70.0
    | FunctionNode -> 54.0

let applyForces (state: GraphState) (pinnedNodeIds: Set<NodeId>) =
    let centerX = state.CanvasWidth / 2.0
    let centerY = state.CanvasHeight / 2.0
    let initialForces = state.Nodes |> Map.map (fun _ _ -> 0.0, 0.0)
    let nodes = state.Nodes |> Map.toList |> List.map snd

    let rec repel pairs forces =
        match pairs with
        | [] -> forces
        | (a, b) :: rest ->
            let distance, dirX, dirY = distanceAndDir a.X a.Y b.X b.Y
            let force = repulsionStrength / (distance * distance + 1.0)
            let fx = dirX * force
            let fy = dirY * force
            let ax, ay = Map.find a.Id forces
            let bx, by = Map.find b.Id forces
            forces
            |> Map.add a.Id (ax - fx, ay - fy)
            |> Map.add b.Id (bx + fx, by + fy)
            |> repel rest

    let pairs =
        [ for i in 0 .. nodes.Length - 1 do
            for j in i + 1 .. nodes.Length - 1 do
                nodes.[i], nodes.[j] ]

    let forces = repel pairs initialForces

    // Push overlapping rendered rectangles apart. Unlike inverse-square
    // point repulsion, this remains strong while cards overlap and therefore
    // establishes a much more reliable visual minimum separation.
    let forces =
        pairs
        |> List.fold (fun forces (a, b) ->
            let distance, dirX, dirY = distanceAndDir a.X a.Y b.X b.Y
            let cardDistance =
                sqrt (
                    ((nodeWidth a + nodeWidth b) / 2.0) ** 2.0 +
                    ((nodeHeight a + nodeHeight b) / 2.0) ** 2.0)
            let influenceDistance = cardDistance + collisionGap
            let softDistance = influenceDistance * collisionInfluenceScale
            let overlap = softDistance - distance
            if overlap <= 0.0 then
                forces
            else
                // Normalize the response at the card boundary. This makes
                // the force smooth over its wider radius instead of creating
                // a hard collision impulse.
                let normalizedOverlap = overlap / max 1.0 (softDistance - cardDistance)
                let force = normalizedOverlap * collisionStrength
                let fx = dirX * force
                let fy = dirY * force
                let ax, ay = Map.find a.Id forces
                let bx, by = Map.find b.Id forces
                forces
                |> Map.add a.Id (ax - fx, ay - fy)
                |> Map.add b.Id (bx + fx, by + fy)) forces

    let forces =
        state.Edges
        |> Map.fold (fun forces _ edge ->
            match Map.tryFind edge.Source state.Nodes, Map.tryFind edge.Target state.Nodes with
            | Some source, Some target ->
                let distance, dirX, dirY = distanceAndDir source.X source.Y target.X target.Y
                let force = springStrength * (distance - springLength)
                let fx = dirX * force
                let fy = dirY * force
                let sx, sy = Map.find source.Id forces
                let tx, ty = Map.find target.Id forces
                forces
                |> Map.add source.Id (sx + fx, sy + fy)
                |> Map.add target.Id (tx - fx, ty - fy)
            | _ -> forces) forces

    // The function graph has no explicit file nodes, so add a subtle grouping
    // force based on each function node's source path. This is deliberately
    // weaker than graph-edge springs: calls can still separate a cluster when
    // the call structure says they should.
    let forces =
        if state.Nodes |> Map.exists (fun _ node -> node.Kind = FunctionNode) then
            let functionPairs =
                [ for i in 0 .. nodes.Length - 1 do
                    for j in i + 1 .. nodes.Length - 1 do
                        if nodes.[i].Kind = FunctionNode &&
                           nodes.[j].Kind = FunctionNode &&
                           nodes.[i].FilePath = nodes.[j].FilePath then
                            nodes.[i], nodes.[j] ]
            functionPairs
            |> List.fold (fun forces (source, target) ->
                let distance, dirX, dirY = distanceAndDir source.X source.Y target.X target.Y
                let force = sameFileAttractionStrength * (distance - sameFileAttractionDistance)
                let fx = dirX * force
                let fy = dirY * force
                let sx, sy = Map.find source.Id forces
                let tx, ty = Map.find target.Id forces
                forces
                |> Map.add source.Id (sx + fx, sy + fy)
                |> Map.add target.Id (tx - fx, ty - fy)) forces
        else
            forces

    let forces =
        state.Nodes
        |> Map.fold (fun forces id node ->
            let fx = (centerX - node.X) * centeringStrength
            let fy = (centerY - node.Y) * centeringStrength
            let currentX, currentY = Map.find id forces
            Map.add id (currentX + fx, currentY + fy) forces) forces

    state.Nodes
    |> Map.map (fun id node ->
        if node.Fixed || Set.contains id pinnedNodeIds then
            { node with Vx = 0.0; Vy = 0.0 }
        else
            let fx, fy = Map.find id forces
            let vx = clamp ((node.Vx + fx * timeStep) * damping) (-maxSpeed) maxSpeed
            let vy = clamp ((node.Vy + fy * timeStep) * damping) (-maxSpeed) maxSpeed
            { node with
                Vx = vx
                Vy = vy
                X = node.X + vx * timeStep
                Y = node.Y + vy * timeStep })
