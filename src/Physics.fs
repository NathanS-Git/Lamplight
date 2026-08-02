module Physics

open System
open Types

// The same soft force layout works well for both the compact file graph and
// the denser function graph. It is deliberately deterministic apart from
// user movement, which makes a freshly loaded codebase settle predictably.
// Keep cards separated enough that labels remain readable, especially in the
// denser function view. The graph is still intentionally loose rather than
// trying to pack every node into the viewport.
let repulsionStrength = 20000.0
let springLength = 200.0
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
let collisionGap = 200.0
let collisionInfluenceScale = 1.35
let collisionStrength = 0.22

let damping = 0.90
let centeringStrength = 0.0025
let maxSpeed = 24.0
let timeStep = 0.5

// Barnes-Hut approximation settings. A cell is treated as one aggregate mass
// when it is sufficiently far away; nearby cells are opened so local spacing
// remains accurate. This changes the expensive all-pairs repulsion from O(n²)
// to approximately O(n log n) for larger function graphs.
let private barnesHutTheta = 0.62
let private quadBucketSize = 4
let private quadMaxDepth = 14

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

type private QuadCell(minX: float, minY: float, maxX: float, maxY: float, depth: int) =
    let mutable points: CodeNode list = []
    let mutable children: QuadCell array option = None
    let mutable mass = 0.0
    let mutable centerX = 0.0
    let mutable centerY = 0.0

    member _.MinX = minX
    member _.MinY = minY
    member _.MaxX = maxX
    member _.MaxY = maxY
    member _.Size = maxX - minX
    member _.Points = points
    member _.Children = children
    member _.Mass = mass
    member _.CenterX = centerX
    member _.CenterY = centerY

    member private this.ChildIndex x y =
        let midX = (minX + maxX) / 2.0
        let midY = (minY + maxY) / 2.0
        if y < midY then
            if x < midX then 0 else 1
        elif x < midX then 2
        else 3

    member private this.Subdivide() =
        let midX = (minX + maxX) / 2.0
        let midY = (minY + maxY) / 2.0
        children <- Some [|
            QuadCell(minX, minY, midX, midY, depth + 1)
            QuadCell(midX, minY, maxX, midY, depth + 1)
            QuadCell(minX, midY, midX, maxY, depth + 1)
            QuadCell(midX, midY, maxX, maxY, depth + 1)
        |]
        let existing = points
        points <- []
        for point in existing do this.Insert point

    member this.Insert (point: CodeNode) =
        match children with
        | Some cells -> cells.[this.ChildIndex point.X point.Y].Insert point
        | None when points.Length < quadBucketSize || depth >= quadMaxDepth ->
            points <- point :: points
        | None ->
            this.Subdivide()
            this.Insert point

    member this.UpdateMass() =
        match children with
        | None ->
            mass <- float points.Length
            if mass > 0.0 then
                centerX <- points |> List.averageBy (fun point -> point.X)
                centerY <- points |> List.averageBy (fun point -> point.Y)
            else
                centerX <- (minX + maxX) / 2.0
                centerY <- (minY + maxY) / 2.0
        | Some cells ->
            for cell in cells do cell.UpdateMass ()
            mass <- cells |> Array.sumBy (fun cell -> cell.Mass)
            if mass > 0.0 then
                centerX <- (cells |> Array.sumBy (fun (cell: QuadCell) -> cell.CenterX * cell.Mass)) / mass
                centerY <- (cells |> Array.sumBy (fun (cell: QuadCell) -> cell.CenterY * cell.Mass)) / mass
            else
                centerX <- (minX + maxX) / 2.0
                centerY <- (minY + maxY) / 2.0

let private buildQuadTree (nodes: CodeNode list) =
    let minX = nodes |> List.map (fun node -> node.X) |> List.min
    let maxX = nodes |> List.map (fun node -> node.X) |> List.max
    let minY = nodes |> List.map (fun node -> node.Y) |> List.min
    let maxY = nodes |> List.map (fun node -> node.Y) |> List.max
    let lower = min minX minY
    let upper = max maxX maxY
    let span = max 1.0 (upper - lower)
    let root = QuadCell(lower, lower, lower + span, lower + span, 0)
    for node in nodes do root.Insert node
    root.UpdateMass ()
    root

let private addForce id fx fy forces =
    let currentX, currentY = Map.find id forces
    Map.add id (currentX + fx, currentY + fy) forces

let private applyQuadRepulsion (root: QuadCell) (source: CodeNode) forces =
    let rec visit (cell: QuadCell) forces =
        if cell.Mass = 0.0 then
            forces
        else
            let distance, dirX, dirY = distanceAndDir source.X source.Y cell.CenterX cell.CenterY
            let containsSource =
                source.X >= cell.MinX && source.X <= cell.MaxX &&
                source.Y >= cell.MinY && source.Y <= cell.MaxY
            match cell.Children with
            | None ->
                cell.Points
                |> List.fold (fun forces target ->
                    if target.Id = source.Id then forces
                    else
                        let distance, dirX, dirY = distanceAndDir source.X source.Y target.X target.Y
                        let force = repulsionStrength / (distance * distance + 1.0)
                        addForce source.Id (-dirX * force) (-dirY * force) forces) forces
            | Some cells when not containsSource && cell.Size / distance < barnesHutTheta ->
                let force = repulsionStrength * cell.Mass / (distance * distance + 1.0)
                addForce source.Id (-dirX * force) (-dirY * force) forces
            | Some cells ->
                cells |> Array.fold (fun forces child -> visit child forces) forces
    visit root forces

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

    // Build the spatial index once per physics tick. The tree is only needed
    // while the layout is moving; GraphEditor avoids calling this function
    // entirely when physics is paused.
    let forces =
        if List.length nodes < 24 then
            repel pairs initialForces
        else
            let tree = buildQuadTree nodes
            nodes |> List.fold (fun forces node -> applyQuadRepulsion tree node forces) initialForces

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
                           nodes.[i].ProjectPath = nodes.[j].ProjectPath &&
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
