module Physics

open System
open Types

// Tuned for a readable layout while keeping the active simulation cheap.
let repulsionStrength = 20000.0
let springLength = 200.0
let springStrength = 0.035
let sameFileAttractionDistance = 145.0
let sameFileAttractionStrength = 0.008
let collisionGap = 200.0
let collisionInfluenceScale = 1.35
let collisionStrength = 0.22
let damping = 0.90
let centeringStrength = 0.0025
let maxSpeed = 24.0
let timeStep = 0.5

// Barnes-Hut settings. Distant cells are represented by their aggregate mass;
// nearby cells are opened so local spacing remains accurate.
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
    let mutable points: int list = []
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

    member private this.Subdivide (nodes: CodeNode array) =
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
        for index in existing do this.Insert index nodes

    member this.Insert index (nodes: CodeNode array) =
        let point = nodes.[index]
        match children with
        | Some cells -> cells.[this.ChildIndex point.X point.Y].Insert index nodes
        | None when points.Length < quadBucketSize || depth >= quadMaxDepth ->
            points <- index :: points
        | None ->
            this.Subdivide nodes
            this.Insert index nodes

    member this.UpdateMass (nodes: CodeNode array) =
        match children with
        | None ->
            mass <- float points.Length
            if mass > 0.0 then
                centerX <- points |> List.averageBy (fun index -> nodes.[index].X)
                centerY <- points |> List.averageBy (fun index -> nodes.[index].Y)
            else
                centerX <- (minX + maxX) / 2.0
                centerY <- (minY + maxY) / 2.0
        | Some cells ->
            for cell in cells do cell.UpdateMass nodes
            mass <- cells |> Array.sumBy (fun (cell: QuadCell) -> cell.Mass)
            if mass > 0.0 then
                centerX <- (cells |> Array.sumBy (fun (cell: QuadCell) -> cell.CenterX * cell.Mass)) / mass
                centerY <- (cells |> Array.sumBy (fun (cell: QuadCell) -> cell.CenterY * cell.Mass)) / mass
            else
                centerX <- (minX + maxX) / 2.0
                centerY <- (minY + maxY) / 2.0

let private buildQuadTree (nodes: CodeNode array) =
    let minX = nodes |> Array.map (fun node -> node.X) |> Array.min
    let maxX = nodes |> Array.map (fun node -> node.X) |> Array.max
    let minY = nodes |> Array.map (fun node -> node.Y) |> Array.min
    let maxY = nodes |> Array.map (fun node -> node.Y) |> Array.max
    let lower = min minX minY
    let upper = max maxX maxY
    let span = max 1.0 (upper - lower)
    let root = QuadCell(lower, lower, lower + span, lower + span, 0)
    for index in 0 .. nodes.Length - 1 do root.Insert index nodes
    root.UpdateMass nodes
    root

let private applyQuadRepulsion (root: QuadCell) (sourceIndex: int) (nodes: CodeNode array) (forceX: float array) (forceY: float array) =
    let source = nodes.[sourceIndex]
    let rec visit (cell: QuadCell) =
        if cell.Mass > 0.0 then
            let distance, dirX, dirY = distanceAndDir source.X source.Y cell.CenterX cell.CenterY
            let containsSource =
                source.X >= cell.MinX && source.X <= cell.MaxX &&
                source.Y >= cell.MinY && source.Y <= cell.MaxY
            match cell.Children with
            | None ->
                for targetIndex in cell.Points do
                    if targetIndex <> sourceIndex then
                        let target = nodes.[targetIndex]
                        let distance, dirX, dirY = distanceAndDir source.X source.Y target.X target.Y
                        let force = repulsionStrength / (distance * distance + 1.0)
                        forceX.[sourceIndex] <- forceX.[sourceIndex] - dirX * force
                        forceY.[sourceIndex] <- forceY.[sourceIndex] - dirY * force
            | Some cells when not containsSource && cell.Size / distance < barnesHutTheta ->
                let force = repulsionStrength * cell.Mass / (distance * distance + 1.0)
                forceX.[sourceIndex] <- forceX.[sourceIndex] - dirX * force
                forceY.[sourceIndex] <- forceY.[sourceIndex] - dirY * force
            | Some cells ->
                for child in cells do visit child
    visit root

let private collectNearby (root: QuadCell) x y radius (result: ResizeArray<int>) =
    let radiusSquared = radius * radius
    let rec visit (cell: QuadCell) =
        let dx = max 0.0 (max (cell.MinX - x) (x - cell.MaxX))
        let dy = max 0.0 (max (cell.MinY - y) (y - cell.MaxY))
        if dx * dx + dy * dy <= radiusSquared then
            match cell.Children with
            | None ->
                for index in cell.Points do result.Add index
            | Some cells ->
                for child in cells do visit child
    visit root

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

let private maxVelocity (nodes: CodeNode array) =
    nodes |> Array.fold (fun current node -> max current (max (abs node.Vx) (abs node.Vy))) 0.0

let applyForces (state: GraphState) (pinnedNodeIds: Set<NodeId>) =
    let nodes = state.Nodes |> Map.toArray |> Array.map snd
    if nodes.Length = 0 then
        state.Nodes
    else
        let nodeCount = nodes.Length
        let forceX = Array.zeroCreate<float> nodeCount
        let forceY = Array.zeroCreate<float> nodeCount
        let widths = nodes |> Array.map nodeWidth
        let heights = nodes |> Array.map nodeHeight

        let maxId = nodes |> Array.maxBy (fun node -> node.Id) |> fun node -> node.Id
        let indexById = Array.create (maxId + 1) -1
        for index in 0 .. nodeCount - 1 do indexById.[nodes.[index].Id] <- index

        // The quadtree is built once and shared by repulsion and the local
        // card-separation query. No immutable Map is updated inside the hot
        // force loops.
        let tree = if nodeCount >= 24 then Some (buildQuadTree nodes) else None

        if tree.IsNone then
            for i in 0 .. nodeCount - 1 do
                for j in i + 1 .. nodeCount - 1 do
                    let distance, dirX, dirY = distanceAndDir nodes.[i].X nodes.[i].Y nodes.[j].X nodes.[j].Y
                    let force = repulsionStrength / (distance * distance + 1.0)
                    let fx, fy = dirX * force, dirY * force
                    forceX.[i] <- forceX.[i] - fx
                    forceY.[i] <- forceY.[i] - fy
                    forceX.[j] <- forceX.[j] + fx
                    forceY.[j] <- forceY.[j] + fy
        else
            let root = tree.Value
            for i in 0 .. nodeCount - 1 do applyQuadRepulsion root i nodes forceX forceY

        // Only query nearby cards. The query buffer is reused for every node,
        // so this path does not allocate a candidate list per node or frame.
        let maxHalfDiagonal =
            widths
            |> Array.mapi (fun index width -> sqrt ((width / 2.0) ** 2.0 + (heights.[index] / 2.0) ** 2.0))
            |> Array.max
        let collisionRadius = collisionInfluenceScale * (2.0 * maxHalfDiagonal + collisionGap)
        let applyCollision i j =
            let distance, dirX, dirY = distanceAndDir nodes.[i].X nodes.[i].Y nodes.[j].X nodes.[j].Y
            let cardDistance = sqrt (((widths.[i] + widths.[j]) / 2.0) ** 2.0 + ((heights.[i] + heights.[j]) / 2.0) ** 2.0)
            let softDistance = (cardDistance + collisionGap) * collisionInfluenceScale
            let overlap = softDistance - distance
            if overlap > 0.0 then
                let normalizedOverlap = overlap / max 1.0 (softDistance - cardDistance)
                let force = normalizedOverlap * collisionStrength
                let fx, fy = dirX * force, dirY * force
                forceX.[i] <- forceX.[i] - fx
                forceY.[i] <- forceY.[i] - fy
                forceX.[j] <- forceX.[j] + fx
                forceY.[j] <- forceY.[j] + fy

        match tree with
        | None ->
            for i in 0 .. nodeCount - 1 do
                for j in i + 1 .. nodeCount - 1 do applyCollision i j
        | Some root ->
            let nearby = ResizeArray<int>()
            for i in 0 .. nodeCount - 1 do
                nearby.Clear()
                collectNearby root nodes.[i].X nodes.[i].Y collisionRadius nearby
                for k in 0 .. nearby.Count - 1 do
                    let j = nearby.[k]
                    if i < j then applyCollision i j

        // Apply edge springs using direct array indexing rather than Map.find.
        for (_, edge) in state.Edges |> Map.toArray do
            if edge.Source <= maxId && edge.Target <= maxId then
                let sourceIndex = indexById.[edge.Source]
                let targetIndex = indexById.[edge.Target]
                if sourceIndex >= 0 && targetIndex >= 0 then
                    let distance, dirX, dirY = distanceAndDir nodes.[sourceIndex].X nodes.[sourceIndex].Y nodes.[targetIndex].X nodes.[targetIndex].Y
                    let force = springStrength * (distance - springLength)
                    let fx, fy = dirX * force, dirY * force
                    forceX.[sourceIndex] <- forceX.[sourceIndex] + fx
                    forceY.[sourceIndex] <- forceY.[sourceIndex] + fy
                    forceX.[targetIndex] <- forceX.[targetIndex] - fx
                    forceY.[targetIndex] <- forceY.[targetIndex] - fy

        // Pull functions toward a loose centroid for their source file. The
        // previous implementation applied every same-file pair, which was
        // quadratic and dominated medium-sized function graphs.
        nodes
        |> Array.mapi (fun index node -> index, node)
        |> Array.groupBy (fun (_, node) ->
            if node.Kind = FunctionNode then node.ProjectPath + "\u0000" + node.FilePath else "")
        |> Array.iter (fun (key, members) ->
            if key <> "" && members.Length > 1 then
                let centroidX = members |> Array.averageBy (fun (_, node) -> node.X)
                let centroidY = members |> Array.averageBy (fun (_, node) -> node.Y)
                for (index, node) in members do
                    let distance, dirX, dirY = distanceAndDir node.X node.Y centroidX centroidY
                    let force = sameFileAttractionStrength * (distance - sameFileAttractionDistance)
                    forceX.[index] <- forceX.[index] + dirX * force
                    forceY.[index] <- forceY.[index] + dirY * force)

        // Centering is a single array pass.
        let centerX = state.CanvasWidth / 2.0
        let centerY = state.CanvasHeight / 2.0
        for index in 0 .. nodeCount - 1 do
            forceX.[index] <- forceX.[index] + (centerX - nodes.[index].X) * centeringStrength
            forceY.[index] <- forceY.[index] + (centerY - nodes.[index].Y) * centeringStrength

        // Map.map preserves the graph API, but all expensive force accumulation
        // above uses mutable indexed arrays. Map iteration order matches the
        // Map.toArray order used to build nodes.
        let mutable index = 0
        let nextNodes =
            state.Nodes
            |> Map.map (fun _ node ->
                let current = index
                index <- index + 1
                if node.Fixed || Set.contains node.Id pinnedNodeIds then
                    { node with Vx = 0.0; Vy = 0.0 }
                else
                    let vx = clamp ((node.Vx + forceX.[current] * timeStep) * damping) (-maxSpeed) maxSpeed
                    let vy = clamp ((node.Vy + forceY.[current] * timeStep) * damping) (-maxSpeed) maxSpeed
                    { node with Vx = vx; Vy = vy; X = node.X + vx * timeStep; Y = node.Y + vy * timeStep })
        nextNodes
