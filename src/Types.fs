module Types

type NodeId = int
type EdgeId = int

type NodeKind =
    | ProjectNode
    | SourceFileNode
    | FunctionNode

type EdgeKind =
    | ProjectReference
    | FileReference
    | FunctionCall

type LayoutPreset =
    | ForceLayout
    | RadialLayout
    | GridLayout

type ProjectInfo = {
    Name: string
    Path: string
    References: string list
}

type SourceInput = {
    Project: string
    Path: string
    Text: string
}

type CodeFunction = {
    Name: string
    QualifiedName: string
    FilePath: string
    Line: int
    Body: string
    Calls: string list
}

type SourceFile = {
    ProjectPath: string
    Path: string
    Name: string
    ModuleName: string
    Text: string
    Functions: CodeFunction list
}

type CodeNode = {
    Id: NodeId
    Kind: NodeKind
    Name: string
    Detail: string
    ProjectPath: string
    FilePath: string
    Line: int option
    SourceCode: string option
    X: float
    Y: float
    Vx: float
    Vy: float
    Radius: float
    Fixed: bool
}

type CodeEdge = {
    Id: EdgeId
    Kind: EdgeKind
    Source: NodeId
    Target: NodeId
    Label: string
}

type GraphState = {
    Nodes: Map<NodeId, CodeNode>
    Edges: Map<EdgeId, CodeEdge>
    NextNodeId: NodeId
    NextEdgeId: EdgeId
    SelectedNodes: Set<NodeId>
    SelectedEdges: Set<EdgeId>
    Hovered: Choice<NodeId, EdgeId> option
    Drag: DragState
    PhysicsPaused: bool
    CanvasWidth: float
    CanvasHeight: float
    MouseX: float
    MouseY: float
    Zoom: float
    PanX: float
    PanY: float
    Layout: LayoutPreset
}

and DragState =
    | NoDrag
    | DragNode of NodeId * offsetX: float * offsetY: float * origMouseX: float * origMouseY: float
    | DragSelection of offsetX: float * offsetY: float
    | SelectBox of startX: float * startY: float
    | PanCanvas of lastX: float * lastY: float