module Types

type NodeId = int
type EdgeId = int

type NodeKind =
    | SourceFileNode
    | FunctionNode

type EdgeKind =
    | FileReference
    | FunctionCall

type CodeFunction = {
    Name: string
    QualifiedName: string
    FilePath: string
    Line: int
    Body: string
    Calls: string list
}

type SourceFile = {
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
}

and DragState =
    | NoDrag
    | DragNode of NodeId * offsetX: float * offsetY: float * origMouseX: float * origMouseY: float
    | DragSelection of offsetX: float * offsetY: float
    | SelectBox of startX: float * startY: float
