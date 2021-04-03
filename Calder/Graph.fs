namespace Calder

type Edge<'Node when 'Node: comparison> =
    private | Edge of 'Node * 'Node
    with
    static member create (node1: 'Node, node2: 'Node) =
        if node1 < node2
        then Edge (node1, node2)
        else Edge (node2, node1)
    member this.Node1 = match this with | Edge (node1, _) -> node1
    member this.Node2 = match this with | Edge (_, node2) -> node2

type Graph<'Node when 'Node: comparison> = {
    Nodes: Set<'Node>
    Edges: Set<Edge<'Node>>
    }
    with
    static member empty<'Node> () =
        { Nodes = Set.empty<'Node>; Edges = Set.empty<Edge<'Node>> }
    static member addNode (node: 'Node) (graph: Graph<'Node>) =
        { graph with Nodes = graph.Nodes |> Set.add node }
    static member addEdge (node1: 'Node, node2: 'Node) (graph: Graph<'Node>) =
        let edge = Edge.create(node1, node2)
        { graph with Edges = graph.Edges |> Set.add edge }