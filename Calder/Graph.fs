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

module Graphs =

    let partitions (graph: Graph<'Node>) =
        let rec f (groups: list<Set<'Node>>) (remaining: list<'Node>) =
            match remaining with
            | [] -> groups
            | node :: tl ->
                let connections =
                    graph.Edges
                    |> Set.filter (fun edge -> edge.Node1 = node || edge.Node2 = node)
                    |> Set.map (fun edge -> if edge.Node1 = node then edge.Node2 else edge.Node1)
                    |> Set.add node
                let remaining =
                    remaining
                    |> List.filter (fun x ->
                        not (connections |> Set.contains x)
                        )
                let rec mergeGroups acc (groups: list<Set<'Node>>) (candidate: Set<'Node>) =
                    match groups with
                    | [] -> candidate :: acc
                    | group :: rest ->
                        if (Set.intersect group candidate) <> Set.empty
                        then
                            let candidate = Set.union candidate group
                            mergeGroups acc rest candidate
                        else
                            mergeGroups (group :: acc) rest candidate

                let groups = mergeGroups [] groups connections
                f groups tl
        f [] (graph.Nodes |> Set.toList)

    let disjoint (graph: Graph<'Node>) =
        let graphs = partitions graph
        graphs
        |> List.map (fun nodes ->
            let edges =
                graph.Edges
                |> Set.filter (fun e -> nodes |> Set.contains e.Node1)
            (Graph.empty (), nodes)
            ||> Seq.fold (fun g n -> g |> Graph.addNode n)
            |> fun g ->
                (g, edges)
                ||> Seq.fold (fun g e -> g |> Graph.addEdge (e.Node1, e.Node2))
            )