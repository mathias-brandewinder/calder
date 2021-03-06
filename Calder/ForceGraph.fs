namespace Calder

module ForceGraph =

    open Physics

    type Edge<'Node when 'Node: comparison> = {
        Node1: 'Node
        Node2: 'Node
        Force: Force
        }

    type ForceGraph<'Node when 'Node: comparison> = {
        Nodes: Map<'Node, Force>
        Edges: Map<'Node, Map<'Node, Force>>
        Center: Option<Force>
        }

    let empty = {
        Nodes = Map.empty
        Edges = Map.empty
        Center = None
        }

    let addNode (node: 'Node, force: Force) (graph: ForceGraph<'Node>): ForceGraph<'Node> =
        { graph with
            Nodes = graph.Nodes |> Map.add node force
            Edges = graph.Edges |> Map.add node Map.empty
        }

    let removeNode node (graph: ForceGraph<_>) =
        { graph with
            Nodes = graph.Nodes |> Map.remove node
            Edges =
                graph.Edges
                |> Map.remove node
                |> Map.map (fun _ edges ->
                    edges
                    |> Map.filter (fun targetNode _ ->
                        targetNode <> node
                        )
                    )
        }

    let addEdge edge (graph: ForceGraph<_>) =
        { graph with
            Edges =
                graph.Edges
                |> Map.add
                    edge.Node1
                    (graph.Edges.[edge.Node1] |> Map.add (edge.Node2) edge.Force)
                |> Map.add
                    edge.Node2
                    (graph.Edges.[edge.Node2] |> Map.add (edge.Node1) edge.Force)
        }
