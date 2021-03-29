namespace Calder

module Graph =

    open Physics

    type Edge<'Node when 'Node: comparison> = {
        Node1: 'Node
        Node2: 'Node
        Force: Force
        }

    type State =
        | Empty
        | Single
        | Full

    type Graph<'Node when 'Node: comparison> = {
        Nodes: Map<'Node, Point>
        Edges: Map<'Node, Map<'Node, Force>>
        }
        with
        member this.State =
            let nodes = this.Nodes.Count
            if nodes = 0
            then Empty
            elif nodes = 1
            then Single
            else Full
        member this.Node node =
            this.Nodes.[node]
        member this.NewPosition () =
            match this.State with
            | Empty -> { X = 0.0; Y = 0.0 }
            | _ ->
                let rng = System.Random ()
                {
                    X = rng.NextDouble () - 0.5
                    Y = rng.NextDouble () - 0.5
                }
        member this.Center =
            { X = 0.0; Y = 0.0 }
            // match this.State with
            // | Empty -> { X = 0.0; Y = 0.0 }
            // | _ ->
            //     let x = this.Nodes |> Seq.averageBy (fun kv -> kv.Value.X)
            //     let y = this.Nodes |> Seq.averageBy (fun kv -> kv.Value.Y)
            //     { X = x; Y = y }

    let empty = {
        Nodes = Map.empty
        Edges = Map.empty
        }

    let addNode (node: 'Node) (graph: Graph<'Node>): Graph<'Node> =
        { graph with
            Nodes = graph.Nodes |> Map.add node (graph.NewPosition ())
            Edges = graph.Edges |> Map.add node Map.empty
        }

    let removeNode node graph =
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

    let addEdge edge graph =
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
