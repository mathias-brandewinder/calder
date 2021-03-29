namespace Calder

module Layout =

    open Physics
    open Graph

    type Config = {
        CenterAttraction: Force
        NodeRepulsion: Force
        }

    let nodeForce config graph node =
        let position = graph.Nodes.[node]
        let forces = graph.Edges.[node]
        let nodesRepulsion =
            graph.Nodes
            |> Seq.sumBy (fun kv ->
                let force =
                    if kv.Key = node
                    then Neutral
                    else
                        config.NodeRepulsion
                position
                |> force.applyFrom kv.Value
                )
        let edgesAttraction =
            graph.Nodes
            |> Seq.sumBy (fun kv ->
                let force =
                    if kv.Key = node
                    then Neutral
                    else
                        match forces |> Map.tryFind kv.Key with
                        | Some force -> force
                        | None -> Neutral
                position
                |> force.applyFrom kv.Value
                )
        let centralAttraction = config.CenterAttraction.applyFrom graph.Center position
        nodesRepulsion + edgesAttraction + centralAttraction

    let update aggressiveness config graph =
        {
            graph with
                Nodes =
                    graph.Nodes
                    |> Map.map (fun node position ->
                        position + aggressiveness * nodeForce config graph node
                        )
        }

    let energy config graph =
        graph.Nodes
        |> Seq.sumBy (fun kv ->
            kv.Key
            |> nodeForce config graph
            |> fun dir -> dir.Length
            )

    let solve (rate, iters) config (graph: Graph<_>) =
        match graph.State with
        | Empty -> graph
        | Single -> graph
        | Full ->
            graph
            |> Seq.unfold (fun graph ->
                let updated = update rate config graph
                Some (graph, updated)
                )
            |> Seq.item iters

    let project (size: float) (graph: Graph<_>) =
        match graph.State with
        | Empty -> graph
        | Single ->
            { graph with
                Nodes =
                    graph.Nodes
                    |> Map.map (fun _ pos ->
                        { X = size / 2.0; Y = size / 2.0 }
                        )
            }
        | Full ->
            let xs, ys =
                graph.Nodes
                |> Seq.map (fun kv -> kv.Value.X, kv.Value.Y)
                |> Seq.toArray
                |> Array.unzip

            let xMin = xs |> Array.min
            let xMax = xs |> Array.max
            let yMin = ys |> Array.min
            let yMax = ys |> Array.max

            let scaleX x = size * (x - xMin) / (xMax - xMin)
            let scaleY y = size * (y - yMin) / (yMax - yMin)

            { graph with
                Nodes =
                    graph.Nodes
                    |> Map.map (fun _ pos ->
                        { X = scaleX pos.X; Y = scaleY pos.Y }
                        )
            }
