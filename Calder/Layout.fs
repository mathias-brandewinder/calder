namespace Calder

module Layout =

    open Physics
    open Graph

    type State =
        | Empty
        | Single
        | Full

    type Layout<'Node when 'Node: comparison> =
        {
            Nodes: Map<'Node, Point>
        }
        with
        member this.State =
            if this.Nodes.Count = 0
            then Empty
            elif this.Nodes.Count = 1
            then Single
            else Full
        member this.Center =
            match this.Nodes.IsEmpty with
            | true -> Origin
            | false ->
                let x = this.Nodes |> Seq.averageBy (fun kv -> kv.Value.X)
                let y = this.Nodes |> Seq.averageBy (fun kv -> kv.Value.Y)
                { X = x; Y = y }

    let initializeFrom (graph: Graph<_>) =
        let rng = System.Random ()
        {
            Nodes =
                graph.Nodes
                |> Map.map (fun node _ ->
                    {
                        X = rng.NextDouble () - 0.5
                        Y = rng.NextDouble () - 0.5
                    }
                    )
        }

    let nodeForce (graph: Graph<_>) layout node =
        let position = layout.Nodes.[node]

        let nodesRepulsion =
            graph.Nodes
            |> Seq.sumBy (fun kv ->
                let force =
                    if kv.Key = node
                    then Neutral
                    else
                        kv.Value
                position
                |> force.applyFrom (layout.Nodes.[kv.Key])
                )

        let edgesAttraction =
            graph.Edges
            |> Map.find node
            |> Seq.sumBy (fun kv ->
                let force = kv.Value
                let origin = layout.Nodes.[kv.Key]
                position
                |> force.applyFrom origin
                )

        let centralAttraction =
            graph.Center
            |> Option.defaultValue Neutral
            |> fun force -> force.applyFrom Origin position

        nodesRepulsion + edgesAttraction + centralAttraction

    let update aggressiveness (graph: Graph<_>) (layout: Layout<_>) =
        {
            layout with
                Nodes =
                    layout.Nodes
                    |> Map.map (fun node position ->
                        position + aggressiveness * nodeForce graph layout node
                        )
        }

    let energy (graph: Graph<_>) layout =
        graph.Nodes
        |> Seq.sumBy (fun kv ->
            kv.Key
            |> nodeForce graph layout
            |> fun dir -> dir.Length
            )

    let solve (rate, iters) (graph: Graph<_>) (layout: Layout<_>) =
        match layout.State with
        | Empty -> layout
        | Single -> layout
        | Full ->
            layout
            |> Seq.unfold (fun layout ->
                let updated = update rate graph layout
                Some (layout, updated)
                )
            |> Seq.item iters

    let project (size: float) (layout: Layout<_>) =
        match layout.State with
        | Empty -> layout
        | Single ->
            { layout with
                Nodes =
                    layout.Nodes
                    |> Map.map (fun _ pos ->
                        { X = size / 2.0; Y = size / 2.0 }
                        )
            }
        | Full ->
            let xs, ys =
                layout.Nodes
                |> Seq.map (fun kv -> kv.Value.X, kv.Value.Y)
                |> Seq.toArray
                |> Array.unzip

            let xMin = xs |> Array.min
            let xMax = xs |> Array.max
            let yMin = ys |> Array.min
            let yMax = ys |> Array.max

            let scaleX x = size * (x - xMin) / (xMax - xMin)
            let scaleY y = size * (y - yMin) / (yMax - yMin)

            { layout with
                Nodes =
                    layout.Nodes
                    |> Map.map (fun _ pos ->
                        { X = scaleX pos.X; Y = scaleY pos.Y }
                        )
            }
