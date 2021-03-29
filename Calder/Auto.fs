namespace Calder

[<RequireQualifiedAccess>]
module Auto =

    open Calder.Physics
    open Calder.Graph

    let defaultLength = 1.0

    // how large a circle do we need if we want to fit n equal circles in it?
    // https://en.wikipedia.org/wiki/Circle_packing#Densest_packing
    let tightRadius (radius: float) (graph: Graph<_>) =
        let nodes = graph.Nodes.Count
        let packed = (pown (radius) 2) * (float nodes) / 0.9069
        sqrt packed

    let addNode = Graph.addNode
    let addEdge (node1, node2) graph =
        graph
        |> Graph.addEdge {
            Node1 = node1
            Node2 = node2
            Force = { Length = defaultLength; Stiffness = 0.5 }
            }

    let initialValue config graph =
        let initialEnergy =
            graph
            |> Layout.energy config
        let alpha0 = 1.0
        let value0 =
            graph
            |> Layout.update alpha0 config
            |> Layout.energy config

        let mult, op =
            if value0 > initialEnergy
            then 0.9, (>)
            else 1.1, (<)

        let breakpoint =
            let rec search alpha =
                let updatedAlpha = mult * alpha
                let value =
                    graph
                    |> Layout.update updatedAlpha config
                    |> Layout.energy config
                if op value initialEnergy
                then updatedAlpha
                else search updatedAlpha
            search alpha0
        let step = breakpoint / 10.0
        [ step .. step .. breakpoint ]
        |> List.minBy (fun x ->
            graph
            |> Layout.update x config
            |> Layout.energy config
            )

    let foo (config: Layout.Config) (graph: Graph<_>) =
        { graph with
            Nodes =
                graph.Nodes
                |> Map.map (fun node position ->
                    let force = Layout.nodeForce config graph node
                    let amplitude = force.Length
                    let aggressiveness =
                        if amplitude > 0.10
                        then 0.10 / amplitude
                        else 0.10
                    position + aggressiveness * force
                    )
        }

    let solve' (rate, iters, tolerance) config (graph: Graph<_>) =
        match graph.State with
        | Empty -> graph
        | Single -> graph
        | Full ->
            let rec improve iter graph =
                if iter > iters
                then graph
                else
                    let graph' = foo config graph // Layout.update rate config graph
                    if (graph' |> Layout.energy config) < tolerance
                    then graph'
                    else improve (iter + 1) graph'
            improve 0 graph

    let solve (iters, tolerance) (graph: Graph<_>) =
        let surfaceRadius =
            graph
            |> tightRadius (defaultLength / 2.0)
        let config: Layout.Config = {
            CenterAttraction = { Attractor.Strength = 1.0 / surfaceRadius }
            NodeRepulsion = { CoulombRepulsor.Repulsion = defaultLength }
            }
        let alpha = initialValue config graph
        solve' (alpha, iters, tolerance) config graph

