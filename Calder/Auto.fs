namespace Calder

[<RequireQualifiedAccess>]
module Auto =

    open Calder.Physics
    open Calder.Graph

    let defaultLength = 1.0

    // how large a circle do we need if we want to fit n equal circles in it?
    // https://en.wikipedia.org/wiki/Circle_packing#Densest_packing
    let tightRadius (nodes: int, radius: float) =
        let packed = (pown (radius) 2) * (float nodes) / 0.9069
        sqrt packed

    let addNode node = Graph.addNode (node, { CoulombRepulsor.Repulsion = 1.0 })

    let addEdge (node1, node2) graph =
        graph
        |> Graph.addEdge {
            Node1 = node1
            Node2 = node2
            Force = { Length = defaultLength; Stiffness = 0.5 }
            }

    let initialValue graph layout =
        let initialEnergy =
            layout
            |> Layout.energy graph
        let alpha0 = 1.0
        let value0 =
            layout
            |> Layout.update alpha0 graph
            |> Layout.energy graph

        let mult, op =
            if value0 > initialEnergy
            then 0.9, (>)
            else 1.1, (<)

        let breakpoint =
            let rec search alpha =
                let updatedAlpha = mult * alpha
                let value =
                    layout
                    |> Layout.update updatedAlpha graph
                    |> Layout.energy graph
                if op value initialEnergy
                then updatedAlpha
                else search updatedAlpha
            search alpha0
        let step = breakpoint / 10.0
        [ step .. step .. breakpoint ]
        |> List.minBy (fun x ->
            layout
            |> Layout.update x graph
            |> Layout.energy graph
            )

    let solve (iters, tolerance) (graph: Graph<_>) =
        let surfaceRadius = tightRadius (graph.Nodes.Count, defaultLength / 2.0)
        let layout = Layout.initializeFrom graph
        let alpha = initialValue graph layout
        let rec search (iter, energy) layout =
            if iter > iters
            then layout
            else
                let updated = Layout.update alpha graph layout
                let updatedEnergy = Layout.energy graph updated
                printfn "Iter: %i, Energy: %.2f" iter updatedEnergy
                let change = abs ((energy - updatedEnergy) / energy)
                if change < tolerance
                then updated
                else
                    search (iter + 1, updatedEnergy) updated
        search (0, System.Double.MaxValue)

