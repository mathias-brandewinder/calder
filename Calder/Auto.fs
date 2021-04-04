namespace Calder

[<RequireQualifiedAccess>]
module Auto =

    open Calder.Physics
    open Calder.ForceGraph

    let defaultLength = 1.0

    // how large a circle do we need if we want to fit n equal circles in it?
    // https://en.wikipedia.org/wiki/Circle_packing#Densest_packing
    let tightRadius (nodes: int, radius: float) =
        let packed = (pown (radius) 2) * (float nodes) / 0.9069
        sqrt packed

    [<RequireQualifiedAccess>]
    module Spring =

        let addNode node = ForceGraph.addNode (node, Repulsion.SquareRoot)

        let addEdge (node1, node2) graph =
            graph
            |> ForceGraph.addEdge {
                Node1 = node1
                Node2 = node2
                Force = Spring.Log
                }

        let setup (graph: Graph<'Node>) =
            (ForceGraph.empty, graph.Nodes)
            ||> Seq.fold (fun forceGraph node -> forceGraph |> addNode node)
            |> fun forceGraph ->
                (forceGraph, graph.Edges)
                ||> Seq.fold (fun graph edge ->
                    graph |> addEdge (edge.Node1, edge.Node2)
                    )

        let solve (iters, tolerance) (graph: ForceGraph<_>) =
            let layout = Layout.initializeFrom graph
            let rec search iter layout =
                if iter > iters
                then layout
                else
                    let updated = Layout.update 0.1 graph layout
                    let energy = Layout.energy graph updated
                    if energy < tolerance
                    then updated
                    else
                        search (iter + 1) updated
            search 0 layout

    [<RequireQualifiedAccess>]
    module FruchtermanReingold =

        // we assume k = 1, and computed the area accordingly

        let attraction = {
            new Force with
                member this.applyFrom origin target =
                    let direction = target - origin
                    let length = direction.Length
                    let strength = (pown length 2)
                    strength * direction
            }

        let repulsion = {
            new Force with
                member this.applyFrom origin target =
                    let direction = target - origin
                    let length = direction.Length
                    let strength = 1.0 / (max length 0.01)
                    - strength * direction
            }

        let addNode node = ForceGraph.addNode (node, repulsion)

        let addEdge (node1, node2) graph =
            graph
            |> ForceGraph.addEdge {
                Node1 = node1
                Node2 = node2
                Force = attraction
                }

        let setup (graph: Graph<'Node>) =
            (ForceGraph.empty, graph.Nodes)
            ||> Seq.fold (fun forceGraph node -> forceGraph |> addNode node)
            |> fun forceGraph ->
                (forceGraph, graph.Edges)
                ||> Seq.fold (fun graph edge ->
                    graph |> addEdge (edge.Node1, edge.Node2)
                    )

        let update temp (graph: ForceGraph<_>) (layout: Layout.Layout<_>) =
            {
                layout with
                    Nodes =
                        layout.Nodes
                        |> Map.map (fun node position ->
                            let force = Layout.nodeForce graph layout node
                            let aggressiveness = min temp (1.0 / force.Length)
                            position + aggressiveness * force
                            )
            }

        let solve (iters, tolerance, cooldown) (graph: ForceGraph<_>) =

            let area = float graph.Nodes.Count
            let side = sqrt area

            let layout = Layout.initializeFrom graph

            let rec search (iter, temp) layout =
                if iter > iters
                then layout
                else
                    let updated = update temp graph layout
                    let energy = Layout.energy graph updated
                    if energy < tolerance
                    then updated
                    else
                        search (iter + 1, temp * cooldown) updated
            search (0, 1.0) layout

        let shrink (graph: Graph<'Node>) (layout: Layout.Layout<'Node>) =
            let center = layout.Center
            let total = layout.Nodes.Count |> float
            let subGraphs = graph |> Graphs.partitions
            let corrections =
                subGraphs
                |> List.collect (fun nodes ->
                    let subLayout =
                        { Layout.Nodes =
                            nodes
                            |> Seq.map (fun node ->
                                node, layout.Nodes.[node]
                                )
                            |> Map.ofSeq
                        }
                    let localCenter = subLayout.Center
                    let weight = 1.0 - (float nodes.Count / total)
                    nodes
                    |> Set.toList
                    |> List.map (fun node -> node, weight * (center - localCenter))
                    )
                |> Map.ofList
            { layout with
                Nodes =
                    layout.Nodes
                    |> Map.map (fun node point -> point + (-1.0 * corrections.[node]))
            }
