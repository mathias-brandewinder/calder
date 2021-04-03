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

    let addNode node = Graph.addNode (node, Repulsion.coulomb 1.0 )

    let addEdge (node1, node2) graph =
        graph
        |> Graph.addEdge {
            Node1 = node1
            Node2 = node2
            Force = { Spring.Length = defaultLength; Spring.Stiffness = 1.0 }
            }

    let solve (iters, tolerance) (graph: Graph<_>) =
        let layout = Layout.initializeFrom graph
        let rec search iter layout =
            if iter > iters
            then layout
            else
                let updated = Layout.update 1.0 graph layout
                let energy = Layout.energy graph updated
                if energy < tolerance
                then updated
                else
                    search (iter + 1) updated
        search 0 layout

    [<RequireQualifiedAccess>]
    module Spring =

        let addNode node = Graph.addNode (node, Repulsion.SquareRoot)

        let addEdge (node1, node2) graph =
            graph
            |> Graph.addEdge {
                Node1 = node1
                Node2 = node2
                Force = Spring.Log
                }

        let solve (iters, tolerance) (graph: Graph<_>) =
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

        let addNode node = Graph.addNode (node, Repulsion.SquareRoot)

        let addEdge (node1, node2) graph =
            graph
            |> Graph.addEdge {
                Node1 = node1
                Node2 = node2
                Force = Spring.Log
                }

        let solve (iters, tolerance) (graph: Graph<_>) =
            let area = 1.0
            let k = sqrt (area / float graph.Edges.Count)
            let repulsion = {
                new Force with
                    member this.applyFrom origin target =
                        let direction = target - origin
                        let length = direction.Length
                        let strength = (pown k 2) / length
                        - strength * direction
                }
            let attraction = {
                new Force with
                    member this.applyFrom origin target =
                        let direction = target - origin
                        let length = direction.Length
                        let strength = (pown length 2) / k
                        - strength * direction
                }
            let graph = {
                graph with
                    Nodes =
                        graph.Nodes
                        |> Map.map (fun _ _ -> repulsion)
                    Edges =
                        graph.Edges
                        |> Map.map (fun _ edges ->
                            edges
                            |> Map.map (fun _ _ -> attraction)
                            )
                }
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