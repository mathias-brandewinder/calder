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
            Force = { Length = defaultLength; Stiffness = 1.0 }
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
