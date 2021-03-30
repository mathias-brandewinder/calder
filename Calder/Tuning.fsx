#load "Physics.fs"
#load "Graph.fs"
#load "Layout.fs"
#load "Auto.fs"

open Calder
open Calder.Physics
open Calder.Graph
open Calder.Layout

let rng = System.Random 1

let nodesCount = 20
let edgesCount = 100

let nodes = List.init nodesCount id
let edges =
    List.init edgesCount (fun _ ->
        rng.Next(0, nodesCount),
        rng.Next(0, nodesCount)
        )
    |> List.filter (fun (x, y) -> x <> y)
    |> List.distinct

let graph =
    (Graph.empty, nodes)
    ||> List.fold (fun graph node -> graph |> Auto.addNode node)
    |> fun graph ->
        (graph, edges)
        ||> List.fold (fun graph (x, y) ->
            graph
            |> Auto.addEdge (x, y)
            )

// https://en.wikipedia.org/wiki/Circle_packing#Densest_packing
// let tightRadius (graph: Graph<_>) =
//     let nodes = graph.Nodes.Count
//     let packed = (pown (1.0 / 2.0) 2) * 3.14 * (float nodes) / 0.9069
//     sqrt (packed / 3.14)

Auto.tightRadius (graph.Nodes.Count, 1.0)

let layout = Layout.initializeFrom graph
let a0 = Auto.initialValue graph layout

#time "on"

let basic =
    layout
    |> Layout.solve (a0, 100) graph
    |> Layout.energy graph

let manual =
    layout
    |> Layout.solve (0.10, 100) graph
    |> Layout.energy graph

// Auto test

#load "Auto.fs"

layout |> Layout.energy graph
let solved = layout |> Calder.Auto.solve (100, 0.001) graph
solved  |> Layout.energy graph