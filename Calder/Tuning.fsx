#load "Physics.fs"
#load "Graph.fs"
#load "Layout.fs"

open Calder
open Calder.Physics
open Calder.Graph
open Calder.Layout

let rng = System.Random 0

let nodesCount = 20
let edgesCount = 100

let edges =
    List.init edgesCount (fun _ ->
        rng.Next(0, nodesCount),
        rng.Next(0, nodesCount)
        )
    |> List.filter (fun (x, y) -> x <> y)
    |> List.distinct

let graph =
    (Graph.empty, List.init nodesCount id)
    ||> List.fold (fun graph node -> graph |> Graph.addNode node)
    |> fun graph ->
        (graph, edges)
        ||> List.fold (fun graph (x, y) ->
            graph
            |> Graph.addEdge { Node1 = x; Node2 = y; Force = { Length = 1.0; Stiffness = 0.5 } }
            )

let config: Layout.Config = {
    CenterAttraction = { Attractor.Strength = 0.10 }
    Disconnected = { Repulsor.Length = 1.0 }
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

let a0 = initialValue config graph

let updateAndAdjust config (alpha, graph) =
    [ alpha * 0.99; alpha; alpha * 1.01 ]
    |> List.map (fun a ->
        let updatedGraph =
            graph
            |> Layout.update a config
        let updatedEnergy =
            updatedGraph
            |> Layout.energy config
        (a, updatedGraph, updatedEnergy)
        )
    |> List.minBy (fun (a, graph, energy) ->
        energy)

let auto (iters, tolerance) config graph =
    let alpha = initialValue config graph
    let rec updated count (alpha, graph) =
        if count > iters
        then graph
        else
            let alpha', graph', energy' = updateAndAdjust config (alpha, graph)
            if energy' < tolerance
            then graph'
            else updated (count + 1) (alpha', graph')
    updated 0 (alpha, graph)

#time "on"
let test =
    auto (100, 0.01) config graph
    |> Layout.energy config

let basic =
    graph
    |> Layout.solve (a0, 100) config
    |> Layout.energy config

let manual =
    graph
    |> Layout.solve (0.25, 100) config
    |> Layout.energy config