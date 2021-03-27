#load "Physics.fs"
#load "Graph.fs"
#load "Layout.fs"

open Calder
open Calder.Physics
open Calder.Graph
open Calder.Layout

let graph =
    Graph.empty
    |> Graph.addNode 1
    |> Graph.addNode 2
    |> Graph.addNode 3
    |> Graph.addNode 4
    |> Graph.addEdge { Node1 = 1; Node2 = 2; Force = { Length = 0.1; Stiffness = 1.0 } }
    |> Graph.addEdge { Node1 = 3; Node2 = 4; Force = { Length = 0.1; Stiffness = 1.0 } }

let config: Layout.Config = {
    CenterAttraction = { Attractor.Strength = 0.25 }
    Disconnected = { Repulsor.Length = 0.25 }
    }

Layout.energy config graph

let solved = Layout.solve (0.5, 50) config graph
solved |> Layout.energy config

distance (solved.Node 1) (solved.Node 2)
distance (solved.Node 3) (solved.Node 4)

for origin in 1 .. 4 do
    for target in 1 .. 4 do
        printfn "%i %i: %.2f" origin target (distance (solved.Node origin) (solved.Node target))

solved |> Layout.project 100.0

graph
|> Layout.update 0.1 config
|> Layout.update 0.1 config
|> Layout.update 0.1 config
|> Layout.energy config

Graph.empty
|> Layout.project 100.0

let nodeA = { X = 0.0; Y = 0.0 }
let nodeB = { X = 1.0; Y = 0.0 }
let nodeC = { X = 0.0; Y = 1.0 }

let spring = { Length = 1.0; Stiffness = 1.0 } :> Force

let network =
    [
        nodeA, spring
        nodeC, spring
    ]

network
|> List.sumBy (fun (node, spring) -> nodeB |> spring.applyFrom node)