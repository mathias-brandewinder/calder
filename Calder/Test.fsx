#load "Physics.fs"
#load "Graph.fs"
#load "Layout.fs"

open Calder
open Calder.Physics
open Calder.Graph
open Calder.Layout

let graph =
    Graph.empty
    |> Graph.addNode (1, Repulsion.coulomb 1.0)
    |> Graph.addNode (2, Repulsion.coulomb 1.0)
    |> Graph.addNode (3, Repulsion.coulomb 1.0)
    |> Graph.addNode (4, Repulsion.coulomb 1.0)
    |> Graph.addEdge { Node1 = 1; Node2 = 2; Force = { Spring.Length = 1.0; Spring.Stiffness = 1.0 } }
    |> Graph.addEdge { Node1 = 3; Node2 = 4; Force = { Spring.Length = 1.0; Spring.Stiffness = 1.0 } }

let layout = graph |> Layout.initializeFrom

Layout.energy graph layout

layout
|> Layout.update 0.5 graph
// |> Layout.update 1.0 config
|> Layout.energy graph

let solved = Layout.solve (0.25, 50) graph layout
solved |> Layout.energy graph

solved |> Layout.project 100.0

layout
|> Layout.update 0.1 graph
|> Layout.update 0.1 graph
|> Layout.update 0.1 graph
|> Layout.energy graph

Graph.empty<int>
|> Layout.initializeFrom
|> Layout.project 100.0
