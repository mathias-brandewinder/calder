#load "Graph.fs"
#load "Physics.fs"
#load "ForceGraph.fs"
#load "Layout.fs"
#load "Auto.fs"

#time "on"

open Calder
open Calder.ForceGraph

let template (content: string) =
    content
    |> sprintf """
<!DOCTYPE html>
<html>
    <head/>
    <body>
        <div width=500 height=500>
            <svg width=500 height=500 viewBox="0 0 500 500">
                %s
            </svg>
        </div>
    </body>
</html>
"""

let save content =
    System.IO.File.WriteAllText(
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "graph.html"),
        content
        )

let render (graph: Graph<'Node>) layout =
    let layout =
        layout
        |> Layout.project 500.0
    let nodes =
        layout.Nodes
        |> Seq.map (fun kv ->
            let pos = kv.Value
            sprintf """<ellipse cx="%f" cy="%f" rx="3" ry="3"></ellipse>""" pos.X pos.Y
            )
        |> String.concat "\n"
    let edges =
        graph.Edges
        |> Seq.map (fun edge ->
            let origin = layout.Nodes.[edge.Node1]
            let destination = layout.Nodes.[edge.Node2]
            sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" style="stroke: Black;stroke-width:1" />"""
                origin.X
                origin.Y
                destination.X
                destination.Y
            )
        |> String.concat "\n"
    sprintf "%s\n%s" nodes edges
    |> template
    |> save


let rng = System.Random 0

let nodesCount = 15
let edgesCount = 15

let nodes = List.init nodesCount id
let edges =
    List.init edgesCount (fun _ ->
        rng.Next(0, nodesCount),
        rng.Next(0, nodesCount)
        )
    |> List.filter (fun (x, y) -> x <> y)
    |> List.distinct

let graph =
    Graph.empty ()
    |> Graph.addNodes nodes
    |> Graph.addEdges edges

// SPRING algorithm

let SPRING =
    graph
    |> Auto.Spring.setup
    |> Auto.Spring.solve (100, 0.01)

SPRING |> render graph

// Fruchterman-Reingold algorithm
let config: Auto.FruchtermanReingold.Config = { Iterations = 100; Tolerance = 0.01; Cooldown = 0.95 }
let fr =
    graph
    |> Auto.FruchtermanReingold.setup
    |> Auto.FruchtermanReingold.solve config
fr |> render graph

// run the algo 100 times to see if we get explosions
let stability () =

    let seeds = [ 0 .. 99 ]
    let maxNodes = 30
    let maxEdges = 30

    seeds
    |> List.map (fun seed ->
        let rng = System.Random seed

        let nodesCount = rng.Next maxNodes
        let edgesCount = rng.Next maxEdges

        let nodes = List.init nodesCount id
        let edges =
            List.init edgesCount (fun _ ->
                rng.Next(0, nodesCount),
                rng.Next(0, nodesCount)
                )
            |> List.filter (fun (x, y) -> x <> y)
            |> List.distinct

        let graph =
            Graph.empty ()
            |> Graph.addNodes nodes
            |> Graph.addEdges edges

        let fGraph = Auto.FruchtermanReingold.setup graph
        let layout = Layout.initializeFrom fGraph
        let initialEnergy = layout |> Layout.energy fGraph
        let solved = Auto.solve graph
        let finalEnergy = solved  |> Layout.energy fGraph

        seed, nodesCount, edgesCount, initialEnergy, finalEnergy
        )