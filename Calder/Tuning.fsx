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


let rng = System.Random 1

let nodesCount = 20
let edgesCount = 20

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

let SPRINGGraph =
    graph
    |> Graphs.disjoint
    |> List.head
let SPRING =
    SPRINGGraph
    |> Auto.Spring.setup
    |> Auto.Spring.solve (100, 0.01)

SPRING |> render SPRINGGraph

// Fruchterman-Reingold algorithm
let subGraph =
    graph
    // |> Graphs.disjoint
    // |> List.maxBy (fun x -> x.Nodes.Count)
let fr =
    subGraph
    |> Auto.FruchtermanReingold.setup
    |> Auto.FruchtermanReingold.solve (100, 0.01, 0.95)
    |> Auto.FruchtermanReingold.shrink subGraph
fr |> render subGraph

// run the algo 100 times to see if we get explosions
let crashes () =

    let seeds = [ 0 .. 99 ]
    seeds
    |> List.map (fun seed ->
        let rng = System.Random seed

        let nodesCount = 10
        let edgesCount = 20

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
        let initialNrj = layout |> Layout.energy fGraph
        let solved = fGraph |> Auto.FruchtermanReingold.solve (100, 0.01, 0.95)
        let finalEnergy = solved  |> Layout.energy fGraph

        seed, initialNrj, finalEnergy
        )