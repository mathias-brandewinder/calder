#load "Physics.fs"
#load "Graph.fs"
#load "Layout.fs"
#load "Auto.fs"

open Calder
open Calder.Graph

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

let render graph layout =
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
        |> Seq.collect (fun kv ->
            let origin = layout.Nodes.[kv.Key]
            kv.Value
            |> Seq.map (fun kv ->
                let destination = layout.Nodes.[kv.Key]
                sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" style="stroke: Black;stroke-width:1" />"""
                    origin.X
                    origin.Y
                    destination.X
                    destination.Y
                )
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
    (Graph.empty, nodes)
    ||> List.fold (fun graph node -> graph |> Auto.addNode node)
    |> fun graph ->
        (graph, edges)
        ||> List.fold (fun graph (x, y) ->
            graph
            |> Auto.addEdge (x, y)
            )
    // |> fun graph ->
    //     { graph with
    //         Center = Attraction.coulomb 0.5 |> Some
    //     }

Auto.tightRadius (graph.Nodes.Count, 1.0)

let layout = Layout.initializeFrom graph

#time "on"

let manual =
    layout
    |> Layout.solve (1.0, 1000) graph
    |> Layout.energy graph

// Auto test

#load "Auto.fs"

let SPRING = Auto.Spring.solve (1000, 0.01) graph
SPRING |> Layout.energy graph


let fr = Auto.FruchtermanReingold.solve (100, 0.01, 1.0) graph
fr |> Layout.energy graph

layout |> Layout.energy graph
let solved = Calder.Auto.solve (100, 0.01) graph
solved  |> Layout.energy graph

// #load "Render.fsx"
SPRING |> render graph

// run the algo 100 times to see if we get explosions
let crashes () =

    let seeds = [ 0 .. 99 ]
    seeds
    |> List.map (fun seed ->
        let rng = System.Random seed

        let nodesCount = 10
        let edgesCount = 30

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
            ||> List.fold (fun graph node -> graph |> Auto.FruchtermanReingold.addNode node)
            |> fun graph ->
                (graph, edges)
                ||> List.fold (fun graph (x, y) ->
                    graph
                    |> Auto.FruchtermanReingold.addEdge (x, y)
                    )
            // |> fun graph ->
            //     { graph with
            //         Center = Attraction.coulomb 0.1 |> Some
            //     }

        let layout = Layout.initializeFrom graph
        let initialNrj = layout |> Layout.energy graph
        let solved = graph |> Auto.FruchtermanReingold.solve (100, 0.01, 0.95) // |> Calder.Auto.solve (100, 0.001)
        let finalEnergy = solved  |> Layout.energy graph

        seed, initialNrj, finalEnergy
        )