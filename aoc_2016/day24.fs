module aoc_2016.day24

open System
open System.Collections.Generic
open System.IO

type Point = int * int

let solve() =
    let map = File.ReadLines "day24.txt" |> Seq.toArray
    
    let positions = map |>
                    Seq.mapi(fun y row -> (y,row))
                    |> Seq.map(fun (y, row) -> row |>
                                               Seq.mapi(fun x col -> (x, col)) |> Seq.map(fun (x, _) -> (y, x))) |> Seq.collect id
                    
    let points = positions |> Seq.where(fun (y,x) -> map[y][x] >= '0' && map[y][x] <= '9')
    
    let get_shortest_paths_between_points point_a point_b =
        
        let get_next_moves (point: Point) =
            let y,x = point
            seq {(y+1, x); (y-1, x); (y, x+1); (y, x-1)} |> Seq.where(fun (y2, x2) -> map[y2][x2] <> '#')
            
        let q = Queue()
        let visited = Dictionary<Point, int>()
        
        q.Enqueue (point_a, 0)
        visited.Add (point_a, 0)
            
        let mutable shortest_path = Int32.MaxValue
        while q.Count > 0 do
            let current_pos, dist = q.Dequeue()
            
            if current_pos = point_b then
                shortest_path <- min dist shortest_path
            else
                for next in get_next_moves current_pos |> Seq.where(fun n -> not (visited.ContainsKey(n))) do
                    visited.Add(next, dist+1)
                    q.Enqueue(next, dist+1)
        shortest_path
                
    let connections = points |> Seq.map(fun p -> points |> Seq.where(fun p2 -> p <> p2) |> Seq.map(fun p2 -> (p, p2)) |> Seq.toArray ) |> Seq.toArray
    
    let connection_map = Dictionary<char, Dictionary<char, int>>()
    
    for conn in connections do
        for (p1,p2) in conn do
            let dist = get_shortest_paths_between_points p1 p2
            let point_a, point_b, dist = map[fst p1][snd p1] ,map[fst p2][snd p2], dist
            if not (connection_map.ContainsKey(point_a)) then
                connection_map.Add(point_a, Dictionary())
            connection_map[point_a].Add(point_b, dist)
        
    let point_names = points |> Seq.map(fun p -> map[fst p][snd p]) |> Seq.toList
    
    
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs ->
          (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
      
    let cost = (permute point_names |> Seq.where(fun pn -> pn[0] = '0') |> Seq.map(fun pn -> pn @ ['0'])) |> Seq.map(fun points -> points |> Seq.windowed 2)
                       |> Seq.map(fun points_window -> points_window |> Seq.map(fun window ->
                           let point_a = window[0]
                           let point_b = window[1]
                           connection_map[point_a][point_b]) |> Seq.sum)
            
    printfn "%A" (cost |> Seq.sort)
            //printfn $"%A{get_shortest_paths_between_points p1 p2}"
        
    0


