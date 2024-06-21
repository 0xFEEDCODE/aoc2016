module aoc_2016.day22

open System
open System.Collections.Generic
open System.IO

type Node =
    {Name: string; Size: int; Used : int; Avail : int }
    
    member this.Pos =
        let spl = (this.Name.Split "-") |> Seq.where(fun x -> x[0] = 'x' || x[0] = 'y') |> Seq.toArray 
        let x = spl[0] |> Seq.skipWhile(fun ch -> ch < '0' || ch > '9') |> Seq.fold(fun acc x -> acc + x.ToString()) ""
        let y = spl[1] |> Seq.skipWhile(fun ch -> ch < '0' || ch > '9') |> Seq.fold(fun acc x -> acc + x.ToString()) ""
        (y |> int, x |> int)
        
    member this.CanFitIntoOther (other: Node) =
        this.Used <> 0 && this.Name <> other.Name && this.Used < other.Avail
        
    static member Default =
        {Name = ""; Size = 0; Used = 0; Avail = 0; }

let solve() =
    let inp = File.ReadLines "day22.txt"
    
    let nodes = inp |> Seq.map(fun line ->
        let spl = line.Split(" ") |> Seq.where(fun x -> x.Length > 0) |> Seq.toArray
        
        let numerical_data = spl[1..] |>
                             Seq.map(fun x -> (x |> Seq.takeWhile(fun ch -> ch >= '0' && ch <= '9'))|> Seq.fold(fun acc ch -> acc + ch.ToString()) "" )
                             |> Seq.map(fun x -> x |> int) |> Seq.toArray
        
        {Name=spl[0]; Size = numerical_data[0]; Used = numerical_data[1]; Avail = numerical_data[2]; }) |> Seq.toArray
    
    let positions = nodes |> Seq.map(_.Pos)
    let y_positions = positions |> Seq.map(fst) |> Seq.distinct
    let x_positions = positions |> Seq.map(snd) |> Seq.distinct
    
    let node_map = Array2D.create (y_positions |> Seq.length) (x_positions |> Seq.length) Node.Default
    
    let init_node_map() = 
        for node in nodes do
            let y, x = node.Pos
            node_map[y,x] <- node
    init_node_map()
    
    let create_node_map() = 
        let map = Array2D.create (y_positions |> Seq.length) (x_positions |> Seq.length) Node.Default
        for node in nodes do
            let y, x = node.Pos
            map[y,x] <- node
        map
        
        
    let print() = 
        for y in y_positions do
            printf "\n"
            for x in x_positions do
                if ((y,x) = (22 ,24)) then
                    printf "O "
                else
                    let canFitIntoAny = positions |> Seq.tryFind(fun (y2, x2) -> node_map[y,x].CanFitIntoOther node_map[y2,x2]) |> Option.isSome
                    printf $"{(if canFitIntoAny then 'x' else '-')} "
                    
    print()
                    
    let move (source_node : Node) (target_node : Node) =
        let updated_source = { Name = source_node.Name; Size = source_node.Size;Used =  0; Avail = source_node.Avail; }
        let updated_target = { Name = target_node.Name; Size = target_node.Size;Used = target_node.Used + source_node.Used; Avail = target_node.Avail; } 
        (updated_source, updated_target)
        
    let find_path_from_empty_drive_to_goal (empty_drive_pos: int*int) (goal_pos: int*int)  =
        let get_neighbours (pos : int * int) =
            let y, x = pos
            seq {(y+1, x); (y-1, x); (y, x+1); (y, x-1)} |> Seq.where(fun (y2, x2) -> (positions |> Seq.contains(y2,x2)))
            
        let stack = Queue()
        
        let came_from = Dictionary<int * int, Option<int * int>>()
        let rec trace_path (node: Option<int * int>) =
            let rec tp (node: Option<int * int>) (path: seq<int*int>) = 
                if node.IsNone then
                    path
                else
                    tp came_from[node.Value] (path |> Seq.append [|node.Value|])
            tp node Seq.empty
            
        came_from.Add(empty_drive_pos, None)
        stack.Enqueue (empty_drive_pos, 0, create_node_map())
        
        let mutable shortest_path_length = Int32.MaxValue
        
        while (stack.Count > 0) do
            let current_pos, n_steps, node_map = stack.Dequeue()
            let current_y, current_x = current_pos
            let current_node = node_map[current_y, current_x]
            
            if (current_pos = goal_pos) then
                let path = trace_path (Some(current_pos))
                for x in path do
                    printf "%A" x
                shortest_path_length <- min shortest_path_length n_steps
                printfn $"Found! %A{shortest_path_length}"
            else
                for neigh_pos in (get_neighbours current_pos) do
                    let neigh_y, neigh_x = neigh_pos
                    
                    let neigh_node = node_map[neigh_y, neigh_x]
                    if not (came_from.ContainsKey(neigh_pos)) &&
                        neigh_node.CanFitIntoOther current_node then
                           let new_node_map = create_node_map()
                           let updated_source, updated_target = move neigh_node current_node
                           new_node_map[current_y, current_x] <- updated_source
                           new_node_map[neigh_y, neigh_x] <- updated_target
                               
                           stack.Enqueue (neigh_pos, n_steps + 1, new_node_map)
                           came_from.Add(neigh_pos, Some(current_pos))
        shortest_path_length
           
            
    let empty_drive_pos = (nodes |> Seq.find(fun n -> n.Used = 0)).Pos
    let goal_pos = (y_positions |> Seq.head, x_positions |> Seq.last)
    let spl1 = find_path_from_empty_drive_to_goal empty_drive_pos goal_pos
    
    (*
    let (empty_y,empty_x) = empty_drive_pos
    let (goal_y, goal_x) = goal_pos
    let (updated_source, updated_target) = move node_map[goal_y, goal_x] node_map[empty_y, empty_x]
    node_map[goal_y, goal_x] <- updated_source
    node_map[empty_y, empty_x] <- updated_target
    let spl2 = find_path_from_empty_drive_to_goal goal_pos (0,0)
    printfn "%A" (spl1, spl2)
    *)
            
    0

