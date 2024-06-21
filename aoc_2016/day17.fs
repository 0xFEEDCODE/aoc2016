module aoc_2016.day17

open System.Collections.Generic
open System.Diagnostics
open System.Security.Cryptography
open System.Text

let get_md5 (data: byte array) =
    let md5 = MD5.Create()

    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string
    
type Point = int * int
type Path = string
type Direction = | U | D | L | R
type TraverseRecord = Point * string

let solve () =
    let passcode = "rrrbmfta"

    let goal = (3, 3)
    let x_limit_lower = 0
    let x_limit_upper = (fst goal)

    let y_limit_lower = 0
    let y_limit_upper = (snd goal)

    
    let get_possible_points (point: int * int) (secret: string) =
        let point_x = fst point
        let point_y = snd point
        let pos_up = point_x, point_y - 1
        let pos_down = point_x, point_y + 1
        let pos_left = point_x - 1, point_y
        let pos_right = point_x + 1, point_y

        let hash = get_md5 (Encoding.Default.GetBytes(secret))

        let new_points =
            [| (U, pos_up); (D, pos_down); (L, pos_left); (R, pos_right) |]
            |> Array.mapi (fun i x ->(([| 'b'; 'c'; 'd'; 'e'; 'f' |]
                  |> Seq.tryFind (fun letter -> letter = (hash |> Seq.item i))).IsSome, x))
            |> Array.where (fun (is_open, (_, new_point)) ->
                let x = fst new_point
                let y = snd new_point

                is_open
                && x_limit_lower <= x && x <= x_limit_upper
                && y_limit_lower <= y && y <= y_limit_upper)
            |> Seq.map snd

        new_points
        
    let calc() =
        let pQueue = PriorityQueue()
        
        pQueue.Enqueue (((0,0), ""), 0)

        let visited = Dictionary<TraverseRecord, int>()
        let mutable longest = 0

        while pQueue.Count > 0 do
            let curr_point, curr_path = pQueue.Dequeue()
            
            if curr_point = goal then
                longest <- max (curr_path.Length) longest
                ()
            else
                for dir, new_point in (get_possible_points curr_point (passcode + curr_path)) do
                    let new_path = curr_path + dir.ToString()
                    
                    if not (visited.ContainsKey((new_point, new_path))) then
                        visited.Add((new_point, new_path), new_path.Length)
                        pQueue.Enqueue((new_point, new_path), new_path.Length)
                        
        printfn "%A" longest
        
    let calc2() =
        let rec calc' curr_point (curr_path: string) (visited: Dictionary<TraverseRecord, int>) longest_found  =
            if curr_point = goal then
                max curr_path.Length longest_found
            else
                let p =
                    (get_possible_points curr_point (passcode + curr_path))
                    |> Seq.filter(fun (dir, new_point) -> not (visited.ContainsKey(new_point, curr_path + dir.ToString())))
                    |> Seq.map (fun (dir, new_point) ->
                        let new_path = curr_path + dir.ToString()
                        visited.Add((new_point, new_path), new_path.Length)
                        calc' new_point new_path visited longest_found)
                    |> Seq.toArray
                    
                if (Seq.isEmpty p) then longest_found else Seq.max p
                                                                                  
        let sw = Stopwatch()
        sw.Start()
        let r = calc' (0,0) "" (Dictionary<TraverseRecord, int>()) 0
        sw.Stop()
        
        printfn "%A" sw.Elapsed
                
    //calc()
    calc2()

    0
