module aoc_2016.day19

open System.Collections.Generic
open System.Linq
open System

type Entry = {id: int; n_presents: int}

let solve() =
    let n_elves = 3005290
    
    let rec steal n_total = 
        let mutable presents = Dictionary<int, int>()
        Enumerable.Range(0, n_total) |> Seq.iter(fun i -> presents[i] <- 1)
        
        let mutable lpresents = []
        Enumerable.Range(0, n_total) |> Seq.iter(fun i -> lpresents <- lpresents |> List.insertAt i {id = i+1;n_presents = 1})
        
        (*
        let rec get_steal_from i = if presents[i] > 0 then i else get_steal_from ((i+1) % n_total)
        
        let rec loop i =
                let has_presents = presents[i] <> 0
                
                match has_presents with
                | true -> 
                    let steal_from = get_steal_from ((i+1) % n_total)
                
                    if steal_from <> i then
                        presents[i] <- presents[i] + presents[steal_from]
                        presents[steal_from] <- 0
                        
                        loop ((i+1) % n_total)
                    else
                        printfn $"Found %A{i}"
                | false -> 
                        loop ((i+1) % n_total)*)
                        
        let rec loop2 i =
                let has_presents = lpresents[i].n_presents <> 0
                
                match has_presents with
                | true -> 
                    let llen = lpresents |> List.length
                    if llen > 1 then
                        let steal_from = (i + llen /2)  % llen
                        
                        (*
                        presents[i] <- presents[i] + presents[steal_from]
                        presents[steal_from+removed] <- 0
                        printfn "%A" (i, steal_from+removed)
                        *)
                        
                        //printfn "Id: %A" (lpresents[steal_from].id)
                        printfn "Stealing from %A" (lpresents[i].id, lpresents[steal_from].id)
                        lpresents <- lpresents |> List.updateAt i {id = lpresents[i].id; n_presents = lpresents[steal_from].n_presents}
                        lpresents <- lpresents |> List.removeAt steal_from
                        let new_len = llen-1
                        
                        let new_i = (if steal_from > i then (i+1) else i) % new_len
                        
                        loop2 new_i
                    else
                        lpresents[0].id
                | false -> 
                        loop2 ((i+1) % n_total)
                
        
        loop2 0
        
    let steal2 n_total =
        let left = LinkedList()
        let right = LinkedList()
        
        for i in 1..n_total do
            if i < ((n_total / 2) + 1) then
                left.AddLast(i) |> ignore
            else
                right.AddFirst(i) |> ignore
                
        while (left.Count > 0 && right.Count > 0) do
            if (left.Count > right.Count) then
                left.RemoveLast()
            else
                right.RemoveLast()
                
            let first_left = left.First
            left.RemoveFirst()
            right.AddFirst(first_left)
            
            let last_right = right.Last
            right.RemoveLast()
            left.AddLast(last_right)
            
        if left.Count > 0 then left.First.Value else right.First.Value
        
    let calc x = x % (3.0 ** floor (Math.Log(x, 3)))
        
    printfn "%A" (steal 7)
    printfn "%A" (steal2 n_elves)
    exit(0)
    let r = calc 3005290
    printfn $"%A{r}"
        
    0

