module aoc_2016.day18

open System.Linq

type Tile =
    | Trap
    | Safe

let solve() =
    let inp = ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."
    
    let to_tile ch =
        match ch with
        | '.' -> Safe
        | '^' -> Trap
        
    let to_ch ch =
        match ch with
        | Safe -> '.'
        | Trap -> '^'
    
    let range = Enumerable.Range(0, inp.Length)
    
    let rec do' (x: Tile[]) i safe_t =
        let safe_t = (safe_t + (x |> Seq.where(fun t -> t = Safe) |> Seq.length))
         
        if i = (400000)-1 then
            let y = (x |> Seq.map to_ch |> Seq.map string |> String.concat "")
            printfn "%A" (y, safe_t)
            ()
        else
             let new_x =
                 range |> Seq.map(fun idx ->
                 let l = if idx > 0 then x[idx-1] else Safe
                 let c = x[idx]
                 let r = if idx < x.Length-1 then x[idx+1] else Safe
                 if ((l = Trap && c = Trap && r <> Trap) ||
                     (l <> Trap && c = Trap && r = Trap) ||
                     (l = Trap && c <> Trap && r <> Trap) ||
                     (l <> Trap && c <> Trap && r = Trap)) then Trap else Safe)
                 
             do' (new_x |> Seq.toArray) (i+1) safe_t
            
            
    do' (inp |> Seq.map to_tile |> Seq.toArray) 0 0
                
                
            
    0