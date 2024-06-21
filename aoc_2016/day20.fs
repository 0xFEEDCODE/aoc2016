module aoc_2016.day20

open System.IO

type Range =
    {Start: uint; End: uint}
    
    member this.IsDist1 other = ((max this.Start other.End) - (min this.Start other.End)) = 1u
    
    member this.Overlaps other =
        (this.End >= other.Start && this.End <= other.End) ||
        (other.End >= this.Start && other.End <= this.End) ||
        (this.IsDist1 other || other.IsDist1 this)
        
    member this.TryMerge other =
        match (this.Overlaps other) with
        | true -> Some({Start = min this.Start other.Start
                        End = max this.End other.End })
        | _ -> None
        
    member this.Length = 1u + (this.End - this.Start)

let create_range s e = {Start = s; End = e }
let is_overlapping x y = not (x.Start = y.Start && x.End = y.End) && x.Overlaps y

let calc (blacklisted: seq<Range>) =
    let full_range = create_range 0u 4294967295u
    
    let excluded = blacklisted |> Seq.fold(fun acc bl ->
        let new_range = acc
                        |> Array.where(is_overlapping bl)
                        |> Array.append [|bl|]
                        |> Seq.reduce(fun acc x -> create_range (min acc.Start x.Start) (max acc.End x.End))
                        
        acc |> Array.where(fun x -> not(is_overlapping bl x)) |> Array.append [|new_range|]) [||]
        
    let is_valid = excluded |> Seq.forall(fun x -> (excluded |> Seq.tryFind(is_overlapping x) ).IsNone)
    if not is_valid then
        failwith "fu"
        
    let lowest = excluded |> Seq.sortBy(_.Start) |> Seq.mapi(fun i x -> (i, x)) |> Seq.find(fun (i, _) -> (excluded[i].End+1u) <> excluded[i+1].Start)
    let n_excluded = (excluded |> Seq.sumBy(_.Length))
    let remaining = full_range.Length - (excluded |> Seq.sumBy(_.Length))
    printfn $"%A{(lowest, remaining, n_excluded)}" 
        
        
let solve() =
    let inp = File.ReadAllLines "day20.txt"
    calc (inp |> Seq.map(fun l ->
                         let spl = l.Split("-")
                         create_range (spl[0] |> uint) (spl[1] |> uint)))
    0