module aoc_2016.day11

open System.IO


type Chip = string
type Generator = string

[<CustomEquality; NoComparison>]
type MoveConfiguration =
    | C of Chip
    | G of Generator
    | CC of Chip * Chip
    | GG of Generator * Generator
    | CG of Chip * Generator
    
    override this.Equals other =
        match other with
        | :? MoveConfiguration as otherMc -> 
            match this, otherMc with
            | C c1, C c2 -> c1 = c2
            | G g1, G g2 -> g1 = g2
            | CC (c1a, c1b), CC (c2a, c2b) -> (c1a = c2a && c1b = c2b) || (c1a = c2b && c2a = c1b)
            | GG (g1a, g1b), GG (g2a, g2b) -> g1a = g2a && g1b = g2b || (g1a = g2b && g2a = g1b)
            | CG (c1, g1), CG (c2, g2) -> (c1 = c2 && g1 = g2)
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this with
        | C c -> c.GetHashCode()
        | G g -> g.GetHashCode()
        | CC (c1, c2) -> c1.GetHashCode() + c2.GetHashCode()
        | GG (g1, g2) -> g1.GetHashCode() + g2.GetHashCode()
        | CG (c, g) -> c.GetHashCode() + g.GetHashCode()
    
type Configuration =
    {mutable chips: Chip List; mutable generators: Generator List}
    
    member this.moveChip (chip: Chip) (target: Configuration) =
        if not (this.chips |> List.contains chip) then
            failwith "fail"
            
        this.chips <- this.chips |> List.removeAt (this.chips |> List.findIndex(fun el -> el = chip))
        target.chips <- target.chips |> List.insertAt target.chips.Length chip
        
    member this.moveGenerator (generator: Generator) (target: Configuration) =
        if not (this.generators |> List.contains generator) then
            failwith "fail"
            
        this.generators <- this.generators |> List.removeAt (this.generators |> List.findIndex(fun el -> el = generator))
        target.generators <- target.generators |> List.insertAt target.generators.Length generator
        
    member this.Move (target: Configuration) =
        function
        | CG (c, g) -> this.moveChip c target; this.moveGenerator g target
        | GG (g1, g2) -> this.moveGenerator g1 target; this.moveGenerator g2 target
        | G g -> this.moveGenerator g target
        | C c -> this.moveChip c target
        | CC (c1, c2) -> this.moveChip c1 target; this.moveChip c2 target
        
    member this.hasChipHasCorrespondingGenerator (chip: Chip) =
        if not (this.chips |> List.contains chip) then
            failwith "fail"
        this.generators |> List.contains chip
        
    member this.GetCount() = this.chips.Length + this.generators.Length

let canMoveChip (mc: Chip) (target: Configuration) =
    (target.chips |> Seq.isEmpty) || (target.chips |> Seq.exists(fun chip -> chip = mc))
    
let canMoveGenerator (gen: Generator) (target: Configuration) =
    (target.generators |> Seq.isEmpty) || not (target.chips |> Seq.exists(fun mc -> gen <> mc))
    
let canBeMoved (target: Configuration) =
    function
    | C c -> canMoveChip c target
    | G g -> canMoveGenerator g target
    | CC (c1, c2) -> canMoveChip c1 target && canMoveChip c2 target
    | GG (g1, g2) -> canMoveGenerator g1 target && canMoveGenerator g2 target
    | CG (c, g) -> canMoveChip c target && canMoveGenerator g target
    
let rec makePairs x acc =
    function
    | [] -> acc
    | head::tail -> makePairs x (acc @ [(x, head)]) tail
            
    
let x (conf: Configuration array) =
    let itemCount = conf |> Seq.fold(fun acc el -> acc + el.GetCount()) 0
    
    let rec loop nSteps (configuration: Configuration seq) =
        if (configuration |> Seq.item 0).GetCount() = itemCount then
            nSteps
        else
            let possibleConfigurationsUp =
                configuration |> Seq.skip 1 |> Seq.mapi(fun i conf -> 
            
                    let chips = conf.chips |> Seq.toList
                    let generators = conf.generators |> Seq.toList
                    
                    let confC = chips |> Seq.map(C) |> Seq.where (canBeMoved (configuration |> Seq.item (i-1)))
                    let confG = generators |> Seq.map(G) |> Seq.where (canBeMoved (configuration |> Seq.item (i-1)))
                    let confCC = chips |> Seq.map(fun c -> makePairs c [] (chips |> List.where(fun _c -> c <> _c))) |> Seq.collect id |> Seq.map(CC) |> Seq.distinct |> Seq.where (canBeMoved (configuration |> Seq.item (i-1)))
                    let confGG = generators |> Seq.map(fun g -> makePairs g [] (generators |> List.where(fun _g -> g <> _g))) |> Seq.collect id |> Seq.map(GG)  |> Seq.where (canBeMoved (configuration |> Seq.item (i-1)))
                    let confCG = chips |> Seq.map(fun c -> makePairs c [] (generators |> Seq.toList)) |> Seq.collect id |> Seq.map(CG) |> Seq.distinct |> Seq.where (canBeMoved (configuration |> Seq.item (i-1)))
                    [confC; confG; confCC; confGG; confCG] |> Seq.concat)
                
            let possibleConfigurationsDown =
                configuration |> Seq.take (Seq.length configuration - 1) |> Seq.mapi(fun i conf -> 
            
                    let chips = conf.chips |> Seq.toList
                    let generators = conf.generators |> Seq.toList
                    
                    let confC = chips |> Seq.map(C) |> Seq.where (canBeMoved (configuration |> Seq.item (i+1)))
                    let confG = generators |> Seq.map(G) |> Seq.where (canBeMoved (configuration |> Seq.item (i+1)))
                    let confCC = chips |> Seq.map(fun c -> makePairs c [] (chips |> List.where(fun _c -> c <> _c))) |> Seq.collect id |> Seq.map(CC) |> Seq.distinct |> Seq.where (canBeMoved (configuration |> Seq.item (i+1)))
                    let confGG = generators |> Seq.map(fun g -> makePairs g [] (generators |> List.where(fun _g -> g <> _g))) |> Seq.collect id |> Seq.map(GG)  |> Seq.where (canBeMoved (configuration |> Seq.item (i+1)))
                    let confCG = chips |> Seq.map(fun c -> makePairs c [] (generators |> Seq.toList)) |> Seq.collect id |> Seq.map(CG) |> Seq.distinct |> Seq.where (canBeMoved (configuration |> Seq.item (i+1)))
                    [confC; confG; confCC; confGG; confCG] |> Seq.concat)
                
            possibleConfigurationsUp |> Seq.iter(fun possibleConf -> loop nSteps+1 possibleConf))
            possibleConfigurationsDown |> Seq.iter(fun possibleConf -> loop nSteps+1 possibleConf)
            
            false
    loop conf
        

let solve() =
    let inp = File.ReadAllLines "day11.txt"
    
    let configurationAtFloors = inp |> Seq.map(fun line ->
        let split = line.Split ' ' |> Seq.mapi(fun i el -> i, el) |> Seq.toArray
        let chips = split |> Seq.where(fun (i, _) -> split.Length > i+1 && (snd split[i+1]).StartsWith("microchip")) |> Seq.map(fun (_, el) -> (el.Split "-compatible")[0]) |> Seq.toList
        let generators = split |> Seq.where(fun (i, _) -> split.Length > i+1 && (snd split[i+1]).StartsWith("generator")) |> Seq.map snd |> Seq.toList
        
        {chips = chips; generators = generators}) |> Seq.toArray
    
    let print() = configurationAtFloors |> Seq.iter(printfn "%A")
    
    x configurationAtFloors
        
    //print()
    0

