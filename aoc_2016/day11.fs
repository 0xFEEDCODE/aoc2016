module aoc_2016.day11

open System.Collections.Generic
open System.IO

type Chip = string
type Generator = string

type MoveConfiguration =
    | C of Chip
    | G of Generator
    | CC of Chip * Chip
    | GG of Generator * Generator
    | CG of Chip * Generator

[<CustomEquality; NoComparison>]
type Configuration =
    { mutable chips: Chip array
      mutable generators: Generator array }

    member this.moveChip (chip: Chip) (target: Configuration) =
        if not (Array.contains chip this.chips) then
            failwith "fail"

        this.chips <- Array.filter ((<>) chip) this.chips
        target.chips <- Array.append target.chips [|chip|]

    member this.moveGenerator (generator: Generator) (target: Configuration) =
        if not (Array.contains generator this.generators) then
            failwith "fail"

        this.generators <- Array.filter ((<>) generator) this.generators
        target.generators <- Array.append target.generators [|generator|]

    member this.Move(target: Configuration) =
        function
        | CG(c, g) ->
            this.moveChip c target
            this.moveGenerator g target
        | GG(g1, g2) ->
            this.moveGenerator g1 target
            this.moveGenerator g2 target
        | G g -> this.moveGenerator g target
        | C c -> this.moveChip c target
        | CC(c1, c2) ->
            this.moveChip c1 target
            this.moveChip c2 target

    member this.hasChipGotCorrespondingGenerator(ch: Chip) =
        this.generators |> Array.contains ch
        
    member this.hasGeneratorGotCorrespondingChip(gen: Generator) =
        this.chips |> Array.contains gen
        
        
    member this.GetCount() =
        this.chips.Length + this.generators.Length

    override this.GetHashCode() : int =
        let chipHash =
            this.chips
            |> Array.fold (fun acc chip -> acc + chip.GetHashCode()) 0

        let generatorHash =
            this.generators
            |> Array.fold (fun acc generator -> acc + generator.GetHashCode()) 0

        (chipHash * 397) ^^^ generatorHash

    
    member this.IsAnythingFried() =
        not (Array.isEmpty this.generators) && not(this.chips |> Array.forall(this.hasChipGotCorrespondingGenerator))
        (*
        let unhookedChips = this.chips |> Array.where(fun ch -> not (this.hasChipGotCorrespondingGenerator ch))
        let unhookedGenerators = this.generators |> Array.where(fun gen -> not (this.hasGeneratorGotCorrespondingChip gen))
        unhookedChips.Length > 0 && unhookedGenerators.Length > 0
        *)

    member this.Print() =
        printf "Chips: "
        if this.chips.Length > 0 then this.chips |> Seq.iter(fun ch -> printf $"{ch};") else printf "NONE"
        printf " || Generators: "
        if this.generators.Length > 0 then this.generators |> Seq.iter(fun gen -> printf $"{gen};") else printf "NONE"
        
    member this.DeepCopy() =
        { chips = Array.copy this.chips
          generators = Array.copy this.generators }
        
    override this.Equals(other: obj) =
        match other with
        | :? Configuration as otherConfig ->
            let chipEquality = Array.forall2 (=) this.chips otherConfig.chips
            let generatorEquality = Array.forall2 (=) this.generators otherConfig.generators
            chipEquality && generatorEquality
        | _ -> false

let executeMove (source: Configuration) (target: Configuration) =
    function
    | C c -> source.moveChip c target
    | G g -> source.moveGenerator g target
    | CC(c1, c2) ->
        source.moveChip c1 target
        source.moveChip c2 target
    | GG(g1, g2) ->
        source.moveGenerator g1 target
        source.moveGenerator g2 target
    | CG(c, g) ->
        source.moveChip c target
        source.moveGenerator g target
        
[<CustomEquality; NoComparison>]
type Layout =
    | Layout of int * Configuration array

    override this.GetHashCode() : int =
        let (Layout(floor, configurations)) = this

        configurations
        |> Array.fold (fun acc config -> (acc * 31) + config.GetHashCode()) (floor.GetHashCode())
        
    override this.Equals(other: obj) =
        match other with
        | :? Layout as otherLayout ->
            let (Layout(floor1, configurations1)) = this
            let (Layout(floor2, configurations2)) = otherLayout

            floor1 = floor2 &&
            Array.forall2 (=) configurations1 configurations2
        | _ -> false
        
        
    member this.Print() =
        let (Layout(_, configurations)) = this
        
        for i in 0..configurations.Length-1 do
            printf $"\nFloor {i}: "
            configurations[i].Print()
        
    member this.ConfigurationDeepCopy() =
        let (Layout(floor, configurations)) = this
        configurations |> Array.map (_.DeepCopy())

let search (layout: Layout) =
    let pQueue = PriorityQueue()
    pQueue.Enqueue(layout, 0)

    let cameFrom = Dictionary<Layout, Option<Layout>>()
    
    let visited = Dictionary<int, int>()
    visited.Add(layout.GetHashCode(), 0)
    cameFrom.Add(layout, None)

    let startFloor = 0
    let maxFloor = 3

    let totalItemCount =
        let (Layout(_, cfgs)) = layout
        cfgs |> (Seq.fold (fun acc cfg -> acc + cfg.GetCount()) 0)

    let mutable i = 0

    while pQueue.Count > 0 do
        let current = pQueue.Dequeue()
        let (Layout(floor, cfgs)) = current

        let currentHash = current.GetHashCode()
        let currentCost = visited[currentHash]

        if currentCost > i then
            i <- currentCost
            printfn "%A" i

        let areAllItemsOnHighestFloor = cfgs[maxFloor].GetCount() = totalItemCount

        if areAllItemsOnHighestFloor then
            let mutable step = currentCost-1
            printfn $"Found - %A{currentCost}"
            let mutable cfg = cameFrom[current]
            while cfg.IsSome do
                printf "\n\nSTEP %A" step
                cfg.Value.Print()
                step <- step - 1
                cfg <- cameFrom[cfg.Value]
                
        else
            let cMoves = cfgs[floor].chips |> Seq.map C
            let gMoves = cfgs[floor].generators |> Seq.map G

            let ccMoves =
                seq {
                    for c1 in cfgs[floor].chips do
                        for c2 in cfgs[floor].chips do
                            if c1 <> c2 then
                                if c1 > c2 then
                                    yield CC(c1, c2)
                }
            let cgMoves =
                seq {
                    for c in cfgs[floor].chips do
                        for g in cfgs[floor].generators do
                            yield CG(c, g)
                }
            let ggMoves =
                seq {
                    for g1 in cfgs[floor].generators do
                        for g2 in cfgs[floor].generators do
                            if g1 <> g2 then
                                yield GG(g1, g2)
                }

            let floorsToVisit = List()

            let floorDown = floor - 1
            let floorUp = floor + 1
            if  floorDown >= startFloor then
                floorsToVisit.Add(floorDown)
            if floorUp <= maxFloor then
                floorsToVisit.Add(floorUp)

            let allMoves =
                cMoves
                |> Seq.append gMoves
                |> Seq.append cgMoves
                |> Seq.append ggMoves
                |> Seq.append ccMoves

            for move in allMoves do
                for nextFloor in floorsToVisit do
                    let newCfg = current.ConfigurationDeepCopy()
                    executeMove newCfg[floor] newCfg[nextFloor] move
                    
                    if not(newCfg[floor].IsAnythingFried()) &&
                       not(newCfg[nextFloor].IsAnythingFried()) then
                        
                        let newLayout = Layout(nextFloor, newCfg)
                        let newCost = currentCost + 1

                        let newLayoutHash = newLayout.GetHashCode()

                        let alreadyVisited = visited.ContainsKey(newLayoutHash)

                        if not alreadyVisited || newCost < visited[newLayoutHash] then

                            if not alreadyVisited then
                                visited.Add(newLayoutHash, newCost)

                            let mutable enableRanking = false
                            let ranking =
                                if enableRanking then
                                    (newCfg
                                     |> Seq.mapi (fun i el -> (i, el))
                                     |> Seq.fold (fun acc (i, cfg) -> acc + (((i + 1) * (newCfg.Length-1)) * cfg.GetCount())) 0) * -1
                                else
                                    0

                            cameFrom[newLayout] <- Some(current)
                            pQueue.Enqueue(newLayout, newCost + ranking)

let solve () =
    let inp = File.ReadAllLines "day11.txt"

    let layout =
        Layout(
            0,
            inp
            |> Seq.map (fun line ->
                let split = line.Split ' ' |> Seq.mapi (fun i el -> i, el) |> Seq.toArray

                let chips =
                    split
                    |> Seq.where (fun (i, _) -> split.Length > i + 1 && (snd split[i + 1]).StartsWith("microchip"))
                    |> Seq.map (fun (_, el) -> (el.Split "-compatible")[0])
                    |> Seq.toArray

                let generators =
                    split
                    |> Seq.where (fun (i, _) -> split.Length > i + 1 && (snd split[i + 1]).StartsWith("generator"))
                    |> Seq.map snd
                    |> Seq.toArray

                { chips = chips
                  generators = generators })
            |> Seq.toArray
        )

    search layout

(*
let canMoveChip (mc: Chip) (target: Configuration) =
    (target.generators |> Array.contains mc) || target.AreAllPairsHooked()

let canMoveGenerator (gen: Generator) (target: Configuration) =
    (target.chips |> Array.contains gen) || target.AreAllPairsHooked()
    
let canMoveChip2 (ch1: Chip) (ch2: Chip) (target: Configuration) =
    ((target.generators |> Array.contains ch1) && (target.generators |> Array.contains ch2)) 
    || ((target.generators |> Array.contains ch1) && target.AreAllPairsHookedExceptChip ch1)
    || ((target.generators |> Array.contains ch2) && target.AreAllPairsHookedExceptChip ch2)
    
let canMoveGenerator2 (gen1: Generator) (gen2: Generator) (target: Configuration) =
    ((target.chips |> Array.contains gen1) && (target.chips |> Array.contains gen2)) 
    || ((target.chips |> Array.contains gen1) && target.AreAllPairsHookedExceptGen(gen1))
    || ((target.chips |> Array.contains gen2) && target.AreAllPairsHookedExceptGen(gen2))
    
let canMoveCg (ch: Chip) (gen: Generator) (target: Configuration) =
    (ch = gen)
    || ((target.chips |> Array.contains gen) && (target.generators |> Array.contains ch))
    || (target.chips |> Array.contains ch) && target.AreAllPairsHookedExceptChip(ch)
    || (target.generators |> Array.contains gen) && target.AreAllPairsHookedExceptGen(gen)
    

let canBeMoved (target: Configuration) =
    function
    | C c -> canMoveChip c target
    | G g -> canMoveGenerator g target
    | CC(c1, c2) -> canMoveChip2 c1 c2 target
    | GG(g1, g2) -> canMoveGenerator2 g1 g2 target
    | CG(c, g) -> canMoveCg c g target
    *)


