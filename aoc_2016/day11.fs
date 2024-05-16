module aoc_2016.day11

open System.Collections.Generic
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
            | CC(c1a, c1b), CC(c2a, c2b) -> (c1a = c2a && c1b = c2b) || (c1a = c2b && c2a = c1b)
            | GG(g1a, g1b), GG(g2a, g2b) -> g1a = g2a && g1b = g2b || (g1a = g2b && g2a = g1b)
            | CG(c1, g1), CG(c2, g2) -> (c1 = c2 && g1 = g2)
            | _ -> false
        | _ -> false

    override this.GetHashCode() : int =
        match this with
        | C c -> c.GetHashCode()
        | G g -> g.GetHashCode()
        | CC(c1, c2) ->
            let hash1: int = c1.GetHashCode()
            let hash2: int = c2.GetHashCode()
            (hash1 * 397) ^^^ hash2
        | GG(g1, g2) ->
            let hash1: int = g1.GetHashCode()
            let hash2: int = g2.GetHashCode()
            (hash1 * 397) ^^^ hash2
        | CG(c, g) ->
            let hash1: int = c.GetHashCode()
            let hash2: int = g.GetHashCode()
            (hash1 * 397) ^^^ hash2

[<CustomEquality; NoComparison>]
type Configuration =
    { mutable chips: Chip list
      mutable generators: Generator list }

    member this.moveChip (chip: Chip) (target: Configuration) =
        if not (this.chips |> List.contains chip) then
            failwith "fail"

        this.chips <- this.chips |> List.removeAt (this.chips |> List.findIndex (fun el -> el = chip))
        target.chips <- target.chips |> List.insertAt target.chips.Length chip

    member this.moveGenerator (generator: Generator) (target: Configuration) =
        if not (this.generators |> List.contains generator) then
            failwith "fail"

        this.generators <-
            this.generators
            |> List.removeAt (this.generators |> List.findIndex (fun el -> el = generator))

        target.generators <- target.generators |> List.insertAt target.generators.Length generator

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

    member this.hasChipHasCorrespondingGenerator(chip: Chip) =
        if not (this.chips |> List.contains chip) then
            failwith "fail"

        this.generators |> List.contains chip

    member this.GetCount() =
        this.chips.Length + this.generators.Length

    override this.GetHashCode() : int =
        let chipHash =
            this.chips
            |> List.sortDescending
            |> List.fold (fun acc chip -> (acc * 31) + chip.GetHashCode()) 0

        let generatorHash =
            this.generators
            |> List.sortDescending
            |> List.fold (fun acc generator -> (acc * 31) + generator.GetHashCode()) 0

        (chipHash * 397) ^^^ generatorHash

    override this.Equals other = failwith "F"

    member this.DeepCopy() =
        { chips = this.chips |> List.ofSeq
          generators = this.generators |> List.ofSeq }

let canMoveChip (mc: Chip) (target: Configuration) =
    ((target.generators |> Seq.contains mc)
     || (target.generators |> Seq.forall (fun gen -> (target.chips |> Seq.contains gen))))

let canMoveGenerator (gen: Generator) (target: Configuration) =
    (target.chips |> Seq.contains gen)
    || (target.generators |> Seq.forall (fun gen -> (target.chips |> Seq.contains gen)))

let canBeMoved (target: Configuration) =
    function
    | C c -> canMoveChip c target
    | G g -> canMoveGenerator g target
    | CC(c1, c2) -> canMoveChip c1 target && canMoveChip c2 target
    | GG(g1, g2) -> canMoveGenerator g1 target && canMoveGenerator g2 target
    | CG(c, g) -> (c = g) || (canMoveChip c target && canMoveGenerator g target)

let rec makePairs x acc =
    function
    | [] -> acc
    | head :: tail -> makePairs x (acc @ [ (x, head) ]) tail

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

    override this.Equals other = failwith "F"

    member this.ConfigurationDeepCopy() =
        let (Layout(floor, configurations)) = this
        configurations |> Array.map (_.DeepCopy())

let search (layout: Layout) =
    let pQueue = PriorityQueue()
    pQueue.Enqueue(layout, 0)

    let visited = Dictionary<int, int>()
    visited.Add(layout.GetHashCode(), 0)

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

        let highestFloorReached = cfgs |> Seq.findIndexBack (fun cfg -> cfg.GetCount() > 0)
        let areAllItemsOnHighestFloor = cfgs[maxFloor].GetCount() = totalItemCount

        if areAllItemsOnHighestFloor then
            printfn $"Found - %A{currentCost}"
        else

            let cMoves = cfgs[floor].chips |> Seq.map C
            let gMoves = cfgs[floor].generators |> Seq.map G

            let ccMoves =
                seq {
                    for c1 in cfgs[floor].chips do
                        for c2 in cfgs[floor].chips do
                            if c1 <> c2 then
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

            if floor - 1 >= startFloor then
                floorsToVisit.Add(floor - 1)

            if floor + 1 <= maxFloor then
                floorsToVisit.Add(floor + 1)

            let allMoves =
                cMoves
                |> Seq.append gMoves
                |> Seq.append cgMoves
                |> Seq.append ggMoves
                |> Seq.append ccMoves

            for move in allMoves do
                for nextFloor in floorsToVisit do
                    let targetCfg = cfgs[nextFloor]

                    if canBeMoved targetCfg move then
                        let newCfg = current.ConfigurationDeepCopy()
                        executeMove newCfg[floor] newCfg[nextFloor] move
                        let newLayout = Layout(nextFloor, newCfg)
                        let newCost = currentCost + 1

                        let newLayoutHash = newLayout.GetHashCode()

                        let alreadyVisited = visited.ContainsKey(newLayoutHash)

                        if not alreadyVisited || newCost < visited[newLayoutHash] then

                            if alreadyVisited then
                                visited[newLayoutHash] <- newCost
                            else
                                visited.Add(newLayoutHash, newCost)

                            let ranking =
                                (newCfg
                                 |> Seq.mapi (fun i el -> (i, el))
                                 |> Seq.fold (fun acc (i, cfg) -> acc + (((i + 1) * 10) * cfg.GetCount())) 0)

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
                    |> Seq.toList

                let generators =
                    split
                    |> Seq.where (fun (i, _) -> split.Length > i + 1 && (snd split[i + 1]).StartsWith("generator"))
                    |> Seq.map snd
                    |> Seq.toList

                { chips = chips
                  generators = generators })
            |> Seq.toArray
        )

    search layout
