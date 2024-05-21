module aoc_2016.day11Visualizer

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text


type Chip = string
type Generator = string

type MoveConfiguration =
    | C of Chip
    | G of Generator
    | CC of Chip * Chip
    | GG of Generator * Generator
    | CG of Chip * Generator

type Configuration =
    { mutable chips: Chip array
      mutable generators: Generator array }

    member this.moveChip (chip: Chip) (target: Configuration) =
        if not (Array.contains chip this.chips) then
            failwith "fail"

        this.chips <- Array.filter ((<>) chip) this.chips
        target.chips <- Array.append target.chips [| chip |]

    member this.moveGenerator (generator: Generator) (target: Configuration) =
        if not (Array.contains generator this.generators) then
            failwith "fail"

        this.generators <- Array.filter ((<>) generator) this.generators
        target.generators <- Array.append target.generators [| generator |]

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

    member this.hasChipGotCorrespondingGenerator(ch: Chip) = this.generators |> Array.contains ch

    member this.hasGeneratorGotCorrespondingChip(gen: Generator) = this.chips |> Array.contains gen

    member this.GetCount() =
        this.chips.Length + this.generators.Length

    member this.IsAnythingFried() =
        not (Array.isEmpty this.generators) && not (this.chips |> Array.forall this.hasChipGotCorrespondingGenerator)

    member this.AsStringFormatted() =
        let mutable sb = StringBuilder("Chips: ")

        if this.chips.Length > 0 then
            this.chips |> Seq.iter (fun ch -> sb <- sb.Append $"{ch};")
        else
            sb <- sb.Append "NONE"

        sb <- sb.Append " || Generators: "

        if this.generators.Length > 0 then
            this.generators |> Seq.iter (fun gen -> sb <- sb.Append $"{gen};")
        else
            sb <- sb.Append "NONE"

        sb.ToString()

    member this.DeepCopy() =
        { chips = Array.copy this.chips
          generators = Array.copy this.generators }

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

    member this.GetDeltasAsString() =
        let (Layout(floor, configurations)) = this

        let deltas =
            configurations
            |> Array.mapi (fun i el -> (i, el))
            |> Array.map (fun (floor, config) ->
                config.chips
                |> Array.map (fun ch ->
                    (configurations |> Seq.findIndex (fun cfg -> cfg.generators |> Array.contains ch)) - floor)
                |> Array.sortBy id
                )

        let hashDeltas (deltas: int array array) (floor: int) =
            deltas 
            |> Array.map (fun arr -> arr |> Array.fold( fun acc h -> acc + h.ToString()) " ")
            |> Array.fold(fun acc h -> (acc + h)) (floor.ToString())
            
        (hashDeltas deltas floor)


    override this.Equals(other: obj) =
        match other with
        | :? Layout as otherLayout ->
            let (Layout(floor1, configurations1)) = this
            let (Layout(floor2, configurations2)) = otherLayout

            floor1 = floor2 && Array.forall2 (=) configurations1 configurations2
        | _ -> false


    member this.AsStringFormatted() =
        let (Layout(_, configurations)) = this

        let mutable sb = StringBuilder()

        for i in 0 .. configurations.Length - 1 do
            sb <-
                sb.Append(
                    configurations[configurations.Length - 1 - i].AsStringFormatted()
                    + (if i < configurations.Length - 1 then "\n" else "")
                )

        sb.ToString()

    member this.ConfigurationDeepCopy() =
        let (Layout(floor, configurations)) = this
        configurations |> Array.map (_.DeepCopy())


let mutable searchResult: string array = Array.empty

let search (layout: Layout) =
    seq {
        let pQueue = PriorityQueue()
        pQueue.Enqueue(layout, 0)

        let cameFrom = Dictionary<Layout, Option<Layout>>()

        let visited = Dictionary<string, int>()
        visited.Add(layout.GetDeltasAsString(), 0)
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

            let currentHash = current.GetDeltasAsString()
            let currentCost = visited[currentHash]

            if currentCost > i then
                i <- currentCost
                yield i

            let areAllItemsOnHighestFloor = cfgs[maxFloor].GetCount() = totalItemCount

            if areAllItemsOnHighestFloor then
                let mutable step = currentCost - 1

                searchResult <- searchResult |> Array.append [| current.AsStringFormatted() |]

                let mutable cfg = cameFrom[current]

                while cfg.IsSome do
                    searchResult <- searchResult |> Array.append [| cfg.Value.AsStringFormatted() |]
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

                if floorDown >= startFloor then
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

                        if
                            not (newCfg[floor].IsAnythingFried())
                            && not (newCfg[nextFloor].IsAnythingFried())
                        then

                            let newLayout = Layout(nextFloor, newCfg)
                            let newCost = currentCost + 1

                            let newLayoutHash = newLayout.GetDeltasAsString()

                            let alreadyVisited = visited.ContainsKey(newLayoutHash)

                            if not alreadyVisited || newCost < visited[newLayoutHash] then

                                if not alreadyVisited then
                                    visited.Add(newLayoutHash, newCost)

                                let mutable enableRanking = false

                                let ranking =
                                    if enableRanking then
                                        (newCfg
                                         |> Seq.mapi (fun i el -> (i, el))
                                         |> Seq.fold
                                             (fun acc (floor, cfg) ->
                                                 let dist = maxFloor - floor
                                                 acc + dist)
                                             0)
                                    else
                                        0

                                cameFrom[newLayout] <- Some(current)
                                pQueue.Enqueue(newLayout, newCost + ranking)
    }

let solve (inp: string array) =

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
