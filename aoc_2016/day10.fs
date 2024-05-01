module aoc_2016.day10

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions


type Chips = int option * int option
type Bot = int * Chips

let addChip (chips: Chips) (newChip: int option) =
    if newChip.IsNone then
        chips
    else
        match chips with
        | Some _, Some _ -> failwith "WTF"
        | Some(v1), None ->
            if newChip.Value > v1 then
                (Some(v1), newChip)
            else
                (newChip, Some(v1))
        | None, Some(v2) ->
            if newChip.Value > v2 then
                (Some(v2), newChip)
            else
                (newChip, Some(v2))
        | None, None -> (newChip, None)


let solve () =
    let inp = File.ReadAllLines "day10.txt"

    let regex = Regex("[0-9]+")

    let mutable map = Map.empty<int, Chips>

    let botConfData =
        inp
        |> Seq.filter (fun el -> el.Contains "goes")
        |> Seq.map (fun el ->
            let seq =
                regex.Matches(el)
                |> Seq.cast<Match>
                |> Seq.map (fun m -> int m.Value)
                |> Seq.toArray

            (seq[0], seq[1]))

    let instructions =
        inp
        |> Seq.filter (fun el -> el.Contains "gives")
        |> Seq.map (fun el ->
            let seq =
                regex.Matches(el)
                |> Seq.cast<Match>
                |> Seq.map (fun m -> int m.Value)
                |> Seq.toArray

            let lowToBot = el.Contains "low to bot"
            let highToBot = el.Contains "high to bot"
            let low = if lowToBot then seq[1] else seq[1] + 10000
            let high = if highToBot then seq[2] else seq[2] + 10000

            (seq[0], low, high))

    for chip, botN in botConfData do
        if (map |> Map.containsKey botN) then
            map <- (map |> Map.add botN (addChip map[botN] (Some(chip))))
        else
            map <- (map |> Map.add botN (Some(chip), None))

    let q = Queue()

    for botN, low, high in instructions do
        q.Enqueue(botN, low, high)

    while q.Count > 0 do
        let botN, low, high = q.Dequeue()

        if not (map |> Map.containsKey botN) then
            q.Enqueue(botN, low, high)
        else
            let chipLow, chipHigh = map[botN]

            if chipLow = None || chipHigh = None then
                q.Enqueue(botN, low, high)
            else
                let l = 17
                let r = 61

                if chipLow = Some(l) && chipHigh = Some(r) then
                    printfn $"%A{botN} YAP"

                if chipLow = Some(r) && chipHigh = Some(l) then
                    printfn $"%A{botN} YAP"

                if not (map |> Map.containsKey low) then
                    map <- (map |> Map.add low (None, None))

                if not (map |> Map.containsKey high) then
                    map <- (map |> Map.add high (None, None))

                map <- (map |> Map.add low (addChip map[low] chipLow))
                map <- (map |> Map.add high (addChip map[high] chipHigh))
                map <- (map |> Map.add botN (None, None))


    let outputs = map |> Seq.filter (fun el -> el.Key >= 10000)
    printfn "%A" outputs



//printfn "%A" map
