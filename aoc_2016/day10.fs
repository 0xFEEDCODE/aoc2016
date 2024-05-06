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

    let mutable bots = Map.empty<int, Chips>

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

    let instructionsQueue =
        Queue(
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

            (seq[0], low, high)))

    for chip, botN in botConfData do
        if (bots |> Map.containsKey botN) then
            bots <- (bots |> Map.add botN (addChip bots[botN] (Some(chip))))
        else
            bots <- (bots |> Map.add botN (Some(chip), None))
            
    let chipLowTarget = 17
    let chipHighTarget = 61
            
    while instructionsQueue.Count > 0 do
        let botN, low, high = instructionsQueue.Dequeue()
        
        if not (bots |> Map.containsKey botN) then
            instructionsQueue.Enqueue((botN, low, high))
        else
            let chipLow, chipHigh = bots[botN]
                
            if chipLow = None || chipHigh = None then
                instructionsQueue.Enqueue((botN, low, high))
            else
                if chipLow = Some(chipLowTarget) && chipHigh = Some(chipHighTarget) then
                    printfn $"%A{botN} YAP"
                    
                bots <- bots
                        |> Map.add low (addChip (if bots |> Map.containsKey low then bots[low] else (None, None)) chipLow)
                        |> Map.add high (addChip (if bots |> Map.containsKey high then bots[high] else (None, None)) chipHigh)
                        |> Map.add botN (None, None)
                
                    
    let outputs = bots |> Seq.filter (fun el -> el.Key >= 10000)
    printfn "%A" outputs


//printfn "%A" map
