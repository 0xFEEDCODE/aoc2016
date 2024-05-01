module aoc_2016.day9

open System.Diagnostics
open System.IO

type Instruction = int * int

let getInstructionFromMarker (marker: string) : Instruction =
    let split = marker.Split 'x'
    (split[0] |> int, split[1] |> int)

let processInp (inp: string) =
    let rec loop isParsingInstruction tempAcc resultAcc =
        function
        | [] -> resultAcc
        | '(' :: tail -> loop true "" resultAcc tail
        | ')' :: tail ->
            let len, by = getInstructionFromMarker tempAcc
            let decodedSeq = tail[.. len - 1] |> Seq.replicate by |> Seq.collect id

            let newResultAcc =
                resultAcc + (decodedSeq |> Seq.fold (fun acc el -> acc + string el) "")

            loop false "" newResultAcc tail[len..]
        | head :: tail ->
            let newTempAcc =
                if isParsingInstruction then
                    tempAcc + string head
                else
                    tempAcc

            let newResAcc =
                if not isParsingInstruction then
                    resultAcc + string head
                else
                    resultAcc

            loop isParsingInstruction newTempAcc newResAcc tail

    loop false "" "" (inp |> Seq.toList)

let rec processInp2 (inp: string) : uint64 =
    let rec loop (isParsingInstruction: bool) (tempAcc: string) (resultAcc: uint64) =
        function
        | [] -> resultAcc
        | '(' :: tail -> loop true "" resultAcc tail
        | ')' :: tail ->
            let len, by = getInstructionFromMarker tempAcc
            let newSeq = tail[.. len - 1] |> Seq.fold (fun acc ch -> acc + string ch) ""

            let newResultAcc =
                resultAcc
                + ((if newSeq.Contains '(' then
                        processInp2 newSeq
                    else
                        uint64 len)
                   * uint64 by)

            loop false "" newResultAcc tail[len..]
        | head :: tail ->
            let newTempAcc =
                if isParsingInstruction then
                    tempAcc + string head
                else
                    tempAcc

            let newResAcc =
                if not isParsingInstruction then
                    resultAcc + 1UL
                else
                    resultAcc

            loop isParsingInstruction newTempAcc newResAcc tail

    loop false "" 0UL (inp |> Seq.toList)

let solve () =
    let inp = File.ReadAllLines "day9.txt"

    let t = Stopwatch()
    t.Start()
    let res = inp |> Seq.fold (fun acc item -> acc + (processInp item).Length) 0
    t.Stop()
    printfn "%A" (res, t.ElapsedTicks)

    t.Reset()
    t.Start()
    let res2 = inp |> Seq.map processInp2 |> Seq.sum
    t.Stop()
    printfn "%A" (res2, t.ElapsedTicks)

    0
