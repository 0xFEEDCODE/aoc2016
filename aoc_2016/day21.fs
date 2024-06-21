module aoc_2016.day21

open System.IO


type Position = int
type Letter = char

type Direction =
    | Right
    | Left
    
type Instruction =
    | Rotate of Direction * Position
    | RotateBased of Letter
    | Swap of Letter * Letter
    | Reverse of Position * Position
    | Move of Position * Position
    
    member this.Execute (x: string) =
        
        let mutable new_string = x |> Seq.toArray
        
        match this with
        | Rotate(dir, pos) -> new_string
        | RotateBased(lt) -> new_string
        | Swap(lt1, lt2) ->
            new_string |> Array.map(fun ch -> if ch = lt1 then lt2 else if ch = lt2 then lt1 else ch)
        | Reverse(pos1, pos2) ->
            for i in {pos1..(pos2/2)-1} do
                let opposite = pos2 - i
                let temp = new_string[i]
                new_string[i] <- new_string[opposite]
                new_string[opposite] <- temp
            new_string
        | Move(pos1, pos2) ->
            let moving = new_string[pos1]
            for i in {pos1..(pos2-1)} do
                new_string[i] <- new_string[i+1]
            new_string[pos2] <- moving
            new_string
        
    
let parse_position (x: string) = x |> int
let parse_letter (x: string) = match x.Length with | 1 -> x[0] | _ -> failwith "fu"
let parse_direction (x: string) = match x with | "right" -> Right | "left" -> Left | _ -> failwith "fu"
    
let parse_instruction (x: string) =
    let spl = x.Split(" ")
    match spl[0] with
    | "rotate" -> if spl[1] = "based" then RotateBased(parse_letter(spl[6])) else Rotate(parse_direction(spl[1]), parse_position(spl[2]))
    | "move" -> Move(parse_position(spl[2]), parse_position(spl[5]))
    | "reverse" -> Reverse(parse_position(spl[2]), parse_position(spl[4]))
    | "swap" -> Swap(parse_letter(spl[2]), parse_letter(spl[5]))
    
let to_string (x: char array) = x |> Array.map string |> Array.reduce (+)

let solve() =
    let inp = File.ReadLines "day21.txt"
    let to_scramble = "abcdefgh"
    
    let r = Reverse(0, 4).Execute "sosa"
    printfn "%A" r
    exit(0)
    
    let res = inp |> Seq.fold(fun acc line -> ((parse_instruction line).Execute acc) |> to_string) to_scramble
    printfn "%A" res
    
    0
