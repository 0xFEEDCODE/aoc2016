module aoc_2016.day25

open System.Collections.Generic
open aoc_2016.day23

let inp = "cpy a d
cpy 4 c
cpy 633 b
inc d
dec b
jnz b -2
dec c
jnz c -5
cpy d a
jnz 0 0
cpy a b
cpy 0 a
cpy 2 c
jnz b 2
jnz 1 6
dec b
dec c
jnz c -4
inc a
jnz 1 -7
cpy 2 b
jnz c 2
jnz 1 4
dec b
dec c
jnz 1 -4
jnz 0 0
out b
jnz a -19
jnz 1 -21"

type Registers = Dictionary<char, int>

type Reg = char
type Val = int
type RegOrValue =
    | Register of Reg
    | Value of Val
    
type InstructionPointer = int

let mutable last_signal = 1
let mutable signals = ""
let mutable stop_flag = 0

type Instruction =
    | DEC of Reg
    | INC of Reg
    | CPY of RegOrValue * RegOrValue
    | JNZ of RegOrValue * RegOrValue
    | TGL of Reg
    | OUT of RegOrValue
    
    member this.GetToggledVersion () =
        match this with
        | DEC x -> INC x
        | INC x -> DEC x
        | JNZ(x,y) -> CPY(x,y)
        | CPY(x,y) -> JNZ(x, y)
        | TGL(x) -> INC x
    
    member this.Execute (instructions: Instruction array) (registers: Registers) (ip: InstructionPointer) =
        let get_reg_value (x: Reg) =  if (registers.ContainsKey x) then registers[x] else 0
        let get_value (x: RegOrValue) = match x with | Register(r) -> get_reg_value r | Value(v) -> v
        let get_reg (x: RegOrValue) = match x with | Register(r) -> Some(r) | Value(_) -> None
        
        match this with
        | DEC x ->
            registers[x] <- get_reg_value x - 1
            (ip + 1, instructions)
        | INC x ->
            registers[x] <- get_reg_value x + 1
            (ip + 1, instructions)
        | CPY (x, y) ->
            match (x, y) with
            | Register _, Register y -> registers[y] <- get_value x
            | Value _, Register y -> registers[y] <- get_value x
            | _ -> ()
            (ip + 1, instructions)
        | JNZ (x, y) ->
            (ip + (if (get_value x) <> 0 then (get_value y) else 1), instructions)
        | OUT x ->
            let v = get_value x
            
            if last_signal = v then
                stop_flag <- 1
            last_signal <- v
            
            signals <- signals + v.ToString() + ","
            
            if signals.Length > 2000 then
                (ip + 1000, instructions)
            else
                (ip + 1, instructions)
        | TGL(x) ->
            let v = get_reg_value x
            if (ip+v < instructions.Length-1)  then
                let mutable updated_instructions = instructions
                updated_instructions[ip+v] <- updated_instructions[ip+v].GetToggledVersion()
                (ip + 1, updated_instructions)
            else
                (ip + 1, instructions)
        

let solve() =
    let mutable registers = Registers()
    
    let parse_reg (x: string) : Reg = x[0]
    let parse_val (x: string) : Val = (x |> int)
    let parse_regOrValue (x: string) : RegOrValue =
        if x[0] <> '-' && x[0] < '0' || x[0] > '9' then Register(parse_reg x) else Value(parse_val x)
        
    let instructions = inp.Split("\n") |> Seq.map(fun line ->
        let spl = line.Split " "
        match spl[0] with
        | "inc" -> INC(parse_reg(spl[1]))
        | "dec" -> DEC(parse_reg(spl[1]))
        | "cpy" -> CPY(parse_regOrValue(spl[1]), parse_regOrValue(spl[2]))
        | "jnz" -> JNZ(parse_regOrValue(spl[1]), parse_regOrValue(spl[2]))
        | "out" -> OUT(parse_regOrValue(spl[1]))
        | "tgl" -> TGL(parse_reg(spl[1])))
                       |> Seq.toArray
                       
    let mutable i = 0
    while i <> -1 do
        //printfn $"\nTrying for %A{i} \n"
        registers <- Registers()
        stop_flag <- 0
        last_signal <- 1
        signals <- ""
        
        registers['a'] <- i
        
        let process_instructions() =
            let rec proc (ip: int) (instr: Instruction array) =
                
                if ip > (instr.Length-1) then
                    printfn "%A" i
                    i <- -2
                    ()
                else
                    let ip, updated_instr = instr[ip].Execute instr registers ip
                    if stop_flag <> 1 then
                        proc ip updated_instr
                        
            proc 0 instructions
            
        process_instructions()
        i <- i+1
    
    //printfn $"%A{registers}"
    0

