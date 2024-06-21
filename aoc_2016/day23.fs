module aoc_2016.day23

open System
open System.Collections.Generic
open aoc_2016.day21

let inp = "cpy a b
dec b
cpy a d
cpy 0 a
cpy b c
inc a
dec c
jnz c -2
dec d
jnz d -5
dec b
cpy b c
cpy c d
dec d
inc c
jnz d -2
tgl c
cpy -16 c
jnz 1 c
cpy 71 c
jnz 75 d
inc a
inc d
jnz d -2
inc c
jnz c -5"

type Registers = Dictionary<char, int>

type Reg = char
type Val = int
type RegOrValue =
    | Register of Reg
    | Value of Val
    
type InstructionPointer = int

type Instruction =
    | DEC of Reg
    | INC of Reg
    | CPY of RegOrValue * RegOrValue
    | JNZ of RegOrValue * RegOrValue
    | TGL of Reg
    
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
        | TGL(x) ->
            let v = get_reg_value x
            if (ip+v < instructions.Length-1)  then
                let mutable updated_instructions = instructions
                updated_instructions[ip+v] <- updated_instructions[ip+v].GetToggledVersion()
                (ip + 1, updated_instructions)
            else
                (ip + 1, instructions)
        

let solve() =
    let registers = Registers()
    
    let parse_reg (x: String) : Reg = x[0]
    let parse_val (x: String) : Val = (x |> int)
    let parse_regOrValue (x: String) : RegOrValue =
        if x[0] <> '-' && x[0] < '0' || x[0] > '9' then Register(parse_reg x) else Value(parse_val x)
        
    let instructions = inp.Split("\n") |> Seq.map(fun line ->
        let spl = line.Split " "
        match spl[0] with
        | "inc" -> INC(parse_reg(spl[1]))
        | "dec" -> DEC(parse_reg(spl[1]))
        | "cpy" -> CPY(parse_regOrValue(spl[1]), parse_regOrValue(spl[2]))
        | "jnz" -> JNZ(parse_regOrValue(spl[1]), parse_regOrValue(spl[2]))
        | "tgl" -> TGL(parse_reg(spl[1])))
                       |> Seq.toArray
                       
    let base_n = 12
    registers['a'] <- base_n
    
    let process_instructions() =
        let rec proc (ip: int) (instr: Instruction array) =
            //printfn "%A" (instructions[ip], ip)
            if ip > (instr.Length-1) then
                ()
            else
                let a_before = registers['a']
                let ip, updated_instr = instr[ip].Execute instr registers ip
                let a_after = registers['a']
                if (a_after < a_before) then
                    printfn "%A" (a_before, a_after, ip)
                proc ip updated_instr
        proc 0 instructions
        
    let calc x y = (x*y) - x
    let start_n = base_n
    let r = seq {0..(start_n-2)} |> Seq.scan(fun acc i -> calc acc (start_n - i)) base_n
    r |> Seq.iter(printfn "%A")
    let ans = (r |> Seq.last) + 5325
    printfn $"%A{ans}"
        
    process_instructions()
    
    printfn $"%A{registers}"
    0

