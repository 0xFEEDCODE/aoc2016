module aoc_2016.exp

type NotZeroVal =
    | Zero
    | NotZero of int
    
let Bind x f = match x with
                | NotZero v -> f v
                | Zero -> x
                
                
let (>>=) x f = Bind x f
let Return x = if x <> 0 then NotZero x else Zero

let add5 x = Return (x + 5)

module Opt =
    let ret x = Some(x)
    let r x = Some(x)
    let apply f x =
        match f, x with
        | Some f, Some x -> Some (f x)
        | _ -> None
    let map f x = match x with
                    | Some x -> Some(f x)
                    | _ -> None
                    
                    
    let (<!>) = map
    let (<*>) = apply
    
    let lift2 f x y = f <!> x <*> y
    
    let ex() =
        (fun x y -> x + y) <!> (Some 2) <*> (Some 3)
        lift2 (-) (Some 3) (Some 5)
        
module L =
    let apply (fList: ('a -> 'b) list) (xList: 'a list) = [for f in fList do for x in xList do yield f x]
    
    let (<!>) = List.map
    let (<*>) = apply
    
    let lift2 f x y = f <!> x <*> y
    
    let add x y = x + y
    let mul x y = x * y
    
    let ex() =
        (+) <!> [5;10] <*> [2;3]
        lift2 (+) [5;10] [2;3]
        
type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | Some x -> f x
        | None -> None
        
    member this.(>>=) x f = this.Bind(x, f)
        
    member this.Return x = Some(x)

let solve() =
    let a = Return 2
    let c = (Return 2) >>= add5 >>= add5
    
    let r = Opt.ex()
    printfn $"%A{r}"
    let r = L.ex()
    
    let maybe = MaybeBuilder()
    let add x y = if(x <> 0 && y <> 0) then Some(x + y) else None
    //let r = (maybe.(>>=) (fun x -> add x 5) (Some 5)) |> maybe.(>>=) (fun x -> add x 1)
    
    let addN init x y z = maybe{
            let! a = init |> add x
            let! b = a |> add y
            let! c = b |> add z
            return c
        }
    
    let (>>=) x f = maybe.Bind(x, f)
    let addN2 init x y z = (Some(init) >>= (add x)) >>= add y >>= add z
    
    printfn $"%A{addN 1 1 1 3}"
    printfn $"%A{addN2 1 1 5 3}"
    0