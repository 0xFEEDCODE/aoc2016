module aoc_2016.day3

open System.IO

type Triangle =
    { S1: int
      S2: int
      S3: int }

    member this.isValidTriangle =
        (this.S2 + this.S3) > this.S1
        && (this.S1 + this.S3) > this.S2
        && (this.S1 + this.S2) > this.S3

type Money = int
type X =
    | A of int
    | B of (int * int)
    | C of string

let solve () =
    let inp = File.ReadAllLines("day3.txt")

    let getTriangleSidesFromRow (row: string) =
        row.Split ' ' |> Seq.filter (fun x -> x.Length > 0) |> Seq.toArray

    let processRowIntoTriangle (row: string) =
        let split = getTriangleSidesFromRow row

        { S1 = split[0] |> int
          S2 = split[1] |> int
          S3 = split[2] |> int }

    let processChunkOf3RowsIntoTriangle (row1: string, row2: string, row3: string) =
        seq {
            for i in 0..2 do
                let split1 = getTriangleSidesFromRow row1
                let split2 = getTriangleSidesFromRow row2
                let split3 = getTriangleSidesFromRow row3

                yield
                    { S1 = split1[i] |> int
                      S2 = split2[i] |> int
                      S3 = split3[i] |> int }
        }

    let nTriangles1 =
        inp
        |> Seq.fold
            (fun acc row ->
                let triangle = processRowIntoTriangle row
                acc + (if triangle.isValidTriangle then 1 else 0))
            0

    let nTriangles2 =
        inp
        |> Seq.chunkBySize 3
        |> Seq.fold
            (fun acc chunk ->
                let triangles =
                    processChunkOf3RowsIntoTriangle (chunk[0], chunk[1], chunk[2])
                    |> Seq.filter (_.isValidTriangle)

                acc + (triangles |> Seq.length))
            0

    printfn "%A" nTriangles1
    printfn "%A" nTriangles2

    let fn a b = a + b
    let add1 n = fn n 1
    let add2 n = fn n 2

    printfn "%A" (add1 5)
    printfn "%A" (add2 5)
    
    let a = A 5
    let b = B (5, 2)
    let c = C "B"


    ()


//printfn "%A" (triangles2 |> Seq.length)
