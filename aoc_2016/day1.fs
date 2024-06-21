module aoc_2016.day1

let inp =
    "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3"

[<CustomEquality; NoComparison>]
type Pos =
    { X: int
      Y: int }

    override this.Equals other =
        match other with
        | :? Pos as p -> p.X.Equals this.X && p.Y.Equals this.Y
        | _ -> false

    override this.GetHashCode() = (this.X + this.Y).GetHashCode()

type Line = { start': Pos; end': Pos }

type Dir =
    | N
    | S
    | W
    | E

let printLine line =
    printfn $"S: {(line.start'.X, line.start'.Y)}\nE: {(line.end'.X, line.end'.Y)}"

let linesIntersect (line1: Line, line2: Line) =
    let isLine1Horizontal = line1.start'.X <> line1.end'.X
    let isLine2Horizontal = line2.start'.X <> line2.end'.X

    if
        (isLine1Horizontal && isLine2Horizontal)
        || (not (isLine1Horizontal || isLine2Horizontal))
    then
        None
    else
        let hLine = if isLine1Horizontal then line1 else line2
        let vLine = if isLine1Horizontal then line2 else line1

        let yPos = hLine.start'.Y
        let yLow, yHigh = (min vLine.start'.Y vLine.end'.Y, max vLine.start'.Y vLine.end'.Y)

        let xPos = vLine.start'.X
        let xLow, xHigh = (min hLine.start'.X hLine.end'.X, max hLine.start'.X hLine.end'.X)

        if (xLow < xPos && xPos < xHigh) && (yLow < yPos && yPos < yHigh) then
            Some(vLine.start'.X, yPos)
        else
            None

let solve () =
    let instructions = inp.Split ','

    let result, _, _, _ =
        let scan = instructions |> Seq.scan(fun (_, pos, currDir, visited) item ->
            let instruction = item.Trim()
            let turn = instruction[0]
            let distance = instruction[1..] |> int
            let isLeftTurn = turn = 'L'

            let newDir =
                match currDir with
                | N -> if isLeftTurn then W else E
                | S -> if isLeftTurn then E else W
                | W -> if isLeftTurn then S else N
                | E -> if isLeftTurn then N else S

            let newPos =
                match newDir with
                | N -> { X = pos.X; Y = pos.Y - distance }
                | S -> { X = pos.X; Y = pos.Y + distance }
                | W -> { X = pos.X - distance; Y = pos.Y }
                | E -> { X = pos.X + distance; Y = pos.Y }

            let line = { start' = pos; end' = newPos }

            let newVisited = line :: visited

            let intersectingLine =
                if not (pos.Equals { X = 0; Y = 0 }) then
                    newVisited
                    |> Seq.map (fun item -> linesIntersect (item, line))
                    |> Seq.tryFind (fun item -> item.IsSome)
                else
                    None

            (intersectingLine, newPos, newDir, newVisited)) (None, { X = 0; Y = 0 }, N, []) |> Seq.toArray
        scan |> Seq.find (fun (intersectingLine, _, _, _) -> intersectingLine.IsSome)

    printfn $"%A{(abs (fst result.Value.Value)) + (abs (snd result.Value.Value))}"
