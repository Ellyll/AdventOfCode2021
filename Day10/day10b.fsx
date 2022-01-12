open System.IO

let filename = "Day10/day10.data"

let data =
    File.ReadAllLines(filename)

type ParseResult =
    | Valid
    | Incomplete of string
    | SyntaxError of char

let removeLastChar (str: string) : string =
    if str.Length > 1 then
        str.[0..(str.Length - 2)]
    else
        ""

let removeFirstChar (str: string) : string =
    if str.Length > 1 then
        str.[1..(str.Length - 1)]
    else
        ""

let opens = [ '(' ; '[' ; '{' ; '<' ] |> Set.ofList
let closed = [ ')' ; ']' ; '}' ; '>' ] |> Set.ofList
let areSameType a b =
    match a,b with
    | '(', ')'
    | '[', ']'
    | '{', '}'
    | '<', '>' -> true
    | _ -> false

let parseLine (line: string) : ParseResult =
    let rec loop remaining stack =
        if String.length remaining = 0 then
            if String.length stack = 0 then
                Valid
            else
                Incomplete stack
        else
            let curr = remaining.[0]

            if Set.contains curr opens then
                let newStack = stack + string curr
                let newRemaining = removeFirstChar remaining
                loop newRemaining newStack
            elif (stack.Length > 0 && Set.contains curr closed && areSameType stack.[stack.Length - 1] curr ) then
                let newStack = removeLastChar stack
                let newRemaining = removeFirstChar remaining
                loop newRemaining newStack
            else
                SyntaxError curr
    loop line System.String.Empty

let incompleted =
    data
    |> Array.choose (fun line ->
        match parseLine line with
        | Incomplete stack -> Some stack
        | _ -> None
        )
    |> Array.map (fun stack ->
            stack
            |> Seq.rev
            |> Seq.map (function
                | '(' -> ')'
                | '[' -> ']'
                | '{' -> '}'
                | '<' -> '>'
                | c -> failwithf "Invalid character: %c" c
                )
            |> Array.ofSeq
            |> System.String
            |> string
        )

let scores =
    incompleted
    |> Array.map (fun str ->
        str
        |> Seq.fold (fun total c ->
            let cScore =
                match c with
                | ')' -> 1L
                | ']' -> 2L
                | '}' -> 3L
                | '>' -> 4L
                | _ -> failwithf "Invalid character: %c" c
            (total * 5L) + cScore
            ) 0L
        )

let result =
    let sorted = scores |> Array.sort
    let middleIndex = ((Array.length sorted) / 2)
    sorted.[middleIndex]

printfn "Result: %i" result
