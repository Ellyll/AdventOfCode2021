open System.IO

let filename = "Day10/day10.data"

let data =
    File.ReadAllLines(filename)

type ParseResult =
    | Valid
    | Incomplete
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
                Incomplete
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

let result =
    data
    |> Array.map parseLine
    |> Array.sumBy (function
        | SyntaxError ')' -> 3
        | SyntaxError ']' -> 57
        | SyntaxError '}' -> 1197
        | SyntaxError '>' -> 25137
        | _ -> 0)

printfn "Result: %i" result
