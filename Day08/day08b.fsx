open System.IO

let filename = "Day08/day08.data"

let parseSegments (str: string) =
    str.Trim().Split(' ')
    |> Array.map(fun s -> s.Trim().ToCharArray() |> Set.ofArray)

let parseLine (line: string) =
    match line.Split('|') with
    | [| input ; output |] -> (parseSegments input, parseSegments output)
    | _ -> failwithf "Unable to parse line: %s" line

let data =
    File.ReadAllLines(filename)
    |> Array.map (fun line -> line, parseLine line)

let findMapping (input: Set<char>[]) =
    let one = input |> Array.find (fun seg -> (Set.count seg) = 2)
    let four = input |> Array.find (fun seg -> (Set.count seg) = 4)
    let seven = input |> Array.find (fun seg -> (Set.count seg) = 3)
    let eight = input |> Array.find (fun seg -> (Set.count seg) = 7)
    let three =
        input |> Array.find (fun seg -> (Set.count seg) = 5 && Set.isSubset seven seg)
    let nine =
        input |> Array.find (fun seg -> (Set.count seg) = 6 && Set.isSubset three seg)
    let bottomLeft = eight - nine
    let two =
        input |> Array.find (fun seg -> (Set.count seg) = 5 && seg <> three && Set.isSubset bottomLeft seg)
    let five =
        input |> Array.find (fun seg -> (Set.count seg) = 5 && seg <> three && seg <> two)
    let six =
        input |> Array.find (fun seg -> (Set.count seg) = 6 && Set.isSubset five seg && seg <> nine)
    let zero =
        input |> Array.find (fun seg -> (Set.count seg) = 6 && seg <> six && seg <> nine)
    [ (zero, 0) ; (one,1) ; (two,2) ; (three, 3) ; (four, 4) ; (five, 5) ; (six, 6) ; (seven, 7) ; (eight, 8) ; (nine, 9) ]
    |> Map.ofList

let getDigits input output =
    let mapping = findMapping input
    let outputDigits =
        output
        |> Array.map (fun o ->
            try
                mapping |> Map.find o |> string
            with :? System.Collections.Generic.KeyNotFoundException as ex ->
                printfn "Unable to find: %A" o
                reraise()
            )
    System.String.Join("", outputDigits)

let result =
    data
    |> Array.map (fun (line,(input,output)) ->
        int <| getDigits input output
        )
    |> Array.sum

printfn "Result: %i" result
