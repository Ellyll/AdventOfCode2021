open System.IO

let filename = "Day08/day08.data"

let data =
    File.ReadAllLines(filename)
    |> Array.map (fun line ->
        match line.Split('|') with
        | [| input ; output |] -> (input.Trim(), output.Trim())
        | _ -> failwithf "Unable to parse line: %s" line
        )

let uniqeLengths = [ 2; 4; 3; 7 ] |> Set.ofList

let result =
    data
    |> Array.map (snd)
    |> Array.sumBy (fun output ->
        output.Split(' ')
        |> Array.sumBy(fun o -> if Set.contains o.Length uniqeLengths then 1 else 0)
        )

printfn "Result: %i" result
