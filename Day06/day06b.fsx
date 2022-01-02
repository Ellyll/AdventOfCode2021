open System.IO

let filename = "Day06/day06.data"

let data =
    File.ReadAllText(filename).Trim().Split(',')
    |> Array.map (int)
    |> Array.countBy (id)
    |> Array.map (fun (a,b) -> a, int64 b)
    |> Map.ofArray

let processDay fishies =
    fishies
    |> Map.fold (fun state k v ->
        if k = 0 then
            state
            |> Map.change 6 (fun oldn -> Some <| (oldn |> Option.defaultValue 0L) + v)
            |> Map.add 8 v
        else
            state |> Map.change (k - 1) (fun oldn -> Some <| (oldn |> Option.defaultValue 0L) + v)
        ) Map.empty

let rec processDays n fishies =
    if n = 0 then
        fishies
    else
        processDays (n-1) (processDay fishies)

let fishies = processDays 256 data
let result = fishies |> Map.fold (fun state _ v -> state + v) 0L
printfn "Result: %i" result