open System.IO

let filename = "Day06/day06.data"

let data =
    File.ReadAllText(filename).Trim().Split(',')
    |> Array.map (int)

let processDay fishies =
    fishies
    |> Seq.fold (fun (updatedFish) fish ->
            if fish = 0 then
                6::8::updatedFish
            else
                (fish - 1)::updatedFish
        ) ([])

let rec processDays n (fishies: int seq) =
    if n = 0 then
        fishies
    else
        processDays (n-1) (processDay fishies)

let result = processDays 80 data |> Seq.length
printfn "Result: %i" result