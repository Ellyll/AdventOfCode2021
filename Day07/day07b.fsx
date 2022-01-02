open System.IO

let filename = "Day07/day07.data"

let data =
    File.ReadAllText(filename).Trim().Split(',')
    |> Array.map (int)

let minPos = data |> Array.min
let maxPos = data |> Array.max

let calcFuel n =
    // from: https://www.cuemath.com/sum-of-integers-formula/
    n*(n + 1)/2

let result =
    seq { minPos .. maxPos }
    |> Seq.map (fun p -> data |> Seq.sumBy (fun n -> (n - p) |> abs |> calcFuel))
    |> Seq.min

printfn "Result: %i" result