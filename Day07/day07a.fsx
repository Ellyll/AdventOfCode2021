open System.IO

let filename = "Day07/day07.data"

let data =
    File.ReadAllText(filename).Trim().Split(',')
    |> Array.map (int)

let minPos = data |> Array.min
let maxPos = data |> Array.max

let result =
    seq { minPos .. maxPos }
    |> Seq.map (fun p -> data |> Seq.sumBy (fun n -> abs (n - p)))
    |> Seq.min

printfn "Result: %i" result