let filename = "day01.data"
let data =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (int)

let increases : int =
    data
    |> Array.skip 3
    |> Array.fold (fun (count, (prev1, prev2, prevTotal)) n ->
        let thisTotal = prev1 + prev2 + n
        let newCount =
            if thisTotal > prevTotal then
                count + 1
            else
                count
        (newCount, (prev2, n, thisTotal))
        ) (0, (data.[1], data.[2], (data.[0] + data.[1] + data.[2])))
    |> fst

printfn "Number of increases: %i" increases