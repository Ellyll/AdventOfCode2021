let filename = "day01.data"
let data =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (int)

let increases =
    data
    |> Array.fold (fun (count,prev) n ->
        match prev with
        | Some p when n > p -> (count + 1, Some n)
        | _ -> (count, Some n)
        ) (0,None)
    |> fst

printfn "Number of increases: %i" increases