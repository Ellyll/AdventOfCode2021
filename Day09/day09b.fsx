open System.IO

let filename = "Day09/day09.data"

let heightmap =
    let data = File.ReadAllLines(filename)
    let width = data[0].Length
    let height = data |> Array.length
    Array2D.init height width (fun l c -> int <| (data[l][c]).ToString())

let getAdjacents l c heightmap =
    [
        let height = heightmap |> Array2D.length1
        let width = heightmap |> Array2D.length2
        for (la,ca) in [ (l-1,c) ; (l,c-1) ; (l,c+1) ; (l+1,c) ] do
            if (la <> l || ca <> c) && la >= 0 && la < height && ca >= 0 && ca < width then
                yield (la,ca)
    ]

let lowpoints =
    [
        for l in 0..((Array2D.length1 heightmap)-1) do
            for c in 0..((Array2D.length2 heightmap)-1) do
                let adjacents = heightmap |> getAdjacents l c |> List.map (fun (la,ca) -> heightmap[la,ca])
                let isLowpoint =
                    adjacents
                    |> List.forall (fun h -> heightmap[l,c] < h)
                if isLowpoint then
                    yield (l,c)
    ]

let getBasin lpL lpC (heightmap: int[,]) =
    let rec loop l c basin =
        if (basin |> Set.contains (l,c)) || heightmap[l,c] = 9 then
            basin
        else
            let basin' = basin |> Set.add (l,c) 
            let adjacents =
                heightmap
                |> getAdjacents l c
                |> List.filter (fun (la,ca) -> heightmap[la,ca] < 9 && not (basin' |> Set.contains(la,ca)))
            adjacents |> List.fold (fun b (la,ca) -> loop la ca b) basin'
    loop lpL lpC Set.empty

let result =
    lowpoints
    |> List.map (fun (lowpL,lowpC) -> heightmap |> getBasin lowpL lowpC |> Set.count)
    |> List.sortDescending
    |> List.take 3
    |> function
        | [ a ; b ; c ] -> a * b *c
        | xs -> failwithf "Unexpected basin sizes: %A" xs

printfn "Result: %i" result
