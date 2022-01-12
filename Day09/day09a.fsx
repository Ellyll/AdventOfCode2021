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
        for la in (l-1)..(l+1) do
            for ca in (c-1)..(c+1) do
                if (la <> l || ca <> c) && la >= 0 && la < height && ca >= 0 && ca < width then
                    yield heightmap[la,ca]
    ]

let lowpoints =
    [
        for l in 0..((Array2D.length1 heightmap)-1) do
            for c in 0..((Array2D.length2 heightmap)-1) do
                let adjacents = heightmap |> getAdjacents l c
                let isLowpoint =
                    adjacents
                    |> List.forall (fun h -> heightmap[l,c] < h)
                if isLowpoint then
                    yield heightmap[l,c]
    ]

let result =
    lowpoints |> List.sumBy (fun lp -> lp + 1)

printfn "Result: %i" result
