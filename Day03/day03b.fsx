open System

let filename = "Day03/day03.data"

let lines = IO.File.ReadAllLines(filename)
let width = String.length(lines.[0])

let data =
    lines
    |> Array.map (fun line -> Convert.ToInt32(line, 2))

let getValueAtPosition (input : int) (position: int) =
    (input >>> position) &&& 1

let getCountOfOnesAtPosition xs pos =
    xs
    |> Array.fold (fun total x ->
        let value = getValueAtPosition x pos
        if value = 1 then total + 1 else total
        ) 0

let getRating filterOp =
    let rec loop rows position =
        if position < 0 then
            failwithf "Invalid position: %i" position
        // do stuff
        let ones = getCountOfOnesAtPosition rows position
        let zeros = (Array.length rows) - ones
        let filterDigit = if filterOp ones zeros then 1 else 0
        match (rows |> Array.filter (fun r -> (getValueAtPosition r position) = filterDigit)) with
        | [| row |] -> row
        | [| |] -> failwithf "No rows found for digit: %i position: %i" filterDigit position
        | filteredRows -> loop filteredRows (position - 1)
    loop data (width - 1)

let oxygenRating = getRating (>=)
let co2Rating = getRating (<)
let result = oxygenRating * co2Rating

printfn "Oxygen Rating: %s (%i)" (Convert.ToString(oxygenRating, 2)) oxygenRating
printfn "CO2 Rating: %s (%i)" (Convert.ToString(co2Rating, 2)) co2Rating
printfn "Result: %s (%i)" (Convert.ToString(result, 2)) result
