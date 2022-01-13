open System.IO

let filename = "Day11/day11.data"

let octopuses =
    let data = File.ReadAllLines(filename)
    let width = data[0].Length
    let height = data |> Array.length
    Array2D.init height width (fun l c -> int <| (data[l][c]).ToString())

let foldi (folder: 'TState -> int -> int -> 'T -> 'TState) (state: 'TState) (arr: 'T[,]) =
    let indexes = seq {
        for l in 0..((Array2D.length1 arr)-1) do
            for c in 0..((Array2D.length2 arr)-1) -> (l,c)
        }
    indexes
    |> Seq.fold (fun currState (l,c) -> folder currState l c arr[l,c]) state

let tryFindIndexi (predicate: int -> int -> 'T -> bool) (array: 'T[,]) =
    let indexes = seq {
        for l in 0..((Array2D.length1 array)-1) do
            for c in 0..((Array2D.length2 array)-1) -> (l,c)
        }
    indexes
    |> Seq.tryFind (fun (l,c)-> predicate l c array[l,c])

let getAdjacents l c array =
    [
        let height = array|> Array2D.length1
        let width = array |> Array2D.length2
        for la in (l-1)..(l+1) do
            for ca in (c-1)..(c+1) do
                if (la <> l || ca <> c) && la >= 0 && la < height && ca >= 0 && ca < width then
                    yield (la,ca)
    ] |> Set.ofList

(*
- First, the energy level of each octopus increases by 1.
- Then, any octopus with an energy level greater than 9 flashes. This
    increases the energy level of all adjacent octopuses by 1, including
    octopuses that are diagonally adjacent. If this causes an octopus to
    have an energy level greater than 9, it also flashes. This process
    continues as long as new octopuses keep having their energy level
    increased beyond 9. (An octopus can only flash at most once per step.)
- Finally, any octopus that flashed during this step has its energy
    level set to 0, as it used all of its energy to flash.
*)
let runStep octopuses =
    let rec loop flashed (octos: int[,]) =

        // Find first one with energy level > 9 that hasn't already flashed
        // if none, we're done, else
        // add to flashed set
        // find it's adjacents and increase their energy levels by 1
        tryFindIndexi (fun l c energy -> energy > 9 && not (Set.contains (l,c) flashed)) octos
        |> function
            | None -> flashed,octos
            | Some (l,c) ->
                let adjacents = getAdjacents l c octos
                let octos' = Array2D.copy octos
                for (la,ca) in adjacents do
                    octos'.[la,ca] <- octos'.[la,ca] + 1
                let flashed' = Set.add (l,c) flashed
                loop flashed' octos'

    let flashed', octopuses' = loop Set.empty (octopuses |> Array2D.map (fun n -> n+1))
    let octopuses'' = octopuses' |> Array2D.mapi (fun l c energy -> if Set.contains (l,c) flashed' then 0 else energy)
    ((Set.count flashed'), octopuses'')

let result =
    let flashesForAll = (Array2D.length1 octopuses) * (Array2D.length2 octopuses)
    let rec loop step octos =
        let flashes, octos' = runStep octos
        if flashes = flashesForAll then
            step
        else
            loop (step + 1) octos'
    loop 1 octopuses

printfn "Result: %i" result
