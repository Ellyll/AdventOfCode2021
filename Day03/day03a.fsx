open System

let filename = "Day03/day03.data"

let lines = IO.File.ReadAllLines(filename)
let width = String.length(lines.[0])

let data =
    lines
    |> Array.map (fun line -> Convert.ToInt32(line, 2))

let getValueAtPosition (input : int) (position: int) =
    (input >>> position) &&& 1

let countsOfOnes =
    data
    |> Array.fold (fun cs x ->
        cs
        |> Array.mapi (fun i c ->
            c + int (getValueAtPosition x i)
        )
    ) (Array.zeroCreate<int> width)

let gamma =
    countsOfOnes
    |> Array.fold (fun (position,g) ones -> 
        let zeros = (Array.length data) - ones
        let g' =
            if ones > zeros then
                g + int ( 2.0 ** (float position) )
            else
                g
        position + 1, g'        
        ) (0,0)
    |> snd

let epsilon =
    int ( 2.0 ** (float width) ) - 1 - gamma

let result = gamma * epsilon

printfn "Gamma: %s" (Convert.ToString(gamma, 2))
printfn "Epsilon: %s" (Convert.ToString(epsilon, 2))
printfn "Result: %i" result
