open System.IO
open System.Text.RegularExpressions

let filename = "Day13/day13.data"

let points, folds =
    let pointReg = Regex @"^(\d+),(\d+)$"
    let foldReg = Regex @"^fold along (x|y)=(\d+)$"
    File.ReadAllLines(filename)
    |> Array.fold (fun (ps,fs) line ->
        if pointReg.IsMatch line then
            // Add point
            match line.Split(',') with
            | [| a ; b |] -> ((ps |> Set.add (System.Int32.Parse(a),System.Int32.Parse(b))), fs)
            | _ -> failwithf "Unable to parse point %s" line
        elif foldReg.IsMatch line then
            // Add fold
            let axis = foldReg.Replace(line, "$1")
            let value = System.Int32.Parse(foldReg.Replace(line, "$2"))
            (ps, (axis,value)::fs)
        elif String.length line = 0 then
            (ps,fs)
        else
            failwithf "Unable to parse: %s" line
        ) (Set.empty, List.empty)
    |> function (ps,fs) -> (ps, fs |> List.rev)

let fold (axis,value) points =
    match axis with
    | "x" ->
        points
        |> Set.map (fun (x,y)-> if x < value then ((x + 2*(value - x)),y) else (x,y) )
    | "y" ->
        points
        |> Set.map (fun (x,y)-> if y > value then (x, (y - 2*(y - value))) else (x,y) )
    | _ -> failwithf "Invalid axis: %s" axis

let foldedPoints =
    let firstFold = folds |> List.head
    points |> fold firstFold

let result = foldedPoints |> Set.count
printfn "Result: %i" result
