open System.IO
open System.Text.RegularExpressions

let filename = "Day13/day13.data"
let resultFile = "Day13/day13.results.txt"

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
        |> Set.map (fun (x,y)-> if x > value then ( value - ((abs (x-value))), y) else (x,y) )
    | "y" ->
        points
        |> Set.map (fun (x,y)-> if y > value then (x, (value - (abs (y-value)))) else (x,y) )
    | _ -> failwithf "Invalid axis: %s" axis

let fprintPoints stream ps =
    let minx = ps |> Set.map (fst) |> Set.minElement
    let maxx = ps |> Set.map (fst) |> Set.maxElement
    let miny = ps |> Set.map (snd) |> Set.minElement
    let maxy = ps |> Set.map (snd) |> Set.maxElement
    fprintfn stream "minx: %i, maxx: %i" minx maxx
    fprintfn stream "miny: %i, maxy: %i" miny maxy
    for y in miny..maxy do
        fprintf stream "|"
        for x in minx..maxx do
            fprintf stream "%c" (if Set.contains (x,y) ps then '#' else '.')
        fprintfn stream "|"

let generateResult =
    use sw = new StreamWriter(path=resultFile)

    let foldedPoints =
        folds
        |> List.fold (fun ps fld -> fold fld ps) points

    fprintfn sw "\nFolded points:"
    fprintPoints sw foldedPoints
    sw.Close()
