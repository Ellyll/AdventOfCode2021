open System.IO
open System.Text.RegularExpressions

let filename = "Day05/day05.data"

type Point = { X: int ; Y: int }

let data =
    File.ReadAllLines(filename)
    |> Array.map (fun l ->
        match l.Split(" -> ") with
        | [| sp1 ; sp2 |] ->
            let p1 =
                match sp1.Split(',') with
                | [| sx1 ; sy1 |] -> { X = int sx1 ; Y = int sy1 }
                | _ -> failwithf "Unable to parse: %s" sp1
            let p2 =
                match sp2.Split(',') with
                | [| sx2 ; sy2 |] -> { X = int sx2 ; Y = int sy2 }
                | _ -> failwithf "Unable to parse: %s" sp2
            p1,p2
        | _ -> failwithf "Unable to parse: %s" l
        )
    // Only want horizontal or vertical lines
    |> Array.filter (fun (p1,p2) -> p1.X = p2.X || p1.Y = p2.Y)

let drawLine p1 p2 =
    seq {
        let dx = sign (p2.X - p1.X)
        let dy = sign (p2.Y - p1.Y)
        yield p1
        let rec loop p ps =
            if p = p2 then
                ps
            else
                let newP =
                    {
                        X = if p.X = p2.X then p2.X else p.X + dx
                        Y = if p.Y = p2.Y then p2.Y else p.Y + dy
                    }
                loop newP (ps @ [ newP ])
        yield! (loop p1 [])
    }

let overlaps =
    data
    |> Array.map (fun (p1,p2) -> drawLine p1 p2)
    |> Array.fold (fun state drawnPoints ->
        drawnPoints
        |> Seq.fold (fun st p ->
            st
            |> Map.change p (fun v ->
                match v with
                | Some n -> Some (n + 1)
                | None -> Some 1
                )
            ) state     
        ) Map.empty
    |> Map.filter (fun _ v -> v > 1)

let result = overlaps |> Map.count

printfn "Result: %i" result
