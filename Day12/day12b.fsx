open System.IO

let filename = "Day12/day12.data"

let data =
    File.ReadAllLines(filename)
    |> Array.fold (fun state line ->
        match line.Split('-') with
        | [| a ; b |] ->
            state
            // add a to b
            |> Map.change a (fun connections -> connections |> Option.defaultValue Set.empty |> Set.add b |> Some)
            // add b to a
            |> Map.change b (fun connections -> connections |> Option.defaultValue Set.empty |> Set.add a |> Some)
        | _ -> failwithf "Unable to parse: %s" line
        ) Map.empty

let findPaths (data: Map<string, Set<string>>) =
    let rec loop paths history =
        let current = history |> List.head
        if current = "end" then
            paths |> Set.add history
        else
            let destinations =
                data
                |> Map.tryFind current
                |> function
                    | Some ds -> ds
                    | None -> failwithf "Unable to find %s" current 
                |> Set.filter (fun location ->
                    if location = "start" then
                        false
                    elif System.Char.IsLower location[0] then
                        let lowerCaseCounts =
                            history
                            |> List.filter (fun l ->
                                (System.Char.IsLower l[0]) && l <> "start" && l <> "end")
                            |> List.groupBy (id)
                            |> List.map (fun (l,xs) -> (l, xs |> List.length))
                            |> Map.ofList
                        // if it doesn't exist in Map then can add it
                        // if it exists in the Map and there are no counts > 1 in the Map then we can add it
                        (lowerCaseCounts |> Map.containsKey location |> not) ||
                           (lowerCaseCounts |> Map.forall (fun _ v -> v < 2))
                    else
                        true
                    )
            destinations
            |> Set.fold (fun ps location ->
                loop ps (location::history)
                ) paths

    loop Set.empty [ "start" ]

let paths = findPaths data
printfn "Number of paths: %i" (paths |> Set.count)

let result =
    paths
    |> Set.filter (fun ls ->
        ls |> List.exists (fun l -> l <> "start" && l <> "end" && System.Char.IsLower l[0])
        )
    |> Set.count

printfn "Result: %i" result
