let filename = "Day02/day02.data"

let data =
    System.IO.File.ReadAllLines(filename)
    |> Array.map(fun line -> 
        match line.Split(' ') with
        | [| command ; stringValue |] -> (command, int stringValue)
        | _ -> failwithf "Invalid line: %s" line)

let (horizontal, depth, aim) =
    data
    |> Array.fold (fun (h,d, a) (command,value) ->
        match command with
        | "forward" -> (h + value, d + (a * value), a)
        | "down" -> (h, d, a + value)
        | "up" -> (h, d, a - value)
        | _ -> failwithf "Invalid command: %s" command
        ) (0,0,0)

let result = horizontal * depth

printfn "Result: %i" result