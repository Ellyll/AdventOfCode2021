let filename = "Day04/day04.data"

let lines = System.IO.File.ReadAllLines(filename)

let input =
    lines[0].Split(',')
    |> Array.map (int)
    |> List.ofArray

let boards =
    let regWhiteSpace = System.Text.RegularExpressions.Regex(@"\s+")
    [| 1..6..((Array.length lines)-1) |]
    |> Array.map (fun i ->
        let numbers =
            [| 1..5 |]
            |> Array.map (fun l -> regWhiteSpace.Split(lines[i+l].Trim()))
        printfn "Numbers: %A" numbers
        let board = Array2D.init 5 5 (fun r c -> int numbers.[r].[c])
        let markers = Array2D.zeroCreate<bool> 5 5
        (board, markers)
        )

let hasWon (markers: bool[,]) : bool =
    let rec checkColumns c =
        let column = Array.init (Array2D.length1 markers) (fun r -> markers[r, c])
        if column |> Array.forall (id) then
            true
        else
            if c = 0 then
                false
            else
                checkColumns (c - 1)
    let rec checkRows r =
        let row = Array.init (Array2D.length2 markers) (fun c -> markers[r, c])
        if row |> Array.forall (id) then
            true
        else
            if r = 0 then
                false
            else
                checkRows (r - 1)               
    
    checkColumns ((Array2D.length2 markers) - 1)
    || checkRows ((Array2D.length1 markers) - 1)

let boardTryFindIndex (fn: int -> bool) (board: int[,]) =
    [ 0..((Array2D.length1 board)-1) ]
    |> List.tryPick (fun l ->
            [ 0..((Array2D.length2 board)-1) ]
            |> List.tryPick (fun c ->
                if fn board.[l,c] then
                    Some (l,c)
                else
                    None
                )
        )

let array2DMapi (fn: int -> int -> 'a -> 'b) (arr: 'a[,]) =
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun l c -> fn l c arr.[l,c])

let array2DFoldi (folder: 'State -> int -> int -> 'T -> 'State) (state: 'State) (array: 'T[,]) : 'State =
    seq { 0..((Array2D.length1 array)-1) }
    |> Seq.fold (fun sl l ->
        seq { 0..((Array2D.length2 array)-1) }
        |> Seq.fold (fun sc c ->
                folder sc l c array.[l,c]
            ) sl
        ) state

let markNumber n (board,markers) =
    match board |> boardTryFindIndex (fun x -> x = n) with
    | Some (l,c) ->
        let markers' = Array2D.copy markers
        markers'.[l,c] <- true
        (board,markers')
    | None ->
        (board,markers)

let (winN, ((winBoard: int[,]), (winMarkers: bool[,]))) =
    let rec loop ns (bs: (int[,]*bool[,]) array) =
        match ns with
        | [] -> None
        | n::ns' ->
            let markedBoards = bs |> Array.map (markNumber n)
            let winningBoard =
                markedBoards
                |> Array.tryFind (fun (_,m) -> hasWon m)
            match winningBoard with
            | Some wb -> Some (n,wb)
            | None -> loop ns' markedBoards
    match loop input boards with
    | Some x -> x
    | None -> failwith "Unable to find winner"

let result =
    // Get sum of all unmarked numbers
    let boardTotal =
        winBoard
        |> array2DFoldi (fun total l c x ->
            if not winMarkers.[l,c] then
                total + x
            else
                total
            ) 0
    winN * boardTotal

printfn "Result: %i" result