open System.IO
open System.Text.RegularExpressions

let filename = "Day14/day14.data"

let getPairs (str: string) =
    seq { 0..((String.length str)-2)}
    |> Seq.fold (fun pairs idx ->
        let pair = str[idx..idx+1]
        pairs
        |> Map.change pair (function
            | Some n -> Some (n + 1L)
            | None -> Some 1L
            )
        ) Map.empty

let pairCounts, letterCounts, rules =
    let templateReg = Regex @"^([A-Z]+)$"
    let ruleReg = Regex @"^([A-Z]{2}) -> ([A-Z])$"
    File.ReadAllLines(filename)
    |> Array.fold (fun (pairCounts,letterCounts,rules) line ->
        if templateReg.IsMatch line then
            let pairs = getPairs line
            let letters =
                line
                |> Seq.map (fun c -> c.ToString())
                |> Seq.countBy (id)
                |> Seq.map (fun (ltr,cnt) -> ltr, int64 cnt )
                |> Map.ofSeq
            (pairs, letters, rules)
        elif ruleReg.IsMatch line then
            let key = (ruleReg.Matches(line)[0]).Groups[1].Value
            let value = (ruleReg.Matches(line)[0]).Groups[2].Value
            let newRules =
                rules
                |> Map.change key (fun _ -> Some value)
            (pairCounts, letterCounts, newRules)
        elif String.length line = 0 then
            (pairCounts, letterCounts,rules)
        else
            failwithf "Unable to parse: %s" line
        ) (Map.empty, Map.empty, Map.empty)

let applyChanges (pairCounts: Map<string,int64>) (letterCounts: Map<string,int64>) (changes: seq<string*int64>*seq<string*int64>) =
    let pairChanges, letterAdditions = changes
    let newPairCounts =
        pairChanges
        |> Seq.fold (fun t (pair,n) ->
            t |> Map.change pair (function
                | Some count ->
                    let newCount = count + n
                    if newCount >= 1 then
                        Some newCount
                    else
                        None
                | None ->
                    if n < 1L then
                        failwithf "This should happen, pair: %s, n: %i" pair n
                    Some n
                )
            ) pairCounts
    let newLetterCounts =
        letterAdditions
        |> Seq.fold (fun counts (letter,occurances) ->
            counts
            |> Map.change letter (function
                | Some n -> Some (n + occurances)
                | None -> Some occurances)
            ) letterCounts
    newPairCounts,newLetterCounts

let getChanges (pairCounts: Map<string,int64>) rules =
    rules
    |> Map.fold (fun (pairChanges, letterAdditions) key value ->        
        match pairCounts |> Map.tryFind key with
        | Some count ->
            // insert for each new pair
            let pair1 = key[0..0] + value
            let pair2 = value + key[1..1]
            (key,-count)::(pair1,count)::(pair2,count)::pairChanges,(value, count)::letterAdditions
        | None ->
            pairChanges,letterAdditions
        ) ([],[])

let applyRules (pairCounts: Map<string, int64>) (letterCounts: Map<string, int64>) (rules: Map<string,string>) =
    let changes = getChanges pairCounts rules
    applyChanges pairCounts letterCounts changes

let _, finalLetterCounts =
    seq { 1..40 }
    |> Seq.fold (fun (pc,lc) _ -> applyRules pc lc rules) (pairCounts,letterCounts)

let finalLength =
    finalLetterCounts
    |> Map.toSeq
    |> Seq.sumBy(snd)

let minValue = finalLetterCounts |> Map.toSeq |> Seq.minBy (snd) |> snd
let maxValue = finalLetterCounts |> Map.toSeq |> Seq.maxBy (snd) |> snd

let result = maxValue - minValue
printfn "Result: %i" result
