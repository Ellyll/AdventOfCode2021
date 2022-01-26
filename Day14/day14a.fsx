open System.IO
open System.Text.RegularExpressions

let filename = "Day14/day14.data"

let template, rules =
    let templateReg = Regex @"^([A-Z]+)$"
    let ruleReg = Regex @"^([A-Z]{2}) -> ([A-Z])$"
    File.ReadAllLines(filename)
    |> Array.fold (fun (template,rules) line ->
        if templateReg.IsMatch line then
            (line, rules)
        elif ruleReg.IsMatch line then
            let key = (ruleReg.Matches(line)[0]).Groups[1].Value
            let value = (ruleReg.Matches(line)[0]).Groups[2].Value
            let newRules =
                rules
                |> Map.change key (fun _ -> Some value)
            (template, newRules)
        elif String.length line = 0 then
            (template,rules)
        else
            failwithf "Unable to parse: %s" line
        ) ("", Map.empty)

let applyInserts (template: string) (inserts: (string*int) list) =
    let rec loop (currentTemplate: string) remainingInserts =
        match remainingInserts with
        | [] -> currentTemplate
        | (value,index)::xs ->
            let newTemplate = currentTemplate.Insert(index, value)
            let newInserts =
                xs
                |> List.map (fun (v,i) -> v, if i >= index then i + 1 else i)
            loop newTemplate newInserts
    loop template inserts

let getInserts template rules =
    seq { 0..((String.length template) - 1)}
    |> Seq.fold (fun inserts idx ->
            let newInserts =
                rules
                |> Map.fold (fun ri key value ->
                    let keyLength = String.length key
                    if keyLength <= (String.length template[idx..]) && key = template[idx..(idx+keyLength-1)] then
                        (value,idx+1)::ri
                    else
                        ri
                    ) []
            inserts @ newInserts
            ) []


let applyRules (template: string) (rules: Map<string,string>) =
    let inserts = getInserts template rules
    applyInserts template inserts

let finalTemplate =
    seq { 1..10 }
    |> Seq.fold (fun t _ -> applyRules t rules) template

let counts =
    finalTemplate
    |> Seq.countBy (id)
    |> Seq.sortBy (snd)
    |> Seq.toList

let minValue = counts |> List.head |> snd
let maxValue = counts |> List.last |> snd

let result = maxValue - minValue
printfn "Result: %i" result
