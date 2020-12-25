// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.RegularExpressions
open System.IO

let matchNoop
    s
    =
    (false, s)

let matchChar
    c
    (s : string)
    =
    if String.IsNullOrEmpty(s)
    then (false, s)
    else if s.[0] = c
    then (true, s.Substring(1))
    else (false, s)

let matchBoth
    f1
    f2
    s
    =
    let (r, s') = f1 s
    if r
    then f2 s'
    else (false, s)

let matchOne
    f1
    f2
    s
    =
    let (r, s') = f1 s
    if r
    then (r, s')
    else f2 s

let test
    =
    let f4 = matchChar 'a'
    let f5 = matchChar 'b'
    let f3 = matchOne (matchBoth f4 f5) (matchBoth f5 f4)
    let f2 = matchOne (matchBoth f4 f4) (matchBoth f5 f5)
    let f1 = matchOne (matchBoth f2 f3) (matchBoth f3 f2)
    
    List.reduce matchBoth [f4;f1;f5]

type Rule =
    | Matchchar of c : char
    | MatchAll of ss : int list
    | MatchAny of ss : int list * ss' : int list  

let orPattern = @"^([^:]+): (\d+ ?)+\| (\d+ ?)+$"
let andPattern = @"^([^:]+): (\d+ ?)+$"
let charPattern = @"^([^:]+): \""(.)\""$"

let parse
    (s : string)
    =
    // printfn "%A" s
    if Regex.IsMatch(s, charPattern)
    then 
        let m = Regex.Match(s, charPattern)
        (m.Groups.[1].Value |> int, m.Groups.[2].Value |> char |> Matchchar)
    else if Regex.IsMatch(s, andPattern)
    then 
        let m = Regex.Match(s, andPattern)
        let vs = m.Groups.[2].Captures |> Seq.map (fun x -> x.Value |> int) |> List.ofSeq
        (m.Groups.[1].Value |> int, vs |> MatchAll)
    else 
        let m = Regex.Match(s, orPattern)
        let v1 = m.Groups.[2].Captures |> Seq.map (fun x -> x.Value |> int) |> List.ofSeq
        let v2 = m.Groups.[3].Captures |> Seq.map (fun x -> x.Value |> int) |> List.ofSeq
        
        (m.Groups.[1].Value |> int, MatchAny (v1, v2))

let rec parseRules
    (m : Map<int, Rule>)
    (i : int)
    =
    // printfn "i: %A v: %A" i (Map.find i m)
    // Console.ReadKey() |> ignore
    match Map.find i m with
    | Matchchar c -> matchChar c
    | MatchAll is -> List.map (parseRules m) is |> List.reduce matchBoth
    | MatchAny (is, is') ->
        let ms = List.map (parseRules m) is |> List.reduce matchBoth
        let ms' = List.map (parseRules m) is' |> List.reduce matchBoth
        matchOne ms ms'

let readInput
    (p : string)
    =
    let i = File.ReadAllLines p
    // printfn "i: %A" i
    let rs =
        i 
        |> Seq.takeWhile (fun x -> x <> "")
        |> Seq.map parse
        |> Map.ofSeq

    let ss = 
        i 
        |> Seq.skipWhile (fun x -> x <> "")
        |> Seq.skip 1

    (rs, ss)

let solve1
    (p : string)
    =
    let (rs,ss) = readInput p
    let r0 = parseRules rs 0

    let count (s, r) =
        if s && r = ""
        then 1
        else 0

    ss
    |> Seq.map (count << r0)
    |> Seq.reduce (+)

let solve2
    (p : string)
    =
    let (rs,ss) = readInput p
    let r42 = parseRules rs 42
    let r31 = parseRules rs 31

    let rec matchUntil
        (f) 
        (s : string)
        (c : int)
        =
        if s = ""
        then (c, "")
        else
            let (r, s') = f s
            if r
            then matchUntil f s' (c + 1)
            else (c, s)


    let count2 s =
        let (c42, s') = matchUntil r42 s 0
        let (c31, s'') = matchUntil r31 s' 0

        if c31 > 0 && c42 > c31 && s'' = ""
        then
            // printfn "c42: %A c31: %A s'': %A match: %A" c42 c31 s'' s 
            1
        else
            // printfn "c42: %A c31: %A s'': %A no match: %A" c42 c31 s'' s 
            0

    ss
    |> Seq.map count2
    |> Seq.reduce (+)

[<EntryPoint>]
let main argv =
    0 // return an integer exit code