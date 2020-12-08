// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let rowPattern = @"^(.+) bags contain (?:no other bags)?(?:(\d[^,.]+)(?:, )*)*.$"
let entryPattern = @"^(\d) (.+) bags?$"

let toEntry (s : string)
    =
    let m = Regex.Match(s, entryPattern)

    (m.Groups.[1].Value |> int, m.Groups.[2].Value)

let toKvps (s : string)
    =
    let m = Regex.Match(s, rowPattern)
    let key = m.Groups.[1].Value
    let values = 
        m.Groups.[2].Captures
        |> Seq.map (fun x -> x.Value) 
        |> Seq.filter (not << isNull)
        |> Seq.map toEntry

    (key, values)

let rec solve1
    (lookup : Map<string, seq<int * string>>)
    (b : string)
    =
    let isGolden (n : int, b : string) = b = "shiny gold"
    let inner = Map.find b lookup
    if Seq.exists isGolden inner
    then true
    else if Seq.isEmpty inner
    then false
    else
        inner  
        |> Seq.map (fun (_, b) -> solve1 lookup b)
        |> Seq.reduce (||)

let rec solve2
    (lookup : Map<string, seq<int * string>>)
    (b : string)
    =
    let inner = Map.find b lookup
    if Seq.isEmpty inner
    then 0
    else 
        inner
        |> Seq.map (fun (n, b) -> n * (solve2 lookup b) + n)
        |> Seq.reduce (+)

[<EntryPoint>]
let main argv =
    let lookup = 
        File.ReadAllLines "input.txt"
        |> Seq.map toKvps
        |> Map.ofSeq

    // let r = 
    //     lookup
    //     |> Map.toSeq
    //     |> Seq.map (fun (b, _) -> solve1 lookup b)
    //     |> Seq.filter id
    //     |> Seq.length

    let r = solve2 lookup "shiny gold" 

    printfn "%A" r 
    0 // return an integer exit code