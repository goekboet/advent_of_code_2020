// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let parse
    (input : string array)
    =
    let h = input.Length
    let w = input.[0].Length

    {0 .. h - 1}
    |> Seq.collect (fun y -> {0 .. w - 1} |> Seq.map (fun x -> ((x,y), input.[y].[x])))
    |> Map.ofSeq

let print
    (map : Map<int * int, char>)
    =
    map
    |> Map.toSeq
    |> Seq.groupBy (snd << fst)
    |> Seq.sortBy fst
    |> Seq.iter (fun (_, cs) -> Seq.sortBy (fst << fst) cs |> Seq.map snd |> Seq.toArray |> String |> printfn "%s")

let occupations
    (m : Map<int * int, char>)
    (x : int , y : int)
    =
    [(-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0)]
    |> Seq.map (fun (a, b) -> (a + x, b + y))
    |> Seq.choose (fun x -> Map.tryFind x m)
    |> Seq.filter (fun x -> x = '#')
    |> Seq.length

let occupations2
    (m : Map<int * int, char>)
    (x : int , y : int)
    =
    let rec inLineOfSight
        (x' : int, y' : int)
        (d : int)
        =
        match Map.tryFind (d * x' + x, d * y' + y) m with
        | Some c -> 
            if c = 'L' then 0 
            else if c = '#' then 1 
            else inLineOfSight (x', y') (d + 1)
        | None -> 0
    
    [(-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0)]
    |> Seq.map (fun v' -> inLineOfSight v' 1)
    |> Seq.reduce (+)

let next
    (m : Map<int * int, char>)
    (v : int * int) 
    =
    let c = Map.find v m
    let os = occupations m v
    if os = 0
    then if c = 'L' then (v, '#') else (v, c)
    else if os >= 4
    then if c = '#' then (v, 'L') else (v, c)
    else (v, c)

let next2
    (m : Map<int * int, char>)
    (v : int * int) 
    =
    let c = Map.find v m
    let os = occupations2 m v
    if os = 0
    then if c = 'L' then (v, '#') else (v, c)
    else if os >= 5
    then if c = '#' then (v, 'L') else (v, c)
    else (v, c)
    

let turn
    (m : Map<int * int, char>)
    =
    m
    |> Map.toSeq
    |> Seq.map (next2 m << fst)
    |> Map.ofSeq

let run
    (init : Map<int * int, char>) 
    =
    init 
    |> Seq.unfold (fun m -> (m, turn m) |> Some)
    |> Seq.pairwise
    |> Seq.takeWhile (fun (a,b) -> a <> b)
    |> Seq.last
    |> snd

let countOccupied
    (m : Map<int * int, char>)
    =
    m
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.filter (fun x -> x = '#')
    |> Seq.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "ex.txt" // Call the function


    
    0 // return an integer exit code