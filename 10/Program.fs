// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Collections.Generic

let solve1
    (xs : string array)
    =
    let r = 
        ("0" :: List.ofArray xs)
        |> Seq.map int
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> b - a)
        |> Seq.countBy id

    let diffOfOne = 
        r 
        |> Seq.filter (fun x -> fst x = 1)
        |> Seq.map snd
        |> Seq.head

    let diffOfThree =
        r 
        |> Seq.filter (fun x -> fst x = 3)
        |> Seq.map snd
        |> Seq.head

    diffOfOne * (diffOfThree + 1)

let solve2
    (xs : string array)
    =
    let xs' = xs |> Seq.map int |> Seq.sort 
    printfn "xs': %A" (xs' |> List.ofSeq)
    let memo = Dictionary<int, int64>()
    let rec recurse
        (xs'' : int seq)
        (c : int)
        =
        if Seq.isEmpty xs''
        then 1L
        else
            Seq.takeWhile (fun x -> x - c <= 3) xs''
            |> Seq.indexed
            |> Seq.map (fun (i, x) ->
                let hit, r = memo.TryGetValue(x) 
                if hit then r
                else 
                    let r' = recurse (Seq.skip (i + 1) xs'') x
                    memo.Add(x, r')
                    r'
            )
            |> Seq.reduce (+)
    
    recurse xs' 0

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input.txt"

    // printfn "input: %A" input
    printfn "solve2: %A" (solve2 input)
    0 // return an integer exit code