// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let parse (s : string array)
    =
    let arr = s.[0].Split(',')
    
    arr 
    |> Seq.indexed
    |> Seq.fold (fun (s, s') (i,x) -> (Map.add (int x) i s, Map.add i (int x) s')) (Map.empty, Map.empty)

// let solve1
//     (input : string array)
//     =
//     let p = parse input
//     let f (fromV' : Map<int, int>, fromI' : Map<int, int>) i
//         =
//         let last = Map.find (i - 1) fromI'
//         match Map.tryFind last fromV' with
//         | Some i' ->
//             let v' = i - i'
//             (Map.add v' i fromV', Map.add i v' fromI')
//       | _ -> (Map.add 0 i fromV', Map.add i 0 fromI')

//     {6 .. 2020}
//     |> Seq.fold f p

let solve1
    (s : string array)
    =
    let p = s.[0].Split(',') |> Array.mapi (fun i x -> (x, i + 1))

    let rec loop (((x : string), i), (xs : Map<string, int>)) =
        // printfn "((%A, %A), %A)" i x xs
        if i = 30000000
        then x
        else 
            match Map.tryFind x xs with
            | Some i' -> 
                // printfn "turn %i: %i - %i = %i" (i + 1) i i' (i - i') 
                loop ((i - i' |> sprintf "%i", i + 1), Map.add x i xs)
            | None -> 
                // printfn "turn %i: not found 0" (i + 1)
                loop (("0", i + 1), (Map.add x i xs))

    
    loop (p.[p.Length - 1], p.[0 .. p.Length - 2] |> Map.ofArray)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input.txt"
    
    printfn "input %A" input
    0 // return an integer exit code