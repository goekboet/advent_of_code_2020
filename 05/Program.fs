// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let rec search (s : char list) (rows : int list)
    =
    if (rows.Length = 1)
    then List.head rows
    else
        let c = s.[0]
        let half = rows.Length / 2
        if c = 'B' || c = 'R'
        then search (List.skip 1 s) (List.skip half rows)
        else search (List.skip 1 s) (List.take half rows)

let getSeat (s : string)
    =
    let rows = {0 .. 127} |> List.ofSeq
    let cols = {0 ..7} |> List.ofSeq
    let instr = s |> List.ofSeq

    let row = search (List.take 7 instr) rows
    let col = search (List.skip 7 instr) cols

    (row, col)

let getId (row : int, col : int)
    =
    row * 8 + col

let nextSeat (row : int, col : int)
    =
    if col = 7
    then (row + 1, 0)
    else (row, col + 1) 

[<EntryPoint>]
let main argv =
    // let r = 
    //     File.ReadAllLines "input.txt"
    //     |> Seq.map (getId << getSeat)
    //     |> Seq.max

    let r = 
        File.ReadAllLines "input.txt"
        |> Seq.map (getId << getSeat)
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.find (fun (a, b) -> a + 1 <> b)
             
    printfn "%A" (fst r + 1)
    0 // return an integer exit code