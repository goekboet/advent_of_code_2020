// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let getSlope (w : int) (h : int) (dx : int, dy : int) 
    =
    Seq.init h (fun x -> ((x * dx) % w, x * dy))
    |> Seq.takeWhile (fun (x, y) -> y < h)

let countTrees 
    (map : string array) 
    (cs : (int * int) seq)
    =
    cs
    |> Seq.map (fun (x,y) -> map.[y].[x])
    |> Seq.filter (fun x -> x = '#')
    |> Seq.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let w = input.[0].Length
    let h = input.Length

    let ds = [(1,1);(3,1);(5,1);(7,1);(1,2)]
    let r =
        ds
        |> Seq.map ((countTrees input) << (getSlope w h))
        |> Seq.reduce (*)
               
    printfn "w: %i" w
    printfn "h: %i" h
    printfn "r: %i" r
    0 // return an integer exit code