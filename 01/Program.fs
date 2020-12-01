// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input1.txt"

    let (a, b, c) = 
        Seq.allPairs input input
        |> Seq.allPairs input
        |> Seq.map (fun (a, (b, c)) -> (int a, int b, int c))
        |> Seq.find (fun (a, b, c) -> a + b + c = 2020)
    
    printfn "%i" (a * b * c)
    0 // return an integer exit code