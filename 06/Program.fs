// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let countAnyYes (s : string)
    =
    s.Replace("\n","")
    |> Set.ofSeq 
    |> Set.count

let countAllYes (s : string)
    =
    s.Split "\n"
    |> Seq.map Set.ofSeq
    |> Seq.reduce Set.intersect
    |> Set.count

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "input.txt"

    let r = 
        input.Split("\n\n")
        |> Seq.map countAllYes
        |> Seq.reduce (+)
        

    printfn "%i" r
    0 // return an integer exit code