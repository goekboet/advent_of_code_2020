// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let check
    (pre : int64 list)
    (x : int64)
    =
    Seq.allPairs pre pre
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map (fun (a, b) -> a + b)
    |> Seq.contains x

let findNumber
    (pre : int64 list, valid : int64 option)
    (x : int64)
    =
    if check pre x
    then (List.take pre.Length (x :: pre), None)
    else (pre, Some x)

let findSubSet
    (xs : int64 array)
    (t : int64)
    =
    let rec loop l h s
        =
        if h >= xs.Length
        then (0,0)
        else if s = t && h - l > 1
        then (l, h - 1)
        else if s > t
        then 
            let s' = s - xs.[l]
            printfn "s > t %A %A %A %A %A" l h s s' t
            loop (l + 1) h s'
        else 
            let s' = s + xs.[h]
            printfn "s < t %A %A %A %A %A" l h s s' t
            loop l (h + 1) s'

    loop 0 0 0L
    
[<EntryPoint>]
let main argv =
    let preambeLength = 5
    let input = 
        File.ReadAllLines "input.txt" 
        |> Array.map int64 
    
    // let pre = 
    //     Seq.take preambeLength input
    //     |> Seq.rev
    //     |> List.ofSeq

    // let r = 
    //     Seq.skip preambeLength input
    //     |> Seq.scan findNumber (pre, None)
    //     |> Seq.pick snd 

    // let r' = findSet (input |> List.ofSeq) 0L 26134589L []
    let (l, h) = findSubSet input 26134589L
    let min = input.[l .. h] |> Seq.min
    let max = input.[l .. h] |> Seq.max
    printfn "%A" input.[l .. h]
    printfn "min: %A" min
    printfn "max: %A" max

    let r' = min + max
        
     
    // printfn "preamble: %A" pre
    // printfn "r: %A" r
    printfn "r' %A" r'
    0 // return an integer exit code