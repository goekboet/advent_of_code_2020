// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let parse (s : string)
    = 
    let e = s.Split " "

    (e.[0], e.[1] |> int, 0)

let rec solve1
    (prg : array<string * int * int>)
    (ptr : int)
    (acc : int)
    =
    if (ptr > prg.Length || ptr < 0)
    then (acc, false)
    else if ptr = prg.Length
    then (acc, true)
    else
        let (instr, arg, read) = prg.[ptr]
        if (read > 0)
        then (acc, false)
        else 
            prg.[ptr] <- (instr, arg, read + 1)
            match instr with
            | "acc" -> solve1 prg (ptr + 1)   (acc + arg)
            | "jmp" -> solve1 prg (ptr + arg) acc
            | _     -> solve1 prg (ptr + 1)   acc

let modPrg
    (prg : array<string * int * int>)
    (i : int)
    =
    let (instr, arg, _) = prg.[i]
    match instr with
    | "jmp" ->
        let cpy = Array.copy prg
        cpy.[i] <- ("nop", arg, 0)
        cpy
    | "nop" ->
        let cpy = Array.copy prg
        cpy.[i] <- ("jmp", arg, 0)
        cpy
    | _ -> Array.empty

[<EntryPoint>]
let main argv =
    let input = 
        File.ReadAllLines "input.txt"
        |> Seq.map parse
        |> Array.ofSeq

    let runPrg p = solve1 p 0 0
    let (r, _) =
        {0 .. input.Length - 1}
        |> Seq.map (modPrg input)
        |> Seq.filter (not << Seq.isEmpty)
        |> Seq.map runPrg
        |> Seq.find snd

    // let (r, _) = solve1 input 0 0
    printfn "r: %A" r
    0 // return an integer exit code