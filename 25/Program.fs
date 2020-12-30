// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let rec getLoopSize pc n i =
    if n = pc
    then i
    else
        let n' = n * 7
        let n'' = n' % 20201227
        getLoopSize pc n'' (i + 1)

let rec getPrivatekey (pk : int64) ls (n : int64)
    =
    if ls = 0
    then n
    else
        let n' = n * pk
        let n'' = n' % 20201227L
        getPrivatekey pk (ls - 1) n''


let solve1 p =
    let inp = File.ReadAllLines p
    let pcD = int inp.[0]
    let pcL = int inp.[1]

    let lsD = getLoopSize pcD 1 0
    let lsL = getLoopSize pcL 1 0

    let r = getPrivatekey (int64 pcD) lsL 1L
    printfn "r: %A" r

[<EntryPoint>]
let main argv =

    0 // return an integer exit code