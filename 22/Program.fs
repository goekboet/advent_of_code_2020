// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let parse p =
    let ls = File.ReadAllLines p
    let player ls' =
        let id = Seq.head ls'
        let deck =
            ls'
            |> Seq.skip 1
            |> Seq.map int64
            |> Seq.toList
        (id, deck)

    let p1 =
        ls
        |> Seq.takeWhile (not << Seq.isEmpty)
        |> player

    let p2 =
        ls
        |> Seq.skipWhile (not << Seq.isEmpty)
        |> Seq.skip 1
        |> player
    
    (p1, p2)

let rec play (p1, d1) (p2, d2)
    =
    if List.isEmpty d1
    then
        printfn "%s won" p2
        d2
    else if List.isEmpty d2
    then
        printfn "%s won" p1
        d1
    else
        let (p1', p2') = (List.head d1, List.head d2)
        if p1' > p2'
        then
            printfn "%s won the round" p1
            let d1' = (List.tail d1) @ [p1';p2']
            let d2' = List.tail d2  
            play (p1, d1') (p2,d2')
        else 
            printfn "%s won the round" p2
            let d1' = List.tail d1
            let d2' = (List.tail d2) @ [p2';p1'] 
            play (p1, d1') (p2,d2')

let score (d : int64 list)
    =
    let l = List.length d |> int64
    d
    |> Seq.mapi (fun i x -> (l - int64 i) * x)
    |> Seq.sum

let solve1 p
    =
    let (p1, p2) = parse p
    let r = play p1 p2
    
    score r

let rec play' ps game round (p1, d1) (p2, d2)
    =
    let recursiveCombat (p1', d1') (p2', d2') =
        let l1 = List.length d1' |> int64
        let l2 = List.length d2' |> int64
        l1 >= p1' && l2 >= p2'

    // printfn "-- Round %i (Game %i) --" round game
    // printfn "%A" (p1, d1)
    // printfn "%A" (p2, d2)
    if List.isEmpty d1
    then
        // printfn "%s won game %i after %i rounds" p2 game round
        (p2,d2)
    else if List.isEmpty d2
    then
        // printfn "%s won game %i after %i rounds" p1 game round
        (p1,d1)
    else if Set.contains ((p1, d1),(p2, d2)) ps
    then 
        // printfn "%s won game %i after %i rounds to stop infinite recursion" p1 game round
        (p1, d1)
    else
        let (p1', p2') = (List.head d1, List.head d2)
        let ps' = Set.add ((p1, d1),(p2, d2)) ps
        if recursiveCombat (p1', List.tail d1) (p2', List.tail d2)
        then 
            // printfn "Winner is determined by recursive combat."
            let d1' = List.take (int p1') (List.tail d1)
            let d2' = List.take (int p2') (List.tail d2)
            let (winner, _) = play' Set.empty (game + 1) 1 (p1, d1') (p2, d2')
            if winner = p1
            then 
                // printfn "%s won round %i game %i" p1 round game
                let d1' = (List.tail d1) @ [p1';p2']
                let d2' = List.tail d2  
                play' ps' game (round + 1) (p1, d1') (p2,d2')
            else
                // printfn "%s won round %i game %i" p2 round game
                let d1' = List.tail d1
                let d2' = (List.tail d2) @ [p2';p1'] 
                play' ps' game (round + 1) (p1, d1') (p2,d2')
        else if p1' > p2'
        then
            // printfn "%s won round %i game %i" p1 round game
            let d1' = (List.tail d1) @ [p1';p2']
            let d2' = List.tail d2  
            play' ps' game (round + 1) (p1, d1') (p2,d2')
        else 
            // printfn "%s won round %i game %i" p2 round game
            let d1' = List.tail d1
            let d2' = (List.tail d2) @ [p2';p1'] 
            play' ps' game (round + 1) (p1, d1') (p2,d2')

let solve2 p =
    let (p1, p2) = parse p
    let (_, d) = play' Set.empty 1 1 p1 p2
    
    score d

[<EntryPoint>]
let main argv =

    0 // return an integer exit code