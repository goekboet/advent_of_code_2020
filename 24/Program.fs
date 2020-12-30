// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let ds = 
    [ ( 1, 0,-1)
    ; ( 1,-1, 0)
    ; ( 0,-1, 1)
    ; (-1, 0, 1)
    ; (-1, 1, 0)
    ; ( 0, 1,-1)
    ]

let add (x,y,z) (x',y',z') = (x + x', y + y', z + z')

let adj v = List.map (add v) ds

let rec parse (x, y, z) l
    =
    if l = ""
    then (x, y, z)
    else if l.StartsWith "ne"
    then parse (x + 1,    y,z - 1) (l.Substring 2)
    else if l.StartsWith "e"
    then parse (x + 1,y - 1,    z) (l.Substring 1)
    else if l.StartsWith "se"
    then parse (    x,y - 1,z + 1) (l.Substring 2)
    else if l.StartsWith "sw"
    then parse (x - 1,    y,z + 1) (l.Substring 2)
    else if l.StartsWith "w"
    then parse (x - 1,y + 1,    z) (l.Substring 1)
    else if l.StartsWith "nw"
    then parse (    x,y + 1,z - 1) (l.Substring 2)
    else (0,0,0)

let solve1 p
    =
    let ls = File.ReadAllLines p
    let init = Set.empty
    let f s x =
        if Set.contains x s 
        then Set.remove x s 
        else Set.add x s

    let r =
        ls
        |> Seq.map (parse (0,0,0))
        |> Seq.fold f init

    Set.count r 
    
let solve2 p
    =
    let ls = File.ReadAllLines p
    let init = Set.empty
    let f s x =
        if Set.contains x s 
        then Set.remove x s 
        else Set.add x s

    let r =
        ls
        |> Seq.map (parse (0,0,0))
        |> Seq.fold f init

    let f' xs
        =
        let flipToWhite
            =
            Seq.choose (fun v -> 
                let a = adj v |> Set.ofSeq
                let r = Set.intersect a xs |> Set.count
                if r = 0 || r > 2 then (Some v) else None) 
                xs
            |> Set.ofSeq

        let whites =
            let w = 
                Seq.collect adj xs
                |> Set.ofSeq
            
            Set.difference w xs

        let flipToBlack =
            whites
            |> Seq.choose (fun v -> 
                let a = adj v |> Set.ofSeq
                let r = Set.intersect a xs |> Set.count
                if r = 2 then (Some v) else None)
            |> Set.ofSeq

        // printfn "xs: %A" xs
        // printfn "fw: %A" flipToWhite
        // printfn "fb: %A\n" flipToBlack

        let bs = Set.union xs flipToBlack
        let r' = Set.difference bs flipToWhite

        // printfn "%A" (Set.count r')
        Some (r' |> Set.count, r') 

    let r' =
        Seq.unfold f' r
        |> Seq.take 100
        |> Seq.toList
   
    // printfn "%A" r'

    Seq.last r' 

[<EntryPoint>]
let main argv =
 
    0 // return an integer exit code