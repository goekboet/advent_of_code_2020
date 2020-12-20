// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let parse
    (input : string array)
    =
    let h = input.Length
    let w = input.[0].Length

    let addActive s (x, y)
        =
        match input.[y].[x] with
        | '#' -> Set.add (x, y, 0) s
        | _ -> s

    let active =
        Seq.allPairs {0 .. w - 1} {0 .. h - 1}
        |> Seq.fold addActive Set.empty

    ((0,0,0), (w - 1, h - 1, 0), active)

let parse2
    (input : string array)
    =
    let h = input.Length
    let w = input.[0].Length

    let addActive s (x, y)
        =
        match input.[y].[x] with
        | '#' -> Set.add (x, y, 0, 0) s
        | _ -> s

    let active =
        Seq.allPairs {0 .. w - 1} {0 .. h - 1}
        |> Seq.fold addActive Set.empty

    ((0,0,0, 0), (w - 1, h - 1, 0, 0), active)

let space
    (x , y , z )
    (x', y', z')
    =
    Seq.allPairs {x .. x'} {y .. y'} 
    |> Seq.allPairs {z .. z'} 
    |> Seq.map (fun (z'', (x'', y'')) -> (x'', y'', z''))

let space2
    (x , y , z , w )
    (x', y', z', w')
    =
    Seq.allPairs {x .. x'} {y .. y'} 
    |> Seq.allPairs {z .. z'}
    |> Seq.allPairs {w .. w'} 
    |> Seq.map (fun (w'', (z'', (x'', y''))) -> (x'', y'', z'', w''))

let print
    ((x, y, z), (x', y', z'), s)
    =
    let toChar v =
        if Set.contains v s
        then '#'
        else '.'

    let printSlice z''
        =
        space (x, y, z'') (x', y', z'')
        |> Seq.groupBy (fun (_, b, _) -> b)
        |> Seq.sortBy (fun (a, _) -> a)
        |> Seq.iter (fun (_, b) -> Seq.sortBy (fun (a, _, _) -> a) b |> Seq.map toChar |> Seq.toArray |> String |> printfn "%s")

    {z .. z'}
    |> Seq.iter (fun z'' -> 
        printfn "z: %A" z''
        printSlice z'')

let expand
    ((x, y, z), (x', y', z'))
    =
    space (x - 1, y - 1, z - 1) (x' + 1, y' + 1, z' + 1)

let expand2
    ((x, y, z, w), (x', y', z', w'))
    =
    space2 (x - 1, y - 1, z - 1, w - 1) (x' + 1, y' + 1, z' + 1, w' + 1)

let countNs
    p
    s
    =
    expand (p, p)
    |> Seq.except (Seq.singleton p)
    |> Seq.fold (fun c x -> 
        if Set.contains x s
        then c + 1
        else c) 0

let countNs2
    p
    s
    =
    expand2 (p, p)
    |> Seq.except (Seq.singleton p)
    |> Seq.fold (fun c x -> 
        if Set.contains x s
        then c + 1
        else c) 0


let phase
    s
    ((x, y, z), (x', y', z'))
    =
    let f s' x
        =
        let active = Set.contains x s
        let neighbours = countNs x s
        // printfn "p: %A active: %A ns: %A" x active neighbours 
        match (active, neighbours) with
        | (true, 2) -> s'
        | (true, 3) -> s'
        | (true, _) -> Set.remove x s'
        | (false, 3) -> Set.add x s'
        | _ -> s'
    
    space (x, y, z) (x', y', z')
    |> Seq.fold f s

let phase2
    s
    ((x, y, z, w), (x', y', z', w'))
    =
    let f s' x
        =
        let active = Set.contains x s
        let neighbours = countNs2 x s
        // printfn "p: %A active: %A ns: %A" x active neighbours 
        match (active, neighbours) with
        | (true, 2) -> s'
        | (true, 3) -> s'
        | (true, _) -> Set.remove x s'
        | (false, 3) -> Set.add x s'
        | _ -> s'
    
    space2 (x, y, z, w) (x', y', z', w')
    |> Seq.fold f s
    
let run
    inp
    ps
    =
    let (c, c', s) = parse inp
    let next ((x, y, z),(x', y', z'))
        =
        ((x - 1, y - 1, z - 1), (x' + 1, y' + 1, z' + 1))

    let grow x =
        let e = next x
        Some (x, e)

    Seq.unfold grow (c, c')
    |> Seq.skip 1
    |> Seq.take ps
    |> Seq.fold phase s

let run2
    inp
    ps
    =
    let (c, c', s) = parse2 inp
    let next ((x, y, z, w),(x', y', z', w'))
        =
        ((x - 1, y - 1, z - 1, w - 1), (x' + 1, y' + 1, z' + 1, w' + 1))

    let grow x =
        let e = next x
        Some (x, e)

    Seq.unfold grow (c, c')
    |> Seq.skip 1
    |> Seq.take ps
    |> Seq.fold phase2 s

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "ex.txt"
    let p = parse input
    
    printfn "p: %A" p

    printfn "z = 0:"
    
    0 // return an integer exit code