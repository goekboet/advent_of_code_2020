// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let ex = [3;8;9;1;2;5;4;6;7]
let input = [7;1;2;6;4;3;5;8;9]

let toCircle xs n
    =
    let start = List.head xs
    let e = List.last xs
    let h = List.max xs
 
    if n > xs.Length
    then
        let m =
            { 1 .. n}
            |> Seq.map (fun x -> if (x - 1) < xs.Length then xs.[x - 1] else x)
            |> Seq.pairwise
            |> Map.ofSeq

        (Map.add n start m, n, start)
    else
        let m = Seq.pairwise xs |> Map.ofSeq

        (Map.add e start m, h, start)

let rec cMove (m : Map<int,int>) (c : int) (h: int) (i : int) (t : int)
    =
    // printfn "-- move %i --" i
    // printfn "%A" m
    let rec take m' c' r =
        let n = Map.find c' m'
        if List.length r >= 3
        then (n,r)
        else
            take m' n (n :: r)
    
    let rec dst c' xs
        =
        let c'' = if c' < 1 then h else c'
        if List.contains c'' xs
        then dst (c'' - 1) xs
        else c''

    if i > t
    then m
    else
        let (n, xs) = take m c []
        // printfn "pick up: %A c: %A n: %A" xs c n
        let d = dst (c - 1) xs
        // printfn "destination: %A" d
        let d' = Map.find d m
        let xf = List.last xs
        let xl = List.head xs
        
        let m' = Map.add d xf m
        let m'' = Map.add c n m'
        let m''' = Map.add xl d' m''

        cMove m''' n h (i + 1) t

let solve2 xs
    =
    let (m, h, s) = toCircle xs 1000000
    let r = cMove m s h 1 10000000
    let n1 = Map.find 1 r
    let n2 = Map.find n1 r

    int64 n1 * int64 n2


let rec move (n : int) (t : int) (cups : int list) (current : int)
    =
    if t < n
    then cups
    else
        // printfn "-- move %i --" n
        let c = cups.[current]
        // printfn "cups: %A" (cups |> List.map (fun x -> if x = c then sprintf "(%i)" x else string x))
        let taken = 
            { (current + 1) .. (current + 3) }
            |> Seq.map (fun x -> cups.[x % cups.Length])
            |> Seq.toList
        // printfn "pick up: %A" taken

        let rec d d' =
            let d'' = if d' < 1 then 9 else d'
            if List.contains d'' taken
            then d (d'' - 1)
            else
                d''

        let d' = d (c - 1)
        let d'i = List.findIndex (fun x -> x = d') cups + 1
        let notTaken' =
            Seq.init 9 (fun i -> (d'i + i) % List.length cups)
            |> Seq.map (fun i -> cups.[i])
            |> Seq.except (d' :: taken)
            |> Seq.toList

        // printfn "destination: %i" d'
        
        let cups' = d' :: taken @ notTaken'
        let current' = List.findIndex (fun x -> x = c) cups'
        move (n + 1) t cups' ((current' + 1) % cups.Length)

let score cups
    =
    let i1 = List.findIndex (fun x -> x = 1) cups + 1
    let r = 
        Seq.init 8 (fun i -> (i + i1) % cups.Length)
        |> Seq.map (fun i -> cups.[i])

    String.Join("", r)

let solve1 cups
    =
    let cups' = move 1 100 cups 0
    printfn "%A" (score cups')



[<EntryPoint>]
let main argv =
    printfn "let go..."
    0 // return an integer exit code