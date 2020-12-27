// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

type Tile =
    { Id : int
      Data : string array
      North : string
      East : string
      South : string
      West : string
      Rotated : int
      Flipped : bool
    }

let nullTile = { Id = 0; Data = [||]; North = ""; East = ""; South = ""; West = ""; Rotated = 0; Flipped = false }

let parse (p : string) =
    let inp = File.ReadAllText p
    let parseTile (s : string)
        =
        let parseId id =
            let m = Regex.Match(id, @"^Tile (\d+):$")
            m.Groups.[1].Value |> int

        let parseBorders (b : string array)
            =
            let w = 10
            let h = 10

            let n = b.[0]
            let e = Array.map (fun (x : string) -> x.[w - 1]) b |> String
            let s = b.[h - 1]
            let w = Array.map (fun (x : string) -> x.[0]) b |> String

            (n, e, s, w)
        
        let t = s.Split "\n" |> Array.filter (fun x -> x.Length > 0)

        let id = parseId t.[0]
        let data = t.[1 ..]
        let (n, e, s, w) = parseBorders data
        
        { Id = id; Data = data; North = n; East = e; South = s; West = w; Rotated = 0; Flipped = false }
        
    inp.Split "\n\n"
    |> Seq.map parseTile

let rotateOnce (t : Tile)
    =
    { t with 
        North = Array.rev (t.West.ToCharArray()) |> String;
        East = t.North;
        South = Array.rev (t.East.ToCharArray()) |> String;
        West = t.South;
        Rotated = (t.Rotated + 1) % 4
    }

let flipOnce (t : Tile)
    =
    { t with
        North = Array.rev (t.North.ToCharArray()) |> String;
        East = t.West;
        South = Array.rev (t.South.ToCharArray()) |> String;
        West = t.East;
        Flipped = t.Flipped |> not
    }

let allOrientations (t : Tile)
    =
    let g (n, t') =
        if n = 8
        then None
        else if n = 3
        then
            let t'' = rotateOnce t' 
            Some (t', (n + 1, flipOnce t''))
        else
            let t'' = rotateOnce t'
            Some (t', (n + 1, t''))

    Seq.unfold g (0,t)

let lay (ts : Tile seq) (t : Tile) 
    =
    let n = Seq.filter (fun x -> x.South = t.North) ts
    let e = Seq.filter (fun x -> x.West = t.East) ts
    let s = Seq.filter (fun x -> x.North = t.South) ts
    let w = Seq.filter (fun x -> x.East = t.West) ts

    (t, (n,e,s,w))

    

let solve1
    (p : string)
    =
    let ts = parse p
    let ids = Seq.map (fun x -> x.Id) ts
    let os = Seq.collect allOrientations ts

    let isCorner (t, (n,e,s,w))
        =
        match (Seq.isEmpty n, Seq.isEmpty e, Seq.isEmpty s, Seq.isEmpty w) with
        | (true, false, false, true) -> true //upper-left
        | (true, true, false, false) -> true //upper-right
        | (false, true, true, false) -> true //lower-right
        | (false, false, true, true) -> true //lower-left
        | _ -> false

    let r = 
        ids
        |> Seq.collect (
                fun x ->  
                let tos = Seq.filter (fun t -> t.Id = x) os
                let cs = Seq.filter (fun t -> t.Id <> x) os
                tos |> Seq.map (lay cs))
        |> Seq.filter isCorner

    printfn "%A" (r |> Seq.map (fun (t, _) -> int64 t.Id) |> Seq.distinct |> Seq.reduce (*))
    ()

let flip ((x, y), c) = ((9 - x, y), c)
let rotate ((x, y), c) = ((9 - y, x), c)

let parseData (t : Tile)
    =
    let cs = 
        t.Data
        |> Seq.mapi (fun y s -> Seq.mapi (fun x c -> ((x, y), c)) s)
        |> Seq.concat

    
    let stripBorder cs' =
        Seq.allPairs {1 .. 8} {1 .. 8}
        |> Seq.map (fun (x, y) -> ((x - 1, y - 1), Seq.find (fun ((x', y'), c) -> x' = x && y' = y) cs' |> snd))

    let cs' = if t.Flipped then Seq.map flip cs else cs
    let cs'' =
        Seq.init t.Rotated id
        |> Seq.fold (fun t _ -> Seq.map rotate t) cs'
    
    stripBorder cs''

let print cs w
        =     
        Seq.init w
            (fun y -> { 0 .. w - 1 } |> Seq.map (fun x -> if Set.contains (x, y) cs then '#' else '.') |> Array.ofSeq |> String)
        |> Seq.iter (printfn "%s")

let seamonster = 
    [ "                  # "
    ; "#    ##    ##    ###"
    ; " #  #  #  #  #  #   "
    ]
    |> Seq.mapi (fun y s -> Seq.mapi (fun x c -> ((x,y), c)) s)
    |> Seq.concat
    |> Seq.filter (fun (_, c) -> c = '#')
    |> Seq.map fst
    |> Set.ofSeq

let monsterW = 20
let monsterH = 3

let monsterPs
    (w : int)
    =
    Seq.allPairs 
        { 0 .. w - 1 - monsterW } 
        { 0 .. w - 1 - monsterH }
        |> Seq.map (fun (x', y') -> Set.map (fun (x, y) -> (x + x', y + y')) seamonster)
    

let solve2 (p : string)
    =
    let ts = parse p |> Array.ofSeq
    let w = ts.Length |> float |> sqrt |> int
    let ps : (Map<int * int, Tile option>) =
        { 0 .. ts.Length - 1 }
        |> Seq.map (fun i -> ((i % w, i / w), None))
        |> Map.ofSeq

    let next ps' (x, y) 
        =
        [(1, 0); (0, 1);(-1, 0);(0, -1)]
        |> List.tryPick (
            fun (x', y') -> 
                match Map.tryFind (x + x', y + y') ps' with
                | Some None -> Some (x + x', y + y')
                | _ -> None)
                
    let ids = Seq.map (fun x -> x.Id) ts |> List.ofSeq
    let os = 
        Seq.collect allOrientations ts 
        |> List.ofSeq

    let map = 
        ids
        |> Seq.collect (
                fun x ->  
                let tos = Seq.filter (fun t -> t.Id = x) os
                let cs = Seq.filter (fun t -> t.Id <> x) os
                tos |> Seq.map (lay cs))
        |> List.ofSeq

    let getPred ps' (x, y) 
        =         
        fun (t : Tile, (n : Tile seq,e : Tile seq,s : Tile seq,w : Tile seq)) -> 
            let nP = 
                match Map.tryFind (x, y - 1) ps' with
                | None -> Seq.isEmpty n
                | Some None -> Seq.isEmpty n |> not
                | Some (Some t') -> Seq.contains t' n

            let eP =
                match Map.tryFind (x + 1, y) ps' with
                | None -> Seq.isEmpty e
                | Some None -> Seq.isEmpty e |> not
                | Some (Some t') -> Seq.contains t' e

            let sP =
                match Map.tryFind (x, y + 1) ps' with
                | None -> Seq.isEmpty s
                | Some None -> Seq.isEmpty s |> not
                | Some (Some t') -> Seq.contains t' s

            let wP =
                match Map.tryFind (x - 1, y) ps' with
                | None -> Seq.isEmpty w
                | Some None -> Seq.isEmpty w |> not
                | Some (Some t') -> Seq.contains t' w
            
            nP && eP && sP && wP

    let rec place map' ps' p
        =
        match p with
        | None -> ps'
        | Some n ->
            let pred = getPred ps' n
            let (t, _) = List.find pred map'
            let ps'' = Map.add n (Some t) ps'
            place map' ps'' (next ps'' n) 
        
    let ps' = place map ps (Some (0,0))

    let r = 
        [(0,0);(0,w - 1);(w - 1,0);(w - 1,w - 1)]
        |> List.map (fun x -> Map.find x ps' |> Option.map (fun t -> int64 t.Id) |> Option.defaultValue 0L)
        |> List.reduce (*)

    printfn "%A" r

    // printfn "ps': %A" ps'
    let ps'' =
        ps'
        |> Map.toSeq
        |> Seq.collect (fun ((x, y), t) -> 
            Option.defaultValue nullTile t 
            |> parseData
            |> Seq.map (fun ((x', y'), c) -> ((x * 8 + x', y * 8 + y'), c))
            )
        |> Seq.filter (fun (_, c) -> c = '#')
        |> Seq.map fst
        |> Set.ofSeq

    let flip' (x, y) = (9 - x, y)
    let rotate' (x, y) = (9 - y, x)

    let allPs (xs : Set<int * int>)
        =
        let g (n, xs') =
            if n = 8
            then None
            else if n = 3
            then
                let xs'' = Set.map rotate' xs' 
                Some (xs', (n + 1, Set.map flip' xs''))
            else
                let xs'' = Set.map rotate' xs'
                Some (xs', (n + 1, xs''))

        Seq.unfold g (0,xs)

    let sea = allPs ps''
    let monsters = monsterPs (w * 8)

    let scanSea
        rs
        sea
        =
        let scanSea'
            rs'
            m
            =
            if Set.isSubset m sea
            then Set.union m rs'
            else rs'

        monsters
        |> Seq.fold scanSea' rs

    let r2 =
        sea
        |> Seq.fold scanSea Set.empty

    let total = Set.count ps''
    let monsterCount = Set.count r2
    printfn "w: %A t: %A m: %A r: %A" (w * 8) total monsterCount (total - monsterCount)
    0

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "ex.txt"
    0 // return an integer exit code