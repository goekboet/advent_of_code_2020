// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let parse1 (s : string)
    =
    let rulePattern = @"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$"
    let parseRule (r : string)
        =
        let m = Regex.Match(r, rulePattern)
        ( m.Groups.[1].Value
        , (m.Groups.[2].Value |> int, m.Groups.[3].Value |> int)
        , (m.Groups.[4].Value |> int, m.Groups.[5].Value |> int)
        )

    let ps = s.Split("\n\n")
    
    let rules = 
        ps.[0].Split("\n") 
        |> Seq.map parseRule

    let myticket = 
        ps.[1].Split("\n") 
        |> Seq.skip 1 
        |> Seq.head 
        |> (fun x -> x.Split(",") |> Array.map int)

    let otherTickets = 
        ps.[2].Split("\n") 
        |> Seq.skip 1 
        |> Seq.map (fun x -> x.Split(",") |> Array.map int)
    
    (rules, myticket, otherTickets)

let solve1 (r, _, ts)
    =
    let rules =
        r
        |> Seq.collect (fun (_, a, b) -> [a;b])
    
    ts
    |> Seq.collect id
    |> Seq.filter (fun x -> rules |> Seq.exists (fun (a, b) -> x >= a && x <= b) |> not) 
    |> Seq.reduce (+)

let validTicket
    rs
    t
    =
    t
    |> Seq.forall(fun x -> Seq.exists (fun (_, (a,b), (a', b')) -> (x >= a && x <= b) || (x >= a' && x <= b')) rs)

let appliesToRule
    (col : int seq)
    (_, (a, b), (a', b'))
    =
    col
    |> Seq.forall (fun x -> (x >= a && x <= b) || (x >= a' && x <= b'))
    


let solve2
    input
    =
    let (rs, t, ts) = parse1 input
    let ts' = ts |> Seq.filter (validTicket rs)
    let f 
        (i : int, xs : int seq) 
        (s : Map<int, Set<string>>) 
        ((rule, a, b) : string * (int * int) * (int * int))
        =
        let applies = appliesToRule xs (rule, a, b)
        if applies
        then 
            Map.change i 
                (fun e -> 
                    match e with
                    | Some rs -> Set.add rule rs |> Some
                    | None -> [rule] |> Set.ofList |> Some)
                s
        else s

    let r =
        {0 .. t.Length - 1}
        |> Seq.map (fun i -> (i, Seq.map (fun (x : int array) -> x.[i]) ts'))
        |> Seq.fold (fun s x -> Seq.fold (f x) s rs) Map.empty

    let rec loop (s : Map<int, Set<string>>) (r' : (int * string) list)
        =
        if Map.isEmpty s
        then r'
        else
            let cs = 
                s 
                |> Map.toSeq 
                |> Seq.filter (fun (_, x) -> Set.count x = 1)
                |> Seq.map (fun (i, x) -> (i, x |> Set.toList |> List.head))
                |> Seq.toList

            let s' =
                cs
                |> List.fold (fun s' (i,x) -> Map.map (fun _ v -> Set.remove x v) s' |> Map.remove i) s
            
            let r'' = List.concat [r';cs]
            
            printfn "s': %A r'': %A" s' r''
            loop s' r''
    
    let r' = loop r []
    
    r' |> Seq.map (fun (i, x) -> (x, t.[i]))

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "ex.txt" // Call the function
    
    printfn "%A" input
    0 // return an integer exit code