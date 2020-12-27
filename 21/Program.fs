// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.RegularExpressions
open System.IO

let parse p
    =
    let entryPattern = @"^(?:([^ (]+) )+\(contains (?:([^,]+)[, |\)])+$"
    
    let parseEntry s
        =
        let m = Regex.Match(s, entryPattern)
        
        let ingredients = 
            m.Groups.[1].Captures 
            |> Seq.map (fun x -> x.Value.Trim())
            |> Set.ofSeq

        let allergens =
            m.Groups.[2].Captures
            |> Seq.map (fun x -> x.Value.Trim())
            |> Set.ofSeq

        (ingredients, allergens) 
    
    let entries =
        File.ReadAllLines p
        |> Seq.map parseEntry

    let allIngredients =
        entries
        |> Seq.map fst
        |> Seq.fold Set.union Set.empty

    let allAllergens =
        entries
        |> Seq.map snd
        |> Seq.fold Set.union Set.empty

    (allIngredients, allAllergens, entries)

type ResultMap = Map<string, Set<string>>

let initRs ss =
    ss
    |> Seq.map (fun x -> (x, Set.empty))
    |> Map.ofSeq

let solve1 p
    =
    let (allIs, allAs, es) = parse p
    let rs = initRs allIs

    let rec f es' rs'
        =
        match List.tryHead es' with
        | None -> rs'
        | Some (eIs, eAs) ->
            let rs'' = 
                Set.difference allIs eIs
                |> Seq.fold 
                    (fun s x -> 
                        let re = Map.find x s
                        let re' = Set.union re eAs
                        Map.add x re' s) 
                    rs'
            f (List.tail es') rs''

    let rs' = f (Seq.toList es) rs
    let allergenFree = 
        rs' 
        |> Map.toSeq 
        |> Seq.filter (fun (_, v) -> v = allAs) 
        |> Seq.map fst
        |> Set.ofSeq

    let r =
        es
        |> Seq.sumBy (fun (x, _) -> Set.intersect x allergenFree |> Set.count)
        

    printfn "free: %A count: %A" allergenFree r
    ()

let solve2 p
    =
    let (allIs, allAs, es) = parse p
    let rs = initRs allIs

    let rec f es' rs'
        =
        match List.tryHead es' with
        | None -> rs'
        | Some (eIs, eAs) ->
            let rs'' = 
                Set.difference allIs eIs
                |> Seq.fold 
                    (fun s x -> 
                        let re = Map.find x s
                        let re' = Set.union re eAs
                        Map.add x re' s) 
                    rs'
            f (List.tail es') rs''

    let rs' = f (Seq.toList es) rs

    let rec findAllergen es' (rs'' : (string * string) list)
        =
        match List.tryHead es' with
        | None -> rs''
        | Some (e, eAs) ->
            let d = Set.difference allAs eAs
            if d = Set.empty
            then findAllergen (List.tail es') rs''
            else if Set.count d = 1
            then
                let a = Set.toList d |> List.head
                let rs''' = (e, a) :: rs''
                let es'' = List.tail es' |> List.map (fun (eI, eAs') -> (eI, Set.add a eAs'))
                findAllergen es'' rs'''
            else
                let es'' = (List.tail es') @ [(e, eAs)]
                findAllergen es'' rs''

    let rs'' = findAllergen (Map.toList rs') []
    let r2 = String.Join(',', rs'' |> List.sortBy snd |> List.map fst)
    printfn "%A" r2
    ()

[<EntryPoint>]
let main argv =
    0 // return an integer exit code