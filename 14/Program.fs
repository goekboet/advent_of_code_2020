// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let toBs
    (i : int64)
    =
    {35 .. -1 .. 0}
    |> Seq.map (fun x -> if pown 2L x &&& i <> 0L then '1' else '0')
    |> String.Concat

let fromBs
    (s : string)
    =
    s 
    |> Seq.indexed
    |> Seq.fold (fun s (i, x) -> if x = '1' then s + pown 2L (35 - i) else s) 0L

let applyMask
    (is : string)
    (ms : string)
    =
    Seq.zip (is |> int64 |> toBs) ms
    |> Seq.map (fun (i, m) -> if m = 'X' then i else m)
    |> String.Concat
    |> fromBs

let maskPattern = @"^mask = (.+)$"
let valuePattern = @"^mem\[([^]]+)] = (\d+)$"

let parse
    (is : string array) 
    =
    let parseMask m = 
        Regex.Match(m, maskPattern).Groups.[1].Value
        
    let parseValue v = 
        let m = Regex.Match(v, valuePattern)
        (m.Groups.[1].Value |> int64, m.Groups.[2].Value)

    let f ((m, vs'), vs) x
        =
        // printfn "((%A, %A), %A) %A" m vs' vs x
        if Regex.IsMatch(x, maskPattern)
        then ((parseMask x, []), (m, vs') :: vs)
        else ((m, parseValue x :: vs'), vs)

    let (p, ps) =
        is
        |> Seq.fold f (("", []),[])

    p :: ps 

let solve1
    (input : string array)
    =
    let f (m, vs) (r : Map<int64, int64>)  =
        Seq.foldBack (fun (k, v) s  -> 
            // printfn "f: (%A, %A) %A " s k v
            let r' = applyMask v m
            // printfn "r': %A" r'
            Map.add k r' s) vs r

    let p = parse input
    // printfn "p: %A" p
    
    let r = Seq.foldBack f p Map.empty
    // printfn "r: %A" r

    r |> Map.toSeq |> Seq.map snd |> Seq.reduce (+)

let applyMask2
    (addr : int64)
    (ms : string)
    =
    let is = toBs addr
    Seq.zip is ms
    |> Seq.map (fun (i, m) -> if m = '0' then i else m)
    |> String.Concat
    

let getFloats 
    (s : string)
    =
    let l = s |> Seq.filter (fun x -> x = 'X') |> Seq.length
    let k (n : int) = Convert.ToString(n, 2).PadLeft(l, '0')
    let rec f (xs : string) (xs' : string) (bs : string) =
        // printfn "bs: %A xs: %A xs': %A" bs xs xs'
        if xs = ""
        then xs'
        else 
            let c = xs.[0]
            if c = 'X'
            then f (xs.Substring(1)) (sprintf "%s%c" xs' bs.[0]) (bs.Substring(1)) 
            else f (xs.Substring(1)) (sprintf "%s%c" xs' c) bs
  
    { 0 .. (pown 2 l) - 1 } 
    |> Seq.map (fromBs << (f s "") << k) 

let parse2
    (is : string array) 
    =
    let parseMask m = 
        Regex.Match(m, maskPattern).Groups.[1].Value
        
    let parseValue v = 
        let m = Regex.Match(v, valuePattern)
        (m.Groups.[1].Value |> int64, m.Groups.[2].Value |> int64)

    let f ((m, vs'), vs) x
        =
        // printfn "((%A, %A), %A) %A" m vs' vs x
        if Regex.IsMatch(x, maskPattern)
        then ((parseMask x, []), (m, vs') :: vs)
        else ((m, parseValue x :: vs'), vs)

    let (p, ps) =
        is
        |> Seq.fold f (("", []),[])

    p :: ps 

let solve2 
    (input : string array)
    =
    let p = parse2 input 

    let f (m, vs) (r : Map<int64, int64>)  
        =
        Seq.foldBack (fun (k, v) s  -> 
            // printfn "f: (%A, %A) %A " s k v
            let k' = applyMask2 k m
            let ks = getFloats k'
            // printfn "r': %A" r'
            
            ks |> Seq.fold (fun s' x -> Map.add x v s') s) vs r

    let r = Seq.foldBack f p Map.empty

    r |> Map.toSeq |> Seq.map snd |> Seq.reduce (+)


          
    

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "ex.txt" // Call the function
    
    printfn "%A" input
    0 // return an integer exit code