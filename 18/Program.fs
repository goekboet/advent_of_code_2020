// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

// Define a function to construct a message to print

let parse (s : string) = s.Replace(" ", "")

let isOp c' = List.contains c' ['+';'*']
let isLp c' = c' = '('
let isRp c' = c' = ')'

let toPostfix
    (ops, out)
    c
    =
    //printfn "c: %A state: %A" c (ops, out)
    let rec keepOps (ops' : char list, out' : char list) op
        =
        if List.isEmpty ops'
        then (op :: ops', out')
        else 
            let op' = List.head ops'
            if isLp op' || isRp op'
            then (op :: ops', out')
            else keepOps (List.tail ops', op' :: out') op
        
    let rec keepParens (ops' : char list, out' : char list)
        =
        if List.isEmpty ops'
        then (ops',out')
        else 
            let op' = List.head ops'
            if isLp op'
            then (List.tail ops', out')
            else keepParens (List.tail ops', op' :: out')

    if isOp c
    then keepOps (ops, out) c
    else if isLp c
    then (c :: ops, out)
    else if isRp c
    then keepParens (ops, out)
    else (ops, c :: out)



let toPostfix2
    (ops, out)
    c
    =
    // printfn "c: %A state: %A" c (ops, out)
    let rec keepOps (ops' : char list, out' : char list) op
        =
        if List.isEmpty ops'
        then (op :: ops', out')
        else 
            let op' = List.head ops'
            if isLp op' || isRp op'
            then (op :: ops', out')
            else if op = '+' && op' = '*'
            then (op :: ops', out')
            else keepOps (List.tail ops', op' :: out') op
        
    let rec keepParens (ops' : char list, out' : char list)
        =
        if List.isEmpty ops'
        then (ops',out')
        else 
            let op' = List.head ops'
            if isLp op'
            then (List.tail ops', out')
            else keepParens (List.tail ops', op' :: out')

    if isOp c
    then keepOps (ops, out) c
    else if isLp c
    then (c :: ops, out)
    else if isRp c
    then keepParens (ops, out)
    else (ops, c :: out)

let postFixCalculator
    (c : char)
    (stack : int64 list)
    =
    let toLong c' = Char.GetNumericValue(c') |> int64
    let applyOp c' a b
        =
        match c' with
        | '*' -> a * b
        | _   -> a + b
    // printfn "c: %A stack: %A" c stack
    if isOp c
    then
        match stack with
        | a :: b :: r -> (applyOp c a b) :: r
        | _ -> []
    else
        (toLong c :: stack)

let eval
    (s : string)
    =
    let p = parse s
    let (ns, ns') = Seq.fold toPostfix ([],[]) p
    let os = List.concat [ns;ns']
    printfn "os: %A" os
    let r = List.foldBack postFixCalculator os []     

    List.head r

let eval2
    (s : string)
    =
    let p = parse s
    let (ns, ns') = Seq.fold toPostfix2 ([],[]) p
    let os = List.concat [ ns |> List.rev ; ns' ]
    printfn "os: %A" os
    let r = List.foldBack postFixCalculator os []     

    List.head r

let solve h
    =
    let inp = File.ReadAllLines h

    inp
    |> Seq.map eval2
    |> Seq.reduce (+)

[<EntryPoint>]
let main argv =
    // List.foldBack
    // Char.GetNumericValue
    0 // return an integer exit code