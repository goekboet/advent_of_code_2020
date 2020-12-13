// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let parse (s : string)
    =
    (s.[0], s.Substring(1) |> int)

let turnCw
    v
    =
    match v with
    | ( 1, 0) -> ( 0, 1) // E -> S
    | ( 0, 1) -> (-1, 0) // S -> W
    | (-1, 0) -> ( 0,-1) // W -> N
    | _       -> ( 1, 0) // N -> E

let turnCcw
    v
    =
    match v with
    | ( 1, 0) -> ( 0,-1) // E -> N
    | ( 0,-1) -> (-1, 0) // N -> W
    | (-1, 0) -> ( 0, 1) // W -> S
    | _       -> ( 1, 0) // S -> E

let turnDegrees
    (dir : int * int)
    (f : (int * int) -> (int * int))
    (deg : int)
    =
    Seq.init (deg / 90) id
    |> Seq.fold (fun v _ -> f v) dir


let ccw (x, y) = (-y,x)
let cw (x, y) = (y,-x)

let processInstr
    (instr)
    ((x, y), (x', y'))
    =
    // printfn "instr: %A boat: (%A, %A) (%A, %A)" instr x y x' y'
    match instr with
    | ('N', v) -> ((x, y - v), (x', y'))
    | ('S', v) -> ((x, y + v), (x', y'))
    | ('E', v) -> ((x + v, y), (x', y'))
    | ('W', v) -> ((x - v, y), (x', y'))
    | ('L', v) -> ((x, y), turnDegrees (x', y') turnCcw v )
    | ('R', v) -> ((x, y), turnDegrees (x', y') turnCw v)
    | ('F', v) -> ((x + (x' * v), y + (y' * v)), (x', y'))
    | _ -> ((x, y), (x', y'))

let processInstr2
    (instr)
    ((x, y), (x', y'))
    =
    match instr with
    | ('N', v) -> ((x, y), (x', y' + v))
    | ('S', v) -> ((x, y), (x', y' - v))
    | ('E', v) -> ((x, y), (x' + v, y'))
    | ('W', v) -> ((x, y), (x' - v, y'))
    | ('L', v) -> ((x, y), turnDegrees (x', y') ccw v )
    | ('R', v) -> ((x, y), turnDegrees (x', y') cw v)
    | ('F', v) -> ((x + (x' * v), y + (y' * v)), (x', y'))
    | _ -> ((x, y), (x', y'))
    
let run
   init
   is
   =
   Seq.fold (fun s i -> 
    let r = processInstr2 i s
    printfn "s: %A i: %A r: %A" s i r
    r) init is


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "ex.txt" // Call the function
    
    printfn "input %A" input
    0 // return an integer exit code