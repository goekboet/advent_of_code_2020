// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

// Define a function to construct a message to print
let parse
    (input : string array)
    =
    let n = input.[0] |> int
    let xs = 
        input.[1].Split(",")
        |> Seq.filter (fun x -> x <> "x")
        |> Seq.map int

    (n, xs)

let solve1
    (n, xs)
    =
    let (id, w) =
        xs
        |> Seq.map (fun x -> (x, x - (n % x)))
        |> Seq.minBy snd

    w * id

let parse2
    (s : string array)
    =
    let f x (m, (ms, rs))
        =
        if x = "x"
        then (m + 1L, (ms, rs))
        else (m + 1L, (m :: ms, int64 x :: rs))

    Seq.foldBack f (s.[1].Split(",")) (0L, ([], [])) 
    |> snd

let rec gcd (a : int64) (b : int64) =
  if b = 0L
    then abs a
  else gcd b (a % b)

let MI n g =
  let rec fN n i g e l a =
    match e with
    | 0L -> g
    | _ -> let o = n/e
           fN e l a (n-o*e) (i-o*l) (g-o*a) 
  (n+(fN n 1L 0L g 0L 1L))%n

let CD (n : int64 seq) (g : int64 seq) =
  match Seq.fold(fun n g->if (gcd n g)=1L then n*g else 0L) 1L g with
  |0L -> None
  |fN-> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(MI g ((fN/g)%g))) 0L n g)%fN)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "ex.txt"

    printfn "%A" input
    0 // return an integer exit code