// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions

type Entry = 
    { Min : int
      Max : int
      Char : char
      Pwd : string
    }

let pattern = Regex(@"^(\d+)-(\d+) (.): (.+)$", RegexOptions.Compiled)

let parseEntry (l : string)
    =
    let m = pattern.Match l

    { Min = m.Groups.[1].Value |> int; 
      Max = m.Groups.[2].Value |> int; 
      Char = m.Groups.[3].Value |> char; 
      Pwd = m.Groups.[4].Value 
    }

let validateEntry (e : Entry)
    =
    let r = [ e.Pwd.[e.Min - 1] = e.Char; e.Pwd.[e.Max - 1] = e.Char ] 

    (List.filter id r |> List.length) = 1

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input.txt"

    let r =
        Seq.map parseEntry input
        |> Seq.filter validateEntry
        |> Seq.length

    printfn "%i" r
    0 // return an integer exit code
