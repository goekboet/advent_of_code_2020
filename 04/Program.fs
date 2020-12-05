// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let separatorPattern = @"([^ |\n]+)[ |\n]*"
let keyPattern = @"^([^:]+):(.+)$"

let getEntries (l : string)
    =
    Regex.Matches(l, separatorPattern)
    |> Seq.map (fun x -> x.Groups.[1].Value)

let getKeys (entries : string seq)
    =
    let getEntry (e : string)
        =
        let m = Regex.Match(e, keyPattern)
        (m.Groups.[1].Value, m.Groups.[2].Value)

    Seq.map getEntry entries

let keys = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid";"cid"] |> Set.ofList

let fieldsPresent (entries : (string * string) seq) 
    =
    let diff = 
        entries
        |> Seq.map fst 
        |> Set.ofSeq 
        |> Set.difference keys

    Set.isEmpty diff || (Set.remove "cid" diff |> Set.isEmpty) 
  
let valid (e : string * string) 
    =
    let fourDigits = @"^\d{4}$"
    let height s = 
        let m = Regex.Match(s, @"^(\d+)(cm|in)$")
        if (m.Groups.Count = 3)
        then (m.Groups.[1].Value |> int, m.Groups.[2].Value)
        else (0, "")

    let hair = @"^#[0-9a-f]{6}$"
    let eyes = @"^(amb|blu|brn|gry|grn|hzl|oth)$"
    let pid = @"^[0-9]{9}$"
    
    match e with
    | ("byr", v) -> Regex.IsMatch(v, fourDigits) && int v >= 1920 && int v <= 2002
    | ("iyr", v) -> Regex.IsMatch(v, fourDigits) && int v >= 2010 && int v <= 2020
    | ("eyr", v) -> Regex.IsMatch(v, fourDigits) && int v >= 2020 && int v <= 2030
    | ("hgt", v) -> 
        let (n, u) = height v
        if u = ""
        then false
        else (u = "in" && n >= 59 && n <= 76) || (u = "cm" && n >= 150 && n <= 193)
    | ("hcl", v) -> Regex.IsMatch(v, hair)
    | ("ecl", v) -> Regex.IsMatch(v, eyes)
    | ("pid", v) -> Regex.IsMatch(v, pid)
    | ("cid", _) -> true
    | _ -> false

[<EntryPoint>]
let main argv =
    
    let r = 
        File.ReadAllText("input.txt").Split "\n\n"
        |> Seq.map (getKeys << getEntries)
        |> Seq.filter (fieldsPresent << Seq.filter valid)
        |> Seq.length

    printfn "r: %i" r
    0 // return an integer exit code