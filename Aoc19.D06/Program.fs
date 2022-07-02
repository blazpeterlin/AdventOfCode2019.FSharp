module D06.Solution

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators

let env = P

let flip (a,b) = (b,a)
let parse text = text |> text2lines |> List.map (fun ln -> ln |> text2tokens ")" |> fun (x::y::_) -> (x,y)) |> List.map flip |> Map.ofSeq

let solve1 text = 
    let dict = parse text
    
    let getOrbitLengths key =
        key |> Common.unfold dict.TryFind |> Seq.length

    dict.Keys |> Seq.map getOrbitLengths |> Seq.sum


let solve2 text =
    let dict = parse text
    let getOrbits key =
        key |> Common.unfold dict.TryFind |> List.ofSeq |> List.mapi (fun i elt -> elt,(i))

    let dictOrbitsYOU = getOrbits "YOU" |> Map.ofSeq
    let dictOrbitsSAN = getOrbits "SAN" |> Map.ofSeq
    
    let joinedKey = dictOrbitsSAN |> Map.toSeq |> Seq.sortBy snd |> Seq.map fst |> Seq.find (dictOrbitsYOU.ContainsKey)

    let orbitalTransfers = dictOrbitsYOU[joinedKey] + dictOrbitsSAN[joinedKey]
    orbitalTransfers
    
let text =  env |> env2f |> f2text
let res1 = text |> solve1
let res2 = text |> solve2

let finished = true