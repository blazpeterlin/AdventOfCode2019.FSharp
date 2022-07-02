module D07.Solution

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators
open Aoc19.IntCode
open System

let env = P


let parse text =
    let tkns = env |> env2f |> f2tokens "," 
    let nums = tkns |> List.map int
    nums

let setupAmps (ic:IntCodeState) (code:string) =
    let amps = 
        [0..5] 
        |> List.map (fun idx -> 
            let sig1 = int64 code[idx]
            //Observable.
            //{ ic with input }
            ic
        )
    0

let solve1 text =
    let input = parse text

    let appendRnd str = 
        [0..5] |> List.map (fun num -> str + num.ToString())

    let candidates = 
        [0..5] 
        |> List.fold (fun strs _ -> strs |> List.map appendRnd |> List.concat) [""] 
        |> List.filter (fun str -> str.ToCharArray() |> Seq.distinct |> Seq.length |> fun l -> l=5)


        
    0

let solve2 text =
    let input = parse text
    0

let text = env |> env2f |> f2text
let res1 = text |> solve1
let res2 = text |> solve2

let finished = true