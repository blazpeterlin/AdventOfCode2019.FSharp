module Template.Solution

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators

let env = P

let parse text =
    let tkns = env |> env2f |> f2tokens "," 
    let nums = tkns |> List.map int
    nums

let solve1 text =
    let input = parse text
    0

let solve2 text =
    let input = parse text
    0

let text = env |> env2f |> f2text
let res1 = text |> solve1
let res2 = text |> solve2

let finished = true