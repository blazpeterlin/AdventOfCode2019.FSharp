module Template.Solution

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators

let env = P
let tkns = env |> env2f |> f2tokens "," 
let nums = tkns |> List.map int


let finished = true