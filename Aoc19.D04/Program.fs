module D04.Solution

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators

let env = P

let parse text =
    let tkns = env |> env2f |> f2tokens "-" 
    let nums = tkns |> List.map int
    nums
    
let isCandidate num =
    let windows = num.ToString().ToCharArray() |> Seq.map (fun x -> x.ToString()) |> Seq.map int |> Seq.windowed 2 |> Seq.cache 
    let cnd1 = windows |> Seq.exists (fun [|x;y|] -> x = y)
    let cnd2 = windows |> Seq.forall (fun [|x;y|] -> x <= y)
    cnd1 && cnd2

let isCandidate2 num =
    let digits = num.ToString().ToCharArray() |> Seq.map (fun x -> x.ToString()) |> Seq.map int
    let wnd2 = digits |> Seq.windowed 2 |> Seq.cache 
    let wnd3 = digits |> Seq.windowed 3 |> Seq.cache 
    let allRep2 = wnd2 |> Seq.filter (fun [|x;y|] -> x = y) |> Seq.map (Seq.head)
    let allRep3 = wnd3 |> Seq.filter (fun [|x;y;z|] -> x = y && y = z) |> Seq.map (Seq.head)
    let cnd1 = allRep2 |> Seq.except allRep3 |> Seq.isEmpty |> not
    let cnd2 = wnd2 |> Seq.forall (fun [|x;y|] -> x <= y)
    cnd1 && cnd2

let solve1 text =
    let inp = text |> parse
    let candidates = seq { inp[0] .. inp[1] } |> Seq.filter isCandidate
    candidates |> Seq.length
    

let solve2 text =
    let inp = text |> parse
    let candidates = seq { inp[0] .. inp[1] } |> Seq.filter isCandidate2
    candidates |> Seq.length

let res1 = env |> solve1
let res2 = env |> solve2

let finished = true