module D05.Solution

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators

open IntCode

let env = P
let parse = parseIntCode

let solve1 text = 
    let ic_init = text |> parse

    let q = System.Collections.Generic.Queue<int64>()
    let (ic0,q) = initStreams ic_init (seq [ 1 ])
    let icf = ic0 |> runUntilHalt |> ics2codes

    let output = q |> q2seq |> List.ofSeq
    if output |> List.take (output.Length-1) |> List.exists (fun x -> x<>0) then failwith "intcode test fail" else

    output |> List.last


let solve2 text = 
    let ic_init = text |> parse

    let q = System.Collections.Generic.Queue<int64>()
    let (ic0,q) = initStreams ic_init (seq [ 5 ])
    let icf = ic0 |> runUntilHalt |> ics2codes

    let output = q.consumeElt()

    output

let text = env |> env2f |> f2text
let res1 = text |> solve1
let res2 = text |> solve2

let finished = true