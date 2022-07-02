module D02.Solution

open Aoc19
open Aoc19.Input
open Aoc19.IntCode

let env = P

let parse text = parseIntCode text

let m0 = env |> env2f |> f2text |> parse

let parameterize p1 p2 (icstate:IntCodeState) =
    let nextCodes = icstate.codes |> Map.add 1 p1 |> Map.add 2 p2
    { icstate with codes=nextCodes }

let runUntilHalt st = st |> Common.unfold step |> Seq.last |> fun st -> st.codes
let getResult (m:Map<int,int64>) = m[0]

let res1 = m0 |> parameterize 12L 2L |> runUntilHalt |> getResult


let res2 = 
    Seq.allPairs [0L..99L] [0L..99L]
    |> Seq.map (fun (x,y) -> m0 |> parameterize x y |> runUntilHalt |> fun m -> (x,y,getResult m))
    |> Seq.filter (fun (x,y,z) -> z=19690720)
    |> Seq.head
    |> fun (x,y,z) -> 100L*x+y

let res2_alternative =
    seq {
        for x in [0L..99L] do
            for y in [0L..99L] do
                let z = m0 |> parameterize x y |> runUntilHalt |> getResult
                if z=19690720 then yield 100L*x+y 
    }
    |> Seq.head

let finished = true