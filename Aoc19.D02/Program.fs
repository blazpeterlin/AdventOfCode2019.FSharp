module D02

open Aoc19
open Aoc19.Input

let env = P

type OpCode = ADD | MUL | END
type State = { codes:Map<int,int64>; pos:int }

let sig2oc (signal:int64) = 
    match (int signal) with 
    | 1 -> ADD | 2 -> MUL | 99 -> END 
    | x -> failwith ("Unknown opcode " + x.ToString())

let step { codes=codes; pos=pos } = 
    
    let arg0 = codes[pos]
    let oc = arg0 |> sig2oc
    if oc=END then None else 

    let newCodes = 
        match oc with
        | ADD -> codes |> Map.add (int codes[pos+3]) (codes[int codes[pos+1]]+codes[int codes[pos+2]])
        | MUL -> codes |> Map.add (int codes[pos+3]) (codes[int codes[pos+1]]*codes[int codes[pos+2]])
        | _ -> codes
    Some { codes=newCodes; pos=pos+4 }


let parse text =
    let tkns = text |> text2tokens ","
    let nums = tkns |> List.map int64
    let m = nums |> List.indexed |> Map.ofList
    m

let m0 = env |> env2f |> f2text |> parse

let map2state m = { codes=m; pos=0 }

let parameterize p1 p2 map = map |> Map.add 1 p1 |> Map.add 2 p2
let runUntilHalt st = st |> Common.unfold step |> Seq.last |> fun st -> st.codes
let getResult (m:Map<int,int64>) = m[0]

let res1 = m0 |> parameterize 12L 2L |> map2state |> runUntilHalt |> getResult


let res2 = 
    Seq.allPairs [0L..99L] [0L..99L]
    |> Seq.map (fun (x,y) -> m0 |> parameterize x y |> map2state |> runUntilHalt |> fun m -> (x,y,getResult m))
    |> Seq.filter (fun (x,y,z) -> z=19690720)
    |> Seq.head
    |> fun (x,y,z) -> 100L*x+y

let res2_alternative =
    seq {
        for x in [0L..99L] do
            for y in [0L..99L] do
                let z = m0 |> parameterize x y |> map2state |> runUntilHalt |> getResult
                if z=19690720 then yield 100L*x+y 
    }
    |> Seq.head

let finished = true