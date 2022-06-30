module D05.IntCode

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators

type InputStream = int64 seq
type OutputStream = int64 -> unit

type OpCode = ADD | MUL | END | INPUT | OUTPUT | JMPT | JMPF | LT | EQ
type IntCodeState = { codes:Map<int,int64>; pos:int; input:InputStream; output:OutputStream }

let sig2opcode (signal:int64) = 
    match (int signal)%100 with 
    | 1 -> ADD 
    | 2 -> MUL 
    | 3 -> INPUT
    | 4 -> OUTPUT
    | 5 -> JMPT
    | 6 -> JMPF
    | 7 -> LT
    | 8 -> EQ
    | 99 -> END 
    | x -> failwith ("Unknown opcode " + x.ToString())

let sig2argpos (signal:int64) =
    let s = int signal
    let folder num _ = num/10
    let modes = [1..2] |> Seq.scan folder (s/100) |> Seq.map (fun x -> x%10) |> List.ofSeq
    let listArgPos = 
        modes 
        |> List.mapi (fun i elt ->
                        match elt with
                        | 0 -> fun (pos:int) (codes:Map<int,int64>) -> int codes[pos+i+1] 
                        | 1 -> fun pos codes -> pos+i+1
                    )
    listArgPos

let step (st:IntCodeState) = 
    let { codes=codes; pos=pos; input=input; output=output; } = st

    let opcode = codes[pos] |> sig2opcode
    let [argpos1;argpos2;argpos3] = codes[pos] |> sig2argpos |> List.map (fun f -> fun _ -> f pos codes)
    if opcode=END then None else 

    let newCodes,newPos = 
        match opcode with
        | ADD -> codes |> Map.add (int codes[pos+3]) (codes[argpos1()]+codes[argpos2()]) , pos+4
        | MUL -> codes |> Map.add (int codes[pos+3]) (codes[argpos1()]*codes[argpos2()]) , pos+4
        | INPUT -> 
            let inp = input |> seqPop
            codes |> Map.add (int codes[pos+1]) inp , pos+2
        | OUTPUT -> codes[argpos1()] |> output ; codes , pos+2
        | JMPT -> 
            let jmpPos = if codes[argpos1()]<>0 then (int codes[argpos2()]) else pos+3
            codes, jmpPos
        | JMPF -> 
            let jmpPos = if codes[argpos1()]=0 then (int codes[argpos2()]) else pos+3
            codes, jmpPos
        | LT -> 
            let v = if codes[argpos1()] < codes[argpos2()] then 1 else 0
            codes |> Map.add (int codes[pos+3]) v , pos+4
        | EQ -> 
            let v = if codes[argpos1()] = codes[argpos2()] then 1 else 0
            codes |> Map.add (int codes[pos+3]) v , pos+4
        | _ -> codes,pos
    Some { st with codes=newCodes; pos=newPos }


let parseIntCode text =
    let tkns = text |> text2tokens ","
    let nums = tkns |> List.map int64
    let m = nums |> List.indexed |> Map.ofList
    { codes=m; pos=0; input=Seq.empty; output=fun _->() }

let initStreams ic inputSeq =
    let q = System.Collections.Generic.Queue<int64>()
    let ic0 = { ic with input = inputSeq ; output = q.Enqueue }
    (ic0, q)

//let parameterize p1 p2 map = map |> Map.add 1 p1 |> Map.add 2 p2
let runUntilHalt st = st |> Common.unfold step |> Seq.last |> fun st -> st.codes
//let getResult (m:Map<int,int64>) = m[0]