module D03

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators

let env = P


let move dir (st,pos,numSteps) _ = 
    let newPos = pos +.. dir
    let st2 = st |> Map.add newPos (match st.TryFind newPos with | Some v -> v | None -> numSteps+1)
    (st2,newPos,numSteps+1)

let moveN (st,pos,numSteps) (dir,num) =
     Seq.init num id |> Seq.fold (move dir) (st,pos,numSteps)

let collisions (m1:Map<int*int,int>) (m2:Map<int*int,int>) =
    let lst = List.append (m1 |> Map.toList) (m2 |> Map.toList)
    let mTotal = lst |> List.groupBy fst |> List.map (fun (x,y) -> (x,y |> List.map snd))
    mTotal |> Seq.filter ( fun (k,v) -> v.Length>1 ) |> Seq.sortBy (fun (k,v) -> List.sum v) |> List.ofSeq
    
let parseLine line = 
    line 
    |> text2tokens ","
    |> List.map (fun t -> 
        let dir = match t[0] with | 'R' -> (1,0) | 'L' -> (-1,0) | 'U' -> (0,1) | 'D' -> (0,-1)
        let n = t.Substring(1) |> int
        (dir, n)
    )
let parse text =
    let lines = text |> text2lines
    parseLine lines[0], parseLine lines[1]
    


let runWire ins = ins |> List.fold moveN (Map.empty, (0,0), 0)

let solve1 text = 
    let ins = text |> parse
    let (w1,_,_) = fst ins |> runWire
    let (w2,_,_) = snd ins |> runWire

    let c = collisions w1 w2
    let closest = c |> List.map fst |> List.minBy (manhattan (0,0))
    manhattan (0,0) closest

let solve2 text =
    let ins = text |> parse
    let (w1,_,_) = fst ins |> runWire
    let (w2,_,_) = snd ins |> runWire
    let firstC = collisions w1 w2 |> List.head
    let numSteps = firstC |> snd |> List.sum
    numSteps

let text = env |> env2f |> f2text
let res1 = text |> solve1
let res2 = text |> solve2

let finished = true