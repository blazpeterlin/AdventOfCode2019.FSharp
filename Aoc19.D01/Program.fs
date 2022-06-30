module D01.Solution

open Aoc19
open Aoc19.Input

let env = P
let raw = env |> env2f
let inp = raw |> f2lines |> List.map int

let reqFuel mass =
    mass/3-2

let res1 = inp |> List.map reqFuel |> List.sum

let reqTotalFuel mass = 
    mass
    |> List.unfold (reqFuel >> function | rf when rf <=0 -> None | rf -> Some (rf, rf))
    |> List.sum

let res2 = inp |> List.map reqTotalFuel |> List.sum

let finished = true