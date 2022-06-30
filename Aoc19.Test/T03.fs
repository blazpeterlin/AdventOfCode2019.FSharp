module Aoc19.T03

open NUnit.Framework
open D03
open Aoc19.Common

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestManhattan () =
    
    let p1 = (1,2)
    let p2 = (4,7)

    let m = manhattan p1 p2

    Assert.AreEqual(4-1+7-2, m)
    
    Assert.Pass()

[<Test>]
let TestMove () =
    let (m,(x,y),_) = moveN (Map.empty,(0,0),0) ((1,0),8)

    Assert.AreEqual (8, m.Count)
    Assert.AreEqual ((8,0),(x,y))

    Assert.Pass()
    
[<Test>]
let TestCollision () =
    let (m1,_,_) = "R8,U5,L5,D3" |> parseLine |> runWire
    let (m2,_,_) = "U7,R6,D4,L4" |> parseLine |> runWire

    let c = collisions m1 m2 |> List.map fst

    Assert.AreEqual(2, c.Length)

    Assert.Pass()

[<Test>]
let testCountSteps () =
    let (m,_,_) = "R8,U5,L5,D3" |> parseLine |> runWire

    let vals = m |> Map.values |> List.ofSeq |> List.sort

    Assert.That(vals, Is.EquivalentTo (Seq.init vals.Length (fun x -> x+1)))
    
    Assert.Pass()
    
[<Test>]
let TestParse () =
    let text = "R8,U5,L5,D3
U7,R6,D4,L4"
    let (m1, m2) = text |> parse

    Assert.AreEqual(4, m1.Length)
    Assert.AreEqual(4, m2.Length)

    Assert.Pass()

    
[<Test>]
let TestSolve1 () =
    let text = "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"

    let s = solve1 text

    Assert.AreEqual(159, s)

    Assert.Pass()


[<Test>]
let TestSolve2A () =
    let text = "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"

    let s2 = solve2 text

    Assert.That(610, Is.EqualTo s2)

    Assert.Pass()



[<Test>]
let TestSolve2B () =
    let text = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

    let s2 = solve2 text

    Assert.That(410, Is.EqualTo s2)

    Assert.Pass()