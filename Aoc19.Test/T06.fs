module Aoc19.T06

open NUnit.Framework
open D06.Solution

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestSolve1 () =

    let text = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"

    let res = text |> solve1

    Assert.That(res, Is.EqualTo(42))
    
    Assert.Pass()


[<Test>]
let TestSolve2 () =

    let text = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"

    let res = text |> solve2

    Assert.That(res, Is.EqualTo(4))
    
    Assert.Pass()
