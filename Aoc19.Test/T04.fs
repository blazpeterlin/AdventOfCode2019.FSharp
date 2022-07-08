module Aoc19.T04

open NUnit.Framework
open D04.Solution

[<SetUp>]
let Setup () =
    ()

[<TestCase(111111, true)>]
[<TestCase(223450, false)>]
[<TestCase(123789, false)>]
let TestCandidate (num:int) (result:bool) =
    Assert.That(isCandidate num, Is.EqualTo result)
    Assert.Pass()
    
[<TestCase(112233, true)>]
[<TestCase(123444, false)>]
[<TestCase(111122, true)>]
let TestCandidate2 (num:int) (result:bool) =
    Assert.That(isCandidate2 num, Is.EqualTo result)
    Assert.Pass()