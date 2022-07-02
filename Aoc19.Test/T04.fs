module Aoc19.T04

open NUnit.Framework
open D04.Solution

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestCandidate () =

    Assert.That(isCandidate 111111, Is.True)
    Assert.That(isCandidate 223450, Is.False)
    Assert.That(isCandidate 123789, Is.False)

    Assert.Pass()

[<Test>]
let TestCandidate2 () =

    Assert.That(isCandidate2 112233, Is.True)
    Assert.That(isCandidate2 123444, Is.False)
    Assert.That(isCandidate2 111122, Is.True)

    Assert.Pass()