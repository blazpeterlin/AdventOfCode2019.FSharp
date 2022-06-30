module Aoc19.T01

open NUnit.Framework
open D01.Solution

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.AreEqual(2, reqFuel 12)
    Assert.AreEqual(2, reqFuel 14)
    Assert.AreEqual(654, reqFuel 1969)
    Assert.AreEqual(33583, reqFuel 100756)
    Assert.Pass()

[<Test>]
let Test2 () =
    Assert.AreEqual(2, reqTotalFuel 14)
    Assert.AreEqual(966, reqTotalFuel 1969)
    Assert.AreEqual(50346, reqTotalFuel 100756)
    Assert.Pass()