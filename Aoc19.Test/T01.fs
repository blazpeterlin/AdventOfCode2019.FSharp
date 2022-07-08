module Aoc19.T01

open NUnit.Framework
open D01.Solution

[<SetUp>]
let Setup () =
    ()
    
[<TestCase(12, ExpectedResult=2)>]
[<TestCase(14, ExpectedResult=2)>]
[<TestCase(1969, ExpectedResult=654)>]
[<TestCase(100756, ExpectedResult=33583)>]
let TestReqFuel input = reqFuel input

[<TestCase(14, ExpectedResult=2)>]
[<TestCase(1969, ExpectedResult=966)>]
[<TestCase(100756, ExpectedResult=50346)>]
let TestReqTotalFuel input = reqTotalFuel input