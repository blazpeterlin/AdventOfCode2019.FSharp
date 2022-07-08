module Aoc19.T05

open NUnit.Framework
open D05.Solution
open Aoc19.IntCode

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestInputOutput () =

    let createIC _ = "3,0,4,0,99" |> parseIntCode

    for num in ([1..9] |> Seq.map int64) do
        //let q = System.Collections.Generic.Queue<int64>()
        let ic,q = initStreams (createIC()) (seq [ num ])
        ic |> runUntilHalt

        Assert.That(q.consumeElt(), Is.EqualTo(num))
    Assert.Pass()


[<Test>]
let TestArgModes () =
    
    let ic0 = "1002,4,3,4,33" |> parseIntCode
    let icf = ic0 |> runUntilHalt |> ics2codes

    Assert.That(icf[4], Is.EqualTo(99))

    Assert.Pass()

    
[<TestCase("3,9,8,9,10,9,4,9,99,-1,8", TestName="TestEqualTo_PositionMode")>]
[<TestCase("3,3,1108,-1,8,3,4,3,99", TestName="TestEqualTo_ImmediateMode")>]
let TestEqualTo (text:string) =
    let ic = text |> parseIntCode
    for num in 1..9 do
        let ic0,q = initStreams ic (seq [int64 num])
        let icf = ic0 |> runUntilHalt |> ics2codes
        Assert.That(q.consumeElt(), Is.EqualTo(if num=8 then 1 else 0))
    Assert.Pass()
    
[<TestCase("3,9,7,9,10,9,4,9,99,-1,8", TestName="TestLessThan_PositionMode")>]
[<TestCase("3,3,1107,-1,8,3,4,3,99", TestName="TestLessThan_ImmediateMode")>]
let TestLessThan (text:string) =
    let ic = text |> parseIntCode
    for num in 1..9 do
        let ic0,q = initStreams ic (seq [int64 num])
        let icf = ic0 |> runUntilHalt |> ics2codes
        Assert.That(q.consumeElt(), Is.EqualTo(if num<8 then 1 else 0))
    Assert.Pass()
    
    
[<TestCase("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", TestName="TestJump_PositionMode")>]
[<TestCase("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", TestName="TestJump_ImmediateMode")>]
let TestJump (text:string) =
    let ic = text |> parseIntCode
    for num in -5..5 do
        let ic0,q = initStreams ic (seq [int64 num])
        let icf = ic0 |> runUntilHalt |> ics2codes
        Assert.That(q.consumeElt(), Is.EqualTo(if num=0 then 0 else 1))
    Assert.Pass()
    
[<Test>]
let TestComplexScenario () =
    let text = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

    let ic = text |> parseIntCode
    for num in 0..10 do
        let ic0,q = initStreams ic (seq [int64 num])
        let icf = ic0 |> runUntilHalt |> ics2codes

        let expectedRes = 
            match num with
            | x when x <8 -> 999
            | 8 -> 1000
            | x when x >8 -> 1001

        Assert.That(q.consumeElt(), Is.EqualTo(expectedRes))

    Assert.Pass()