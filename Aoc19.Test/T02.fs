module Aoc19.T02

open NUnit.Framework
open D02.Solution
open Aoc19.IntCode

[<SetUp>]
let Setup () =
    ()
    
[<TestCase("1,6,7,4,0,0,19,21", 40, TestName="TestStepAddition")>] // addition: 19+21=40
[<TestCase("2,6,7,4,0,0,19,21", 399, TestName="TestStepMultiplication")>] // multiplication 19*21=399
let TestStep (text:string) (result:int64) =
    let p0 = text |> parse
    let p1 = p0 |> step |> Option.get
    Assert.That(p1.codes[4], Is.EqualTo result )

[<Test>]
let TestMultiStep () =
    
    let confidentStep st = st |> step |> Option.get

    let text = "1,9,10,3,2,3,11,0,99,30,40,50"
    let p0 = text |> parse
    let p1 = p0 |> confidentStep
    let p2 = p1 |> confidentStep

    Assert.That(p1.codes[3], Is.EqualTo 70)
    Assert.That(p2.codes[0], Is.EqualTo 3500)
    
[<TestCase("1,0,0,0,99", "2,0,0,0,99")>]
[<TestCase("2,3,0,3,99", "2,3,0,6,99")>]
[<TestCase("2,4,4,5,99,0", "2,4,4,5,99,9801")>]
[<TestCase("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")>]
let TestRunUntilHalt (text0:string) (textN:string) =
    let actual = text0 |> parse |> runUntilHalt |> ics2codes
    let expected = parse textN
    Assert.That(actual, Is.EqualTo expected.codes)

    Assert.Pass()

[<Test>]
let TestParameterize () =
    let text = "0,0,0,0,0,0,0,0"
    let m = text |> parse

    let m2 = m |> parameterize 13 29

    Assert.That (m2.codes[1], Is.EqualTo 13)
    Assert.That (m2.codes[2], Is.EqualTo 29)

    Assert.Pass()