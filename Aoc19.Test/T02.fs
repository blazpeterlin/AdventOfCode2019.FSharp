module Aoc19.T02

open NUnit.Framework
open D02

[<SetUp>]
let Setup () =
    ()
    
[<Test>]
let TestStepAdd () =
    let p0 = "1,6,7,4,0,0,19,21" |> parse |> map2state
    let p1 = p0 |> step |> Option.get
    Assert.AreEqual(19+21, p1.codes[4])

[<Test>]
let TestStepMul () =
    let p0 = "2,6,7,4,0,0,19,21" |> parse |> map2state
    let p1 = p0 |> step |> Option.get
    Assert.AreEqual(19*21, p1.codes[4])

[<Test>]
let TestMultiStep () =
    
    let confidentStep st = st |> step |> Option.get

    let text = "1,9,10,3,2,3,11,0,99,30,40,50"
    let p0 = text |> parse |> map2state
    let p1 = p0 |> confidentStep
    let p2 = p1 |> confidentStep

    Assert.AreEqual(70, p1.codes[3])
    Assert.AreEqual(3500, p2.codes[0])

[<Test>]
let TestRunUntilHalt () =

    let assert0toN str0 strN =
        let expected = parse strN
        let actual = str0 |> parse |> map2state |> runUntilHalt
        Assert.AreEqual(expected, actual)

    assert0toN "1,0,0,0,99" "2,0,0,0,99"
    assert0toN "2,3,0,3,99" "2,3,0,6,99"
    assert0toN "2,4,4,5,99,0" "2,4,4,5,99,9801"
    assert0toN "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"

    Assert.Pass()

[<Test>]
let TestParameterize () =
    let text = "0,0,0,0,0,0,0,0"
    let m = text |> parse

    let m2 = m |> parameterize 13 29

    Assert.AreEqual (13, m2[1])
    Assert.AreEqual (29, m2[2])

    Assert.Pass()