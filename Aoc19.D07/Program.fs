module D07.Solution

open Aoc19
open Aoc19.Common
open Aoc19.Input
open Aoc19.Operators
open Aoc19.IntCode
open System

let env = P

let generateAllInputs allowedNums =
    let appendRnd lst = 
        allowedNums |> List.map (fun num -> num::lst)

    [1..5] 
    |> List.fold (fun numsList _ -> numsList |> List.map appendRnd |> List.concat) [[]] 
    |> List.filter (fun nums -> nums |> Seq.distinct |> Seq.length |> (=)5)
    
let setupAmpsPart1 (baseICS: IntCodeState) (phaseSetting: int64 list) =
    let phasedICS = 
        phaseSetting 
        |> List.map (fun phase -> { baseICS with input=QueuedEventOutput [phase]; output=Event<int64>()|>Some })

    phasedICS
    |> List.pairwise 
    |> List.iter (fun (icsFrom,icsTo) -> icsFrom.output.Value.Publish |> icsTo.input.addEvent)

    phasedICS[0].input.addBeforeRun 0
    phasedICS

let setupAmpsPart2 (baseICS: IntCodeState) (phaseSetting: int64 list) =
    let amps1 = setupAmpsPart1 baseICS phaseSetting
    amps1[0].input.addEvent (amps1|>List.last|>fun amp -> amp.output.Value.Publish)
    amps1

let runAmpsAndGetResult (amps : IntCodeState list) =
    let lastOutput = amps |> List.last |> ics2output
    //let ampsFinished = amps |> List.map runUntilHalt
    let ampsFinished = amps |> List.map (fun amp -> async { return runUntilHalt amp }) |> Async.Parallel |> Async.RunSynchronously
    lastOutput.consumeAllElts()
    
let phase2thrusters setupAmps amp phase = setupAmps amp phase |> runAmpsAndGetResult

let getMaxPhase allowedPhases setupAmps inputAmp = 
    let maxPhase = 
        generateAllInputs allowedPhases
        |> List.maxBy (phase2thrusters setupAmps inputAmp >> Seq.last)

    maxPhase

let solve1 text =
    let inputAmp = parseIntCode text
    let maxPhase = getMaxPhase [0L..4L] setupAmpsPart1 inputAmp
    maxPhase |> phase2thrusters setupAmpsPart1 inputAmp |> Seq.last

let solve2 text =
    let inputAmp = parseIntCode text
    let maxPhase = getMaxPhase  [5L..9L] setupAmpsPart2 inputAmp
    let res = maxPhase |> phase2thrusters setupAmpsPart2 inputAmp |> Seq.last
    res

let text = env |> env2f |> f2text
let res1 = text |> solve1
let res2 = text |> solve2

let finished = true