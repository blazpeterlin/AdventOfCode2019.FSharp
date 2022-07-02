namespace Aoc19

open System.IO

type Env = T | P

module Operators =
    let (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
    let (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)
    let (-..) (x0,y0) (x1,y1) = (x0-x1,y0-y1)
    let (-...) (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)

module Input =
    let rec private skipLastEmpty (lst:string list) =
        match lst with
        | [] -> []
        | [""] -> []
        | [x] -> [x]
        | head :: tail -> 
            let skippedTail = skipLastEmpty(tail)
            match head, skippedTail with
            | "", [] -> []
            | _ -> head :: skippedTail

    let env2f env = env |> function | T -> "t.txt" | P -> "p.txt"
    let f2text fpath = fpath |> File.ReadAllText
    let f2lines fpath = fpath |> File.ReadAllLines |> List.ofSeq |> skipLastEmpty
    let text2tokens (splitCh:string) (text:string) = text.Split(splitCh.ToCharArray()) |> List.ofArray
    let text2lines (text:string) = text.Split("\r\n") |> List.ofArray |> skipLastEmpty
    let f2tokens splitCh fpath = fpath |> f2text |> text2tokens splitCh


module Common =
    let manhattan (x0,y0) (x1,y1) = abs(x1-x0)+abs(y1-y0)

    [<System.Diagnostics.DebuggerStepThrough>]
    let rec unfold f state = 
      seq {
        match f state with
        | Some x ->
          yield x
          yield! unfold f x
        | None -> ()
      }
    let seqPop (s:'T seq) : 'T =
        let enumerator = s.GetEnumerator()
        
        enumerator.MoveNext() |> ignore
        let head = enumerator.Current
        
        head
