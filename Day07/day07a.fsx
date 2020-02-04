#load "workingfunctionality.fsx"
open Workingfunctionality
#load "sampledata.fsx"
open Sampledata

let u = AmpChain example1Seq example1Program // should be 43210
let v = AmpChain example2Seq example2Program // should be 54321
let w = AmpChain example3Seq example3Program // should be 65210

let z = Permute [ 0 .. 4 ] |> List.collect (fun x -> (AmpChain x inputProgram).Remains())
let answer = List.head (List.rev (List.sort z))
