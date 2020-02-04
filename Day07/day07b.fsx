#load "workingfunctionality.fsx"
open Workingfunctionality
#load "sampledata.fsx"
open Sampledata

let x = AmpChainFeedback example4Seq example4Program
let y = AmpChainFeedback example5Seq example5Program
let z = Permute [ 5 .. 9 ] |> List.collect (fun x -> (AmpChainFeedback x inputProgram).Remains)
let answer = List.head (List.rev (List.sort z))
