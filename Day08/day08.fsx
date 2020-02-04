#load "sampledata.fsx"

open Sampledata

let imgSize = 25 * 6
let imgLayers = inputData.Length / imgSize

let layers =
    [ 0 .. imgLayers - 1 ] |> List.map (fun x -> inputData.[(x * imgSize)..((x * imgSize) + imgSize - 1)])

type layerInfo =
    { X : int
      Y : int
      Z : int }

let sortByX =
    layers
    |> List.map (fun x ->
        List.fold (fun acc y ->
            match y with
            | 0 -> { acc with X = acc.X + 1 }
            | 1 -> { acc with Y = acc.Y + 1 }
            | 2 -> { acc with Z = acc.Z + 1 })
            { X = 0
              Y = 0
              Z = 0 } (Array.toList x))
    |> List.sortBy (fun i -> i.X)

let answer1 = (List.head sortByX).Y * (List.head sortByX).Z

let rec get (metalist : int [] list) (lst : int) position =
    match metalist.[lst].[position] with
    | 0 -> 0
    | 1 -> 1
    | 2 -> get metalist (lst + 1) (position)

let answer2 = [ 0 .. imgSize - 1 ] |> List.map (fun x -> get layers 0 x )
