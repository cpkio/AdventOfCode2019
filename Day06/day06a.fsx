#load "workingfunctionality.fsx"

open Workingfunctionality

#load "sampledata.fsx"

open Sampledata

let m = input''.PathTo "YOU"
let n = input''.PathTo "SAN"

let answer1 = PathSum 0 (input''.DepthTree())
let o = m.InsertNode(n)
let q = o.Cut "YOU" "SAN"
let r = (q.PathTo "YOU").DepthTree()
let s = (q.PathTo "SAN").DepthTree()

// Не стал писать функцию показа максимальной глубины дерева,
// взял глубины до YOU & SAN вручную
