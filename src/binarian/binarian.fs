module Binarian

open System.Drawing

let binarateInt (i: int) =
    System.Convert.ToString(i, 2)

let binarateStr (s: string) rowLength =
    s
    |> Seq.map (int >> binarateInt)
    |> System.String.Concat
    |> Seq.map System.Char.GetNumericValue
    |> Seq.map int

let binarate strArray =
    strArray
    |> Array.map binarateStr
    |> System.String.Concat


let binarateFile path rowLength =
    System.IO.File.ReadAllLines(path)
    |> Seq.collect (fun line -> binarateStr line rowLength)
    |> (fun ints -> Seq.chunkBySize rowLength ints)

[<EntryPoint>]
let main argv =
    let rowLength = 110
    let poemPath = @"/Users/gastove/Documents/bonne_journee.txt"
    let imagePath = @"/Users/gastove/Pictures/bonne_journee.png"
    let binaryPoem = binarateFile poemPath rowLength

    let border = 10
    let cellSize = 25
    let gridLineWidth = 1
    let backgroundColor = Color.BurlyWood
    let lineColor = Color.BlanchedAlmond
    let zeroColor = Color.Black
    let oneColor = Color.White
    Empattern.drawPattern imagePath binaryPoem border cellSize gridLineWidth backgroundColor lineColor zeroColor oneColor

    0 // return an integer exit code
