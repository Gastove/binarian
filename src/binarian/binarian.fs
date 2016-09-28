module Binarian

open System.Drawing

open Empattern

let binarateInt (i: int) =
    System.Convert.ToString(i, 2)

let binarateStr (s: string) =
    s
    |> Seq.map (int >> binarateInt)
    |> System.String.Concat
    |> Seq.map System.Char.GetNumericValue
    |> Seq.map int
    |> Seq.toList

let binarate strArray =
    strArray
    |> List.map binarateStr
    |> System.String.Concat


let binarateFile path rowLength : ColorCoding =
    System.IO.File.ReadAllLines(path)
    |> Array.toList
    |> List.collect (fun line -> binarateStr line)
    |> (fun ints ->  List.chunkBySize rowLength ints)

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
    let fontName = "Times New Roman"
    let fontSize = 12

    let colorSpec =
        {ColorCoding = binaryPoem;
         BackgroundColor = backgroundColor;
         OneColor = oneColor;
         ZeroColor = zeroColor;
         GridLineColor = lineColor}

    let spec =
        {ColorSpec = colorSpec;
         GridLineWidth = gridLineWidth;
         CellSize = cellSize;
         Border = border;
         FontName = fontName;
         FontSize = fontSize;
         FileName = imagePath}

    drawPattern spec

    0 // return an integer exit code
