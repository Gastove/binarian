module Binarian


let binarateInt (i: int) =
    System.Convert.ToString(i, 2)


let binarateStr (s: string) =
    s
    |> Seq.map (int >> binarateInt)
    |> System.String.Concat


let binarate strArray =
    strArray
    |> Array.map binarateStr
    |> System.String.Concat


let binarateFile path =
    System.IO.File.ReadAllLines(path)


[<EntryPoint>]
let main argv =
    let p = @"/Users/gastove/Documents/bonne_journee.txt"
    let poem = binarateFile p
    let res = Seq.map binarateStr poem
    for line in res do
        printfn "%A" line
    0 // return an integer exit code
