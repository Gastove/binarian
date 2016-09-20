module Empattern

open System.Drawing
open System.Drawing.Drawing2D

let newBitmap (h: int) (w: int) =
    let bmp = new Bitmap(w, h)
    bmp

let graphicFromBitmap bitmap =
    let gfx = Graphics.FromImage(bitmap)
    gfx.CompositingMode = CompositingMode.SourceCopy |> ignore
    gfx

let makeLatCoords yMin yMax step xMin xMax =
    let yVals = [yMin .. step .. yMax]
    let leftHandYs = List.map (fun y -> (xMin, y)) yVals
    let rightHandYs = List.map (fun y -> (xMax, y)) yVals

    List.zip leftHandYs rightHandYs

let makeLongCoords xMin xMax step yMin yMax =
    let xVals = [xMin .. step .. xMax]
    let topXs = List.map (fun x -> (x, yMin)) xVals
    let bottomXs = List.map (fun x -> (x, yMax)) xVals

    List.zip topXs bottomXs

let drawLine (gfx: Graphics) pen theOne theOther =
    let (startX: int, startY: int) = theOne
    let (endX: int, endY: int) = theOther
    gfx.DrawLine(pen, startX, startY, endX, endY) |> ignore

// Mostly for testing.
let makeRandomBinary (rand: System.Random) members =
    List.init members (fun _ -> rand.Next(2))

let makeBinaryToSquareColor zeroColor oneColor =
    fun binary ->
        if binary = 0 then zeroColor else oneColor

let calculateGrid colorCoding cellSize border gridLineWidth =
    let widthUnits = Seq.head colorCoding |> Seq.length
    let heightUnits = Seq.length colorCoding

    let widthPixels = (widthUnits * cellSize) + (widthUnits * gridLineWidth) + gridLineWidth
    let heightPixels = (heightUnits * cellSize + (heightUnits * gridLineWidth)) + gridLineWidth

    (heightPixels, widthPixels)

let drawPattern fileName colorCoding border cellSize gridLineWidth
    (backgroundColor: Color) (gridLineColor: Color) (zeroColor: Color)
    (oneColor: Color) =

    let binaryToSquareColor = makeBinaryToSquareColor zeroColor oneColor

    let (gridHeight, gridWidth) = calculateGrid colorCoding cellSize border gridLineWidth
    printfn "Grid height: %A, grid width: %A" gridHeight gridWidth

    let imgHeight = gridHeight + (2 * border)
    let imgWidth = gridWidth + (2 * border)

    let pen = new Pen(gridLineColor)
    pen.Width <- float32 gridLineWidth

    let bmp = newBitmap imgHeight imgWidth
    let gfx = graphicFromBitmap bmp

    gfx.Clear(backgroundColor)

    let mutable x = border + gridLineWidth
    let mutable y = border + gridLineWidth
    let brush = new SolidBrush(zeroColor)

    // TODO refactor into own function
    for cellRow in colorCoding do
        for cellColor in cellRow do
            brush.Color <- binaryToSquareColor cellColor
            gfx.FillRectangle(brush, x, y, cellSize, cellSize)
            x <- x + cellSize + gridLineWidth

        y <- y + cellSize + gridLineWidth
        x <- border + gridLineWidth

    let latCoords = makeLatCoords border (gridHeight + border) (cellSize +  gridLineWidth) border (gridWidth + border - gridLineWidth)
    let longCoords = makeLongCoords border (gridWidth + border) (cellSize + gridLineWidth) border (gridHeight + border - gridLineWidth)

    List.map (fun (a, b) -> drawLine gfx pen a b) latCoords |> ignore
    List.map (fun (a, b) -> drawLine gfx pen a b) longCoords |> ignore

    bmp.Save(fileName)

// When this gets evaluated, it gets run. Which surprises me, but here we are. So.
// Only use for testing.
// let parameterizeAndDraw =
//     let rand = System.Random()
//     let p = "demo_grid.png"
//     let colorCoding = List.init 30 (fun _ -> makeRandomBinary rand 20)
//     let border = 10
//     let cellSize = 25
//     let gridLineWidth = 1
//     let backgroundColor = Color.BurlyWood
//     let lineColor = Color.BlanchedAlmond
//     let zeroColor = Color.Black
//     let oneColor = Color.White
//     drawPattern p colorCoding border cellSize gridLineWidth backgroundColor lineColor zeroColor oneColor
