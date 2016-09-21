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

let loadFont =
    let font = new Font("Times New Roman", 12.0f)
    font

let rectToRectF (rect: Rectangle) =
    let p = rect.Location
    let xF = float32 p.X
    let yF = float32 p.Y
    let sideF = float32 rect.Width

    RectangleF(xF, yF, sideF, sideF)

let drawYLabels (gfx: Graphics) xPos yOrigin yMax step cellSize gridLineWidth =
    let pen = Pens.Black
    let brush = Brushes.Black
    let font = loadFont
    let points =
        [(yOrigin + gridLineWidth) .. step .. yMax]
        |> List.map (fun y -> Point(xPos, y))

    let size = Size(cellSize, cellSize)
    let sizeF = SizeF(float32 cellSize, float32 cellSize)

    let mutable rect = new Rectangle(Point(0, 0), size)
    let mutable rectF = new RectangleF(PointF(0.0f, 0.0f), sizeF)

    let mutable labelCounter = 1
    let mutable label = ""

    for point in points do
        rect.Location <- point
        rectF <- rectToRectF rect
        label <- string labelCounter
        gfx.DrawRectangle(pen, rect)
        gfx.DrawString(label, font, brush, rectF)

        labelCounter <- labelCounter + 1

let drawXLabels (gfx: Graphics) yPos xOrigin xMax step cellSize gridLineWidth =
    let pen = Pens.Black
    let brush = Brushes.Black
    let font = loadFont
    let points =
        [(xOrigin + gridLineWidth) .. step .. xMax]
        |> List.map (fun x -> Point(x, yPos))

    let size = Size(cellSize, cellSize)
    let sizeF = SizeF(float32 cellSize, float32 cellSize)

    let mutable rect = new Rectangle(Point(0, 0), size)
    let mutable rectF = new RectangleF(PointF(0.0f, 0.0f), sizeF)

    let mutable labelCounter = 1
    let mutable label = ""

    for point in points do
        rect.Location <- point
        rectF <- rectToRectF rect
        label <- string labelCounter
        gfx.DrawRectangle(pen, rect)
        gfx.DrawString(label, font, brush, rectF)

        labelCounter <- labelCounter + 1

let drawCheckerboard (gfx: Graphics) colorCoding xOffset yOffset (cellSize: int)
    gridLineWidth zeroColor oneColor =

    let binaryToSquareColor = makeBinaryToSquareColor zeroColor oneColor

    let xOrigin = xOffset + gridLineWidth
    let yOrigin = yOffset + gridLineWidth

    let mutable x = xOrigin
    let mutable y = yOrigin

    let step = cellSize + gridLineWidth

    let brush = new SolidBrush(zeroColor)

    for cellRow in colorCoding do
        for cellColor in cellRow do
            brush.Color <- binaryToSquareColor cellColor
            gfx.FillRectangle(brush, x, y, cellSize, cellSize)
            x <- x + step

        y <- y + step
        x <- xOrigin
    ()


let drawPattern fileName colorCoding border cellSize gridLineWidth
    (backgroundColor: Color) (gridLineColor: Color) (zeroColor: Color)
    (oneColor: Color) =

    let (gridHeight, gridWidth) = calculateGrid colorCoding cellSize border gridLineWidth
    printfn "Grid height: %A, grid width: %A" gridHeight gridWidth

    let imgHeight = gridHeight + (3 * border) + cellSize
    let imgWidth = gridWidth + (3 * border) + cellSize

    // Used a lot of places
    let step = cellSize + gridLineWidth
    let xOffset = ((2 * border) + cellSize)
    let yOffset = border

    let pen = new Pen(gridLineColor)
    pen.Width <- float32 gridLineWidth

    let bmp = newBitmap imgHeight imgWidth
    let gfx = graphicFromBitmap bmp

    gfx.Clear(backgroundColor)

    drawYLabels gfx border border gridHeight step cellSize gridLineWidth
    drawXLabels gfx (gridHeight + (2 * border)) xOffset (gridWidth + step) step cellSize gridLineWidth

    drawCheckerboard gfx colorCoding xOffset yOffset cellSize gridLineWidth zeroColor oneColor

    let latCoords = makeLatCoords yOffset (gridHeight + border + yOffset) step xOffset (xOffset + gridWidth - gridLineWidth)
    let longCoords = makeLongCoords xOffset (gridWidth + xOffset) step border (gridHeight + border - gridLineWidth)

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
