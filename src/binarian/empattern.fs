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

let makeRandomBinary (rand: System.Random) members =
    // Make a test color grid with:
    // let colorCoding = List.init 50 (fun _ -> makeRandomBinary rand 25)
    List.init members (fun _ -> rand.Next(2))

let makeBinaryToSquareColor zeroColor oneColor =
    fun binary ->
        if binary = 0 then zeroColor else oneColor

let calculateGrid colorCoding cellSize =
    let widthUnits = Seq.head colorCoding |> Seq.length
    let heightUnits = Seq.length colorCoding
    let widthPixels = widthUnits * cellSize
    let heightPixels = heightUnits * cellSize

    (heightPixels, widthPixels)

let drawPattern fileName colorCoding border cellSize (backgroundColor: Color)
    (gridLineColor: Color) (zeroColor: Color) (oneColor: Color) =

    let binaryToSquareColor = makeBinaryToSquareColor zeroColor oneColor

    let (gridHeight, gridWidth) = calculateGrid colorCoding cellSize
    let imgHeight = gridHeight + (2 * border)
    let imgWidth = gridWidth + (2 * border)

    let pen = new Pen(gridLineColor)

    let bmp = newBitmap imgHeight imgWidth
    let gfx = graphicFromBitmap bmp

    gfx.Clear(backgroundColor)

    let mutable x = border
    let mutable y = border
    let brush = new SolidBrush(zeroColor)

    for cellRow in colorCoding do
        for cellColor in cellRow do
            brush.Color <- binaryToSquareColor cellColor
            gfx.FillRectangle(brush, x, y, cellSize, cellSize)
            x <- x + cellSize

        y <- y + cellSize
        x <- border

    // Parameterize this
    let latCoords = makeLatCoords border gridHeight cellSize border gridWidth
    let longCoords = makeLongCoords border gridWidth cellSize border gridHeight

    List.map (fun (a, b) -> drawLine gfx pen a b) latCoords |> ignore
    List.map (fun (a, b) -> drawLine gfx pen a b) longCoords |> ignore

    bmp.Save(fileName)

let parameterizeAndDraw =
    let rand = System.Random()
    let p = "demo_grid.png"
    let colorCoding = List.init 50 (fun _ -> makeRandomBinary rand 25)
    let border = 10
    let cellSize = 25
    let backgroundColor = Color.BurlyWood
    let lineColor = Color.BlanchedAlmond
    let zeroColor = Color.Black
    let oneColor = Color.White
    drawPattern p colorCoding border cellSize backgroundColor lineColor zeroColor oneColor
