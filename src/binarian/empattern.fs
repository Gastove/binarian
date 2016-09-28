module Empattern

open System.Drawing
open System.Drawing.Drawing2D

type ColorCoding = list<list<int>>

type ColorSpec =
    {ColorCoding: ColorCoding;
     BackgroundColor: Color;
     OneColor: Color;
     ZeroColor: Color;
     GridLineColor: Color}

type PatternSpec =
    {ColorSpec: ColorSpec;
     GridLineWidth: int;
     CellSize: int;
     Border: int;
     FontName: string;
     FontSize: int;
     FileName: string}

type Cell = int * int

type GridSpec =
    {GridHeight: int;
     GridWidth: int;
     Cell: Cell;
     Step: int;
     ColorLookup: int -> Color}

type LabelSpec =
    {Height: int;
     Width: int
     Pen: Pen;
     Brush: Brush;
     Size: Size;
     Step: int;
     Font: Font;
     Points: list<Point>}

let loadFont (fontName: string) fontSize =
    new Font(fontName, float32 fontSize)

let labelDefaults =
    {Height = 0;
     Width = 0;
     Step = 0;
     Pen = Pens.Black;
     Brush = Brushes.Black;
     Size = Size(10, 10);
     Font = loadFont "Times New Roman" 12;
     Points = [Point(0, 0)]}

type ImageSpec =
    {ImageHeight: int;
     ImageWidth: int;}

type Layout =
    {GridOrigin: Point;
     YLeftOrigin: Point;
     YRightOrigin: Point;
     XTopOrigin: Point;
     XBottomOrigin: Point}

let newBitmap (height : int) (width : int) =
    new Bitmap(width, height)

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

// Mostly for testing.
let makeRandomBinary (rand: System.Random) members =
    List.init members (fun _ -> rand.Next(2))

let makeBinaryToSquareColor zeroColor oneColor =
    fun binary ->
        if binary = 0 then zeroColor else oneColor

let calculateGridSpec patternSpec =
    let {ColorSpec=colorSpec; GridLineWidth=gridLineWidth; CellSize=cellSize} = patternSpec
    let {ZeroColor=zeroColor; OneColor=oneColor} = colorSpec
    let lookup = makeBinaryToSquareColor zeroColor oneColor
    let widthUnits = Seq.head colorSpec.ColorCoding |> Seq.length
    let heightUnits = Seq.length colorSpec.ColorCoding

    let widthPixels = (widthUnits * cellSize) + (widthUnits * gridLineWidth) + gridLineWidth
    let heightPixels = (heightUnits * cellSize + (heightUnits * gridLineWidth)) + gridLineWidth

    let step = cellSize + gridLineWidth

    {GridSpec.GridHeight = heightPixels;
     GridSpec.GridWidth = widthPixels;
     GridSpec.Cell = (cellSize, cellSize);
     GridSpec.Step = step;
     GridSpec.ColorLookup = lookup}

let calculateGridDimensions patternSpec =
    let {ColorSpec=colorSpec; CellSize=cellSize; GridLineWidth=gridLineWidth} = patternSpec
    let widthUnits = Seq.head colorSpec.ColorCoding |> Seq.length
    let heightUnits = Seq.length colorSpec.ColorCoding

    let widthPixels = (widthUnits * cellSize) + (widthUnits * gridLineWidth) + gridLineWidth
    let heightPixels = (heightUnits * cellSize + (heightUnits * gridLineWidth)) + gridLineWidth

    (heightPixels, widthPixels)

let rectToRectF (rect: Rectangle) =
    let p = rect.Location
    let xF = float32 p.X
    let yF = float32 p.Y
    let sideF = float32 rect.Width

    RectangleF(xF, yF, sideF, sideF)

let drawLine (gfx: Graphics) pen theOne theOther =
    let (startX: int, startY: int) = theOne
    let (endX: int, endY: int) = theOther
    gfx.DrawLine(pen, startX, startY, endX, endY) |> ignore

let drawLines gfx coords pen =
    List.map (fun (a, b) -> drawLine gfx pen a b) coords |> ignore
    ()


let calculateLabelSize (points : list<Point>) step =
    let widthUnits =
        points
        |> List.map (fun p -> p.X)
        |> List.max

    let heightUnits =
        points
        |> List.map (fun p -> p.Y)
        |> List.max

    widthUnits + step, heightUnits + step

let createLabel labelSpec =
    let {Points=locations; Font=font; Pen=pen;
         Brush=brush; Size=size; Step=step} = labelSpec
    let labelWidth, labelHeight = calculateLabelSize locations step

    let bmp = newBitmap labelHeight labelWidth
    let gfx = Graphics.FromImage(bmp)

    let sizeF = SizeF(float32 size.Height, float32 size.Width)

    let stringFormat = new StringFormat()
    stringFormat.Alignment <- StringAlignment.Center
    stringFormat.LineAlignment <- StringAlignment.Center

    let mutable rect = new Rectangle(Point(0, 0), size)
    let mutable rectF = new RectangleF(PointF(0.0f, 0.0f), sizeF)

    let mutable labelCounter = 1
    let mutable label = ""

    for loc in locations do
        rect.Location <- loc
        rectF <- rectToRectF rect
        label <- string labelCounter
        gfx.DrawRectangle(pen, rect)
        gfx.DrawString(label, font, brush, rectF, stringFormat)

        labelCounter <- labelCounter + 1

    bmp

let drawLabel (gfx: Graphics) (corner: Point) locations pen brush (size: Size)
    (font: Font) =

    let sizeF = SizeF(float32 size.Height, float32 size.Width)

    let mutable rect = new Rectangle(Point(0, 0), size)
    let mutable rectF = new RectangleF(PointF(0.0f, 0.0f), sizeF)

    let mutable labelCounter = 1
    let mutable label = ""

    for loc in locations do
        rect.Location <- loc
        rectF <- rectToRectF rect
        label <- string labelCounter
        gfx.DrawRectangle(pen, rect)
        gfx.DrawString(label, font, brush, rectF)

        labelCounter <- labelCounter + 1

    ()

let generateLabelPoints init step stop (pointBuilder: int -> Point) =
    [init .. step .. stop]
    |> List.map pointBuilder


let createCheckerboard (gridSpec: GridSpec) (patternSpec: PatternSpec) =
    let {GridHeight=gridHeight; GridWidth=gridWidth} = gridSpec

    let bmp = newBitmap gridHeight gridWidth
    let gfx = Graphics.FromImage(bmp)

    let binaryToSquareColor = gridSpec.ColorLookup

    let gridLineWidth = patternSpec.GridLineWidth

    let mutable x = gridLineWidth
    let mutable y = gridLineWidth

    let step = gridSpec.Step
    let cellSize = gridSpec.Cell |> fst
    let brush = new SolidBrush(patternSpec.ColorSpec.ZeroColor)

    for cellRow in patternSpec.ColorSpec.ColorCoding do
        for cellColor in cellRow do
            brush.Color <- binaryToSquareColor cellColor
            gfx.FillRectangle(brush, x, y, cellSize, cellSize)
            x <- x + step

        y <- y + step
        x <- gridLineWidth

    let latCoords = makeLatCoords 0 gridHeight step 0 gridWidth
    let longCoords = makeLongCoords 0 gridWidth step 0 gridHeight

    let linePen = new Pen(patternSpec.ColorSpec.GridLineColor)

    drawLines gfx latCoords linePen
    drawLines gfx longCoords linePen

    bmp

let drawCheckerboard (gfx: Graphics) (gridSpec: GridSpec) (layout: Layout) (patternSpec: PatternSpec) =

    let binaryToSquareColor = gridSpec.ColorLookup

    let  xOrigin = layout.GridOrigin.X
    let  yOrigin = layout.GridOrigin.Y

    let mutable x = xOrigin
    let mutable y = yOrigin

    let step = gridSpec.Step
    let cellSize = gridSpec.Cell |> fst
    let brush = new SolidBrush(patternSpec.ColorSpec.ZeroColor)

    for cellRow in patternSpec.ColorSpec.ColorCoding do
        for cellColor in cellRow do
            brush.Color <- binaryToSquareColor cellColor
            gfx.FillRectangle(brush, x, y, cellSize, cellSize)
            x <- x + step

        y <- y + step
        x <- xOrigin
    ()

let generateLayoutFromSpecs patternSpec gridSpec =
    let {Border=border; CellSize=cellSize; GridLineWidth=gridLineWidth} = patternSpec
    let {GridHeight=gridHeight; GridWidth=gridWidth} = gridSpec

    let gridEdging = cellSize + (2 * border) + (2 * gridLineWidth)
    let gridOrigin = Point(gridEdging, gridEdging)

    let leftYLabelOrigin = Point(border, gridOrigin.Y)
    let rightYLabelOrigin = Point(gridOrigin.X + gridWidth + border, gridOrigin.Y)

    let topXLabelOrigin = Point(gridOrigin.X, border)
    let bottomXLabelOrigin = Point(gridOrigin.X, gridOrigin.Y + gridHeight + border)

    {GridOrigin=gridOrigin;
     YLeftOrigin=leftYLabelOrigin;
     YRightOrigin=rightYLabelOrigin;
     XTopOrigin=topXLabelOrigin;
     XBottomOrigin=bottomXLabelOrigin}

let generateImageSpec patternSpec gridSpec =
    let {Border=border; CellSize=cellSize; GridLineWidth=gridLineWidth} = patternSpec
    let {GridHeight=gridHeight; GridWidth=gridWidth} = gridSpec

    let imgHeight = gridHeight + (4 * border) + (2 * cellSize)
    let imgWidth = gridWidth + (4 * border) + (2 * cellSize)
    {ImageHeight=imgHeight; ImageWidth=imgWidth}

let generateGridSpec patternSpec  =

    let {ZeroColor=zeroColor; OneColor=oneColor} = patternSpec.ColorSpec

    let (gridHeight, gridWidth) = calculateGridDimensions patternSpec
    let cellSize = patternSpec.CellSize
    let cell = (cellSize, cellSize)
    let step = cellSize + patternSpec.GridLineWidth
    let binaryToSquareColor = makeBinaryToSquareColor zeroColor oneColor

    {GridHeight = gridHeight;
     GridWidth = gridWidth;
     Cell = cell;
     ColorLookup = binaryToSquareColor;
     Step = step}

let generatePoints init step stop (pointBuilder: int -> Point) =
    [init .. step .. stop]
    |> List.map pointBuilder

let generateLabelSpec init step stop cellSize (pointBuilder: int -> Point) =

    let points = generatePoints init step stop pointBuilder
    let size = Size(cellSize, cellSize)

    {labelDefaults with Points = points;
                        Step = step;
                        Size = size}

let generateLabelSpecs (gridSpec: GridSpec) (patternSpec: PatternSpec)  =
    let {CellSize=cellSize; GridLineWidth=gridLineWidth} = patternSpec
    let step = cellSize + gridLineWidth

    let init = gridLineWidth
    let stopHeight = gridSpec.GridHeight - gridLineWidth
    let stopWidth = gridSpec.GridWidth - gridLineWidth

    let xTopSpec = generateLabelSpec init step stopWidth cellSize (fun x -> Point(x, 0))
    let xBottomSpec = generateLabelSpec init step stopWidth cellSize (fun x -> Point(x, 0))
    let yLeftSpec = generateLabelSpec init step stopHeight cellSize (fun y -> Point(0, y))
    let yRightSpec = generateLabelSpec init step stopHeight cellSize (fun y -> Point(0, y))

    [xTopSpec; xBottomSpec; yLeftSpec; yRightSpec]

let drawPattern (patternSpec: PatternSpec) =
    let gridSpec = generateGridSpec patternSpec
    let imageSpec = generateImageSpec patternSpec gridSpec
    let layout = generateLayoutFromSpecs patternSpec gridSpec

    let labelSpecs = generateLabelSpecs gridSpec patternSpec

    printfn "Drawing an image %i high by %i wide" imageSpec.ImageHeight imageSpec.ImageWidth
    let bmp = newBitmap imageSpec.ImageHeight imageSpec.ImageWidth
    let gfx = graphicFromBitmap bmp

    gfx.Clear(patternSpec.ColorSpec.BackgroundColor)

    let checkerboard = createCheckerboard gridSpec patternSpec
    let lables = List.map createLabel labelSpecs

    gfx.DrawImage(checkerboard, layout.GridOrigin)

    gfx.DrawImage(lables.[0], layout.XTopOrigin)
    gfx.DrawImage(lables.[1], layout.XBottomOrigin)
    gfx.DrawImage(lables.[2], layout.YLeftOrigin)
    gfx.DrawImage(lables.[3], layout.YRightOrigin)

    bmp.Save(patternSpec.FileName)

// let rand = System.Random()

// let defaultColorSpec =
//     {ColorCoding = List.init 5 (fun _ -> makeRandomBinary rand 5);
//      BackgroundColor = Color.BurlyWood;
//      GridLineColor = Color.AliceBlue;
//      ZeroColor = Color.Black;
//      OneColor = Color.AntiqueWhite}

// let testPatternSpec =
//     {ColorSpec = defaultColorSpec;
//      GridLineWidth = 1;
//      CellSize = 25;
//      Border = 10;
//      FontName = "Times New Roman";
//      FontSize = 12;
//      FileName = "test_pattern.png"}

// drawPattern testPatternSpec
