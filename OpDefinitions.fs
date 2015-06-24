module OpDefinitions

    open System
    open System.Windows

    type MoveOperation = Point list
        
    type DrawOperation = 
        | LineOp of Point list
        | HorizontalLineOp of Double list
        | VerticalLineOp of Double list
        | CubicBezierCurveOp of (Point * Point * Point) list
        | QuadraticBezierCurveOp of (Point * Point) list
        | SmoothCubicBezierCurveOp of (Point * Point) list
        | SmoothQuadraticBezierCurveOp of (Point * Point) list
        | EllipseArcOp of (Size * Double * bool * bool * Point) list // size, rotationAngle, isLargeArc, sweepDirectionFlag, endPoint

    type Command = 
        | Move of MoveOperation 
        | Draw of DrawOperation 
        | Close
