module PathParsing
    open FParsec
    open OpDefinitions
    open System
    open System.Windows

    let pathParser =
        let buildCommandParser theCommand theCommandUnionCase theUnionCaseParam = 
            theCommand |>> (List.map theUnionCaseParam >> theCommandUnionCase)
    
        let command charList commandParser = (choice <| List.map pchar charList) >>. commandParser
            
        let point : Parser<_, unit> = tuple2 (spaces >>. pfloat .>> spaces) (opt (pchar ',') >>. spaces >>. pfloat)
        let size = point
    
        let moveCommand =                       command ['M'; 'm'] (many1 (attempt point))
        let lineCommand =                       command ['L'; 'l'] (many1 (attempt point))
        let horizontalLineCommand =             command ['H'; 'h'] (many1 (attempt pfloat))
        let verticalLineCommand =               command ['V'; 'v'] (many1 (attempt pfloat))
        let cubicBezierCurveCommand =           command ['C'; 'c'] (many1 (attempt (tuple3 point point point)))
        let quadraticBezierCurveCommand =       command ['Q'; 'q'] (many1 (attempt (tuple2 point point)))
        let smoothCubicBezierCurveCommand =     command ['S'; 's'] (many1 (attempt (tuple2 point point)))
        let smoothQuadraticBezierCurveCommand = command ['T'; 't'] (many1 (attempt (tuple2 point point)))
        let ellipseArcCommand =                 command ['A'; 'a'] (many1 (attempt (tuple5 size (spaces >>. pfloat) (spaces >>. pint32) (spaces >>. pint32) point)))
        let closeCommand =                      command ['Z'; 'z'] (preturn [()])
    
        let createPoint x = new Point(fst x, snd x)
        let createSize x = new Size(fst x, snd x)
        let createPointTuple3 =  (fun (p1, p2, p3) -> createPoint p1, createPoint p2, createPoint p3)
        let createPointTuple2 =  (fun (p1, p2) -> createPoint p1, createPoint p2)
        let createEllipse = (fun (size, rotationAngle, isLargeArcFlag, sweepDirectionFlag, endPoint) -> createSize size, float rotationAngle, isLargeArcFlag = 1, sweepDirectionFlag = 1, createPoint endPoint)
    
        let move =                          buildCommandParser  moveCommand                       Move                                   createPoint
        let line =                          buildCommandParser  lineCommand                       (Draw << LineOp)                       createPoint
        let horizontalLine =                buildCommandParser  horizontalLineCommand             (Draw << HorizontalLineOp)             float
        let verticalLine =                  buildCommandParser  verticalLineCommand               (Draw << VerticalLineOp)               float
        let cubicBezierCurve =              buildCommandParser  cubicBezierCurveCommand           (Draw << CubicBezierCurveOp)           createPointTuple3
        let quadraticBezierCurve =          buildCommandParser  quadraticBezierCurveCommand       (Draw << QuadraticBezierCurveOp)       createPointTuple2
        let smoothCubicBezierCurve =        buildCommandParser  smoothCubicBezierCurveCommand     (Draw << SmoothCubicBezierCurveOp)     createPointTuple2
        let smoothQuadraticBezierCurve =    buildCommandParser  smoothQuadraticBezierCurveCommand (Draw << SmoothQuadraticBezierCurveOp) createPointTuple2
        let ellipseArc =                    buildCommandParser  ellipseArcCommand                 (Draw << EllipseArcOp)                 createEllipse    
        let close =                         buildCommandParser  closeCommand                      (fun l -> Close)                       id
    
        let path = choice [move
                           line
                           horizontalLine
                           verticalLine
                           cubicBezierCurve
                           quadraticBezierCurve
                           smoothCubicBezierCurve
                           smoothQuadraticBezierCurve
                           ellipseArc
                           close]
        many (spaces >>. path .>> spaces)
    
    let commandListToString commandList =
        let pointToString = fun (p : Point) -> sprintf "%f, %f" p.X p.Y
        let sizeToString = fun (p : Size) -> sprintf "%f, %f" p.Width p.Height
        let boolToString = fun b -> match b with | true -> sprintf "1" | false -> sprintf "0"
        
        let reduceWithSpace = List.reduce (fun x y -> x + " " + y)
        
        let pointListToString = List.map pointToString >> reduceWithSpace
        let floatListToString = List.map (sprintf "%f") >> reduceWithSpace
        
        let tuple3ListToString = List.map (fun (p1, p2, p3) -> pointToString p1 + " " + pointToString p2 + " " + pointToString p3) >> reduceWithSpace
        let tuple2ListToString = List.map (fun (p1, p2) -> pointToString p1 + " " + pointToString p2) >> reduceWithSpace
        
        let ellipseListToString = List.map (fun (size, rotationAngle, isLargeArcFlag, sweepDirectionFlag, endPoint) -> sizeToString size + " " + sprintf "%f" rotationAngle + " " + boolToString isLargeArcFlag + " " + boolToString sweepDirectionFlag + " " + pointToString endPoint) >> reduceWithSpace

        let getCommandString command = match command with
                                       | Move(pointList)                                     -> "M " + (pointList       |> pointListToString)
                                       | Draw(LineOp(pointList))                             -> "L " + (pointList       |> pointListToString)
                                       | Draw(HorizontalLineOp(yList))                       -> "H " + (yList           |> floatListToString)
                                       | Draw(VerticalLineOp(xList))                         -> "V " + (xList           |> floatListToString)
                                       | Draw(CubicBezierCurveOp(tuple3PointList))           -> "C " + (tuple3PointList |> tuple3ListToString)
                                       | Draw(QuadraticBezierCurveOp(tuple2PointList))       -> "Q " + (tuple2PointList |> tuple2ListToString)
                                       | Draw(SmoothCubicBezierCurveOp(tuple2PointList))     -> "S " + (tuple2PointList |> tuple2ListToString)
                                       | Draw(SmoothQuadraticBezierCurveOp(tuple2PointList)) -> "T " + (tuple2PointList |> tuple2ListToString)
                                       | Draw(EllipseArcOp(ellipseList))                     -> "A " + (ellipseList     |> ellipseListToString)
                                       | Close                                               -> "Z"
        commandList |> List.map getCommandString |> reduceWithSpace

    let getOpDefinitions pathStr =
        let result = run pathParser pathStr
        match result with
            | Success(result, _, _) -> Some(result)
            | Failure(errorMsg, _, _) -> None

        