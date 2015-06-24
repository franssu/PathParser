#r "bin/debug/FParsecCS.dll"
#r "bin/debug/FParsec.dll"
#r "System.Windows"
#r "WindowsBase"

#load "OpDefinitions.fs"
#load "PathParsing.fs"

open FParsec
open PathParsing

// this script 
// - parses a sample path
// - converts the result to a string
// - re-parse this string
// - checks if the result of those two parses are equal

let result =
    let pathStr = "L 77.753189,16.28700 M 77.753189,16.287001 z M20.665127,0 C23.633877,0.37537572 25.653692,2.1997195 26.462396,4.9325521 L30.555752,4.9221137 31.337002,1.0783545 83.180748,0.92210361 79.712028,16.265965 82.045349,16.349297 C84.482117,16.463988 85.586914,17.697364 85.399673,20.015904 85.733009,20.099236 77.962143,52.328488 77.962143,52.328488 76.736732,55.164494 74.647529,56.581639 71.852966,56.856469 L15.430747,56.859998 C13.299918,56.709771 11.691784,55.56107 11.930749,53.047471 L19.649498,20.672246 C21.224096,17.489463 23.922853,15.843336 28.180744,16.328465 L29.212377,12.349271 24.79574,12.349271 C20.074341,12.716091 17.12414,15.148763 15.711998,19.375361 L8.5557497,50.172448 0.10320466,4.8283842 C-0.3922621,1.8987286 0.921893,0.44453725 3.4938062,0.015850891 z A 1.1,2.2 3.3 1 1 4.4,5.5"
    run pathParser pathStr

let printResult = 
    function
    | Success(commandList, _, _) -> printf "%s\n" (commandListToString commandList)
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

printResult result;

match result with
    | Success(commandList, _, _)   -> 
        let secondResult = run pathParser (commandListToString commandList)
        printResult secondResult
        match secondResult with
            | Success(secondCommandList, _, _) -> printf "%A\n" (secondCommandList = commandList)
            | _ -> printf "second failed.."
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg