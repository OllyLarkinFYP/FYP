module Program 

open Expecto
open Tests

let tests = 
    testList "All Tests" [ 
        constExprEvalPrimaryTests
        constExprEvalUniTests 
        constExprEvalBinaryTests
        constExprEvalTernaryTests
    ] 

[<EntryPoint>] 
let main _ = 
    runTestsWithCLIArgs [] [||] tests
