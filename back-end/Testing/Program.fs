module Program 

open Expecto
open Tests

let tests = 
    testList "All Tests" [ 
        ConstExprTests.allTests
        VNumTests.allTests
        TokenTests.allTests
        ExpressionsTests.allTests
        SimulatorTests.allTests
    ] 

[<EntryPoint>] 
let main _ = 
    runTestsWithCLIArgs [] [||] tests
