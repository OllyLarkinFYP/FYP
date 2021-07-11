namespace Tests

open System
open Expecto
open API

module SimHelpers =
    let testSim (tname: string) (files: string array) (inputs: APISimInp array) (expectedOutputs: SimulatorOutput array) =
        let cycles =
            (0, expectedOutputs)
            ||> Array.fold (fun acc output ->
                if output.values.Length > acc
                then output.values.Length
                else acc)
            |> uint
        let reqVars =
            expectedOutputs
            |> Array.map (fun output -> output.name)
        let vFiles =
            files
            |> Array.map (fun file ->
                { name = ""
                  contents = file })
        let simRet = 
            simulate vFiles "" inputs reqVars cycles
            |> fun ret -> ret.output
            |> Array.sortBy (fun o -> o.name)
        let expected =
            expectedOutputs
            |> Array.sortBy (fun o -> o.name)
        testCaseAsync tname <| async { Expect.equal simRet expected tname }

module SimulatorTests =

    open SimHelpers

    let simulatorRuns =
        let testProg = """
            module test(input a, b, output c);
                assign c = a + b;
            endmodule
            """
        let inputs =
            [|{ name = "a"
                repeating = false
                values = [| "0"; "0"; "1"; "1" |] }
             
              { name = "b"
                repeating = true
                values = [| "0"; "1" |] }|]
        let expOut =
            [|{ name = "c"
                values = [| "0"; "1"; "1"; "0" |] }|]
        testSim "simulator runs" [|testProg|] inputs expOut

    let posedgeTriggersCorrectly =
        let testProg = """
            module test(input a, output reg b);
                always @(posedge a) b = 1;
            endmodule
            """
        let inputs =
            [|{ name = "a"
                repeating = false
                values = [| "0"; "1" |] }|]
        let expOut =
            [|{ name = "b"
                values = [| "x"; "1" |] }|]
        testSim "posedge triggers" [|testProg|] inputs expOut

    let negedgeTriggersCorrectly =
        let testProg = """
            module test(input a, output reg b);
                always @(negedge a) b = 1;
            endmodule
            """
        let inputs =
            [|{ name = "a"
                repeating = false
                values = [| "1"; "0" |] }|]
        let expOut =
            [|{ name = "b"
                values = [| "x"; "1" |] }|]
        testSim "negedge triggers" [|testProg|] inputs expOut

    let initialRunsAtBeginning =
        let testProg = """
            module test(input a, output reg [1:0] b);
                reg [1:0] c;
                initial begin 
                    b = 0;
                    c = 3;
                end
                always @(posedge a) b = c;
            endmodule
            """
        let inputs =
            [|{ name = "a" 
                repeating = false 
                values = [| "0"; "1" |] }|]
        let expOut =
            [|{ name = "b"
                values = [| "00"; "11" |] }|]
        testSim "initial runs at beginning" [|testProg|] inputs expOut

    let alwaysBlockNoSelfTrigger =
        let testProg = """
            module test(input a, output reg b);
                always @(posedge a, b) b = 1;
            endmodule
            """
        let inputs =
            [|{ name = "a" 
                repeating = false 
                values = [| "0"; "1" |] }|]
        let expOut =
            [|{ name = "b"
                values = [| "x"; "1" |] }|]
        testSim "always block cannot trigger itself" [|testProg|] inputs expOut

    let nonblockingAfterBlocking =
        let testProg = """
            module test(input a, output reg b);
                always @(posedge a) begin
                    b <= 1;
                    b = 0;
                end
            endmodule
            """
        let inputs =
            [|{ name = "a" 
                repeating = false 
                values = [| "1" |] }|]
        let expOut =
            [|{ name = "b"
                values = [| "1" |] }|]
        testSim "non-blocking assignment occurs after always block is run" [|testProg|] inputs expOut
    
    let allTests =
        testList "All Simulator Tests" [
            simulatorRuns
            posedgeTriggersCorrectly
            negedgeTriggersCorrectly
            initialRunsAtBeginning
            alwaysBlockNoSelfTrigger
            nonblockingAfterBlocking
        ]
