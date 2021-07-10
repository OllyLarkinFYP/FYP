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

    
    
    let allTests =
        testList "All Simulator Tests" [
            simulatorRuns
        ]
