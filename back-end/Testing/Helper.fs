module Helper

open Expecto

let equalityTests name (tests: (string * 'a * 'a) list) =
    tests
    |> List.map (fun (n, e, a) -> 
        testCaseAsync n <| async { Expect.equal a e n })
    |> testList name