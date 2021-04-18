module Helper

open Expecto

let equalityTests name (tests: (string * 'a * 'a) list) =
    tests
    |> List.map (fun (n, e, a) -> 
        testCaseAsync n <| async { Expect.equal a e n })
    |> testList name

let trueTests name (tests: (string * bool) list) =
    tests
    |> List.map (fun (n, a) -> 
        testCaseAsync n <| async { Expect.equal a true n })
    |> testList name

let equalityListTests name (tests: (string * 'a list * 'a list) list) =
    tests
    |> List.map (fun (n, e, a) -> 
        testCaseAsync n <| async { Expect.equal (List.sort a) (List.sort e) n })
    |> testList name
