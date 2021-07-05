module Helper

open Expecto
open FParsec
open CommonTypes

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

let failTest = Expect.isFalse true

let parserSuccTests name (tests: (string * Parser<'Result,UserState> * string) list) =
    tests
    |> List.map (fun (n, p, inp) ->
        let result = runParserOnString p "" "" inp
        testCaseAsync n <| async 
            { match result with
              | Success _ -> Expect.isTrue true "Should not fail"
              | _ -> failTest (sprintf "The parser did not successfully parse: %A" result) })
    |> testList name

let specParserSuccTests name (parser: Parser<'Result,UserState>) (tests: (string * string) list) =
    tests
    |> List.map (fun (n, inp) ->
        let result = runParserOnString parser "" "" inp
        testCaseAsync n <| async 
            { match result with
              | Success _ -> Expect.isTrue true "Should not fail"
              | _ -> failTest (sprintf "The parser did not successfully parse: %A" result) })
    |> testList name

let specParserFailTests name (parser: Parser<'Result,UserState>) (tests: (string * string) list) =
    tests
    |> List.map (fun (n, inp) ->
        let result = runParserOnString parser "" "" inp
        testCaseAsync n <| async 
            { match result with
              | Failure _ -> Expect.isTrue true "Should not fail"
              | _ -> failTest (sprintf "The parser successfully parsed: %A" result) })
    |> testList name

let specParserMatchOutTests name (parser: Parser<'Result,UserState>) (tests: (string * string * 'Result) list) =
    tests
    |> List.map (fun (n, inp, exp) ->
        // let result = run parser inp
        let result = runParserOnString parser "" "" inp
        testCaseAsync n <| async 
            { match result with
              | Success (r, _, _) -> Expect.equal r exp n
              | _ -> failTest (sprintf "The parser did not successfully parse: %A" result) })
    |> testList name
