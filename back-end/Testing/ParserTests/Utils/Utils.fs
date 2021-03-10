namespace ParserTests

open NUnit.Framework
open FParsec

module Utils =
    let parserTest (parser: Parser<'a,unit>) shouldParse input =
        if shouldParse
        then
            run parser input
            |> function
            | Success _ -> ()
            | Failure (a, _, _) -> failwithf "Expected parse to be successful. Result: \n%s" a
        else
            run parser input
            |> function
            | Success (a, _, _) -> failwithf "Expected parse to be unsuccessful. Result: %A" a
            | Failure _ -> ()

    let contentsTest (parser: Parser<'a,unit>) (expected: 'a) input =
        run parser input
        |> function
        | Success (res, _, _) -> Assert.That(res, Is.EqualTo(expected))
        | Failure (err, _, _) -> failwithf "Failed to parse. Result: \n%s" err