namespace ParserTests

open NUnit.Framework
open Parser
open FParsec

[<TestFixture>]
type IdentifierTests () =

    let idenTest shouldParse iden =
        if shouldParse
        then
            run Token.pIdentifier iden
            |> function
            | Success _ -> ()
            | Failure (a, _, _) -> failwithf "Expected parse to be successful. Result: \n%s" a
        else
            run Token.pIdentifier iden
            |> function
            | Success (a, _, _) -> failwithf "Expected parse to be unsuccessful. Result: %s" a
            | Failure _ -> ()

    [<Test>]
    member this.``Just lowecase letters should parse``() =
        "abc"
        |> idenTest true

    [<Test>]
    member this.``Just uppercase letters should parse``() =
        "ABC"
        |> idenTest true

    [<Test>]
    member this.``Uppercase and lowercase should parse``() =
        "abcDEF"
        |> idenTest true

    [<Test>]
    member this.``Underscore at start should parse``() =
        "_abc"
        |> idenTest true

    [<Test>]
    member this.``Underscore inside should parse``() =
        "a_bc"
        |> idenTest true

    [<Test>]
    member this.``Dollar inside should parse``() =
        "a$bc"
        |> idenTest true

    [<Test>]
    member this.``Number inside should parse``() =
        "a3bc"
        |> idenTest true

    [<Test>]
    member this.``Dollar at start should not parse``() =
        "$abc"
        |> idenTest false

    [<Test>]
    member this.``Number at start should not parse``() =
        "1abc"
        |> idenTest false
