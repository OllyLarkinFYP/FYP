namespace ParserTests

open NUnit.Framework
open Parser
open FParsec

module Util =
    let parserTest parser shouldParse iden =
        if shouldParse
        then
            run parser iden
            |> function
            | Success _ -> ()
            | Failure (a, _, _) -> failwithf "Expected parse to be successful. Result: \n%s" a
        else
            run Token.pIdentifier iden
            |> function
            | Success (a, _, _) -> failwithf "Expected parse to be unsuccessful. Result: %s" a
            | Failure _ -> ()


[<TestFixture>]
type IdentifierTests () =
    
    [<Test>]
    member this.``Just lowecase letters should parse``() =
        "abc"
        |> Util.parserTest Token.pIdentifier true

    [<Test>]
    member this.``Just uppercase letters should parse``() =
        "ABC"
        |> Util.parserTest Token.pIdentifier true

    [<Test>]
    member this.``Uppercase and lowercase should parse``() =
        "abcDEF"
        |> Util.parserTest Token.pIdentifier true

    [<Test>]
    member this.``Underscore at start should parse``() =
        "_abc"
        |> Util.parserTest Token.pIdentifier true

    [<Test>]
    member this.``Underscore inside should parse``() =
        "a_bc"
        |> Util.parserTest Token.pIdentifier true

    [<Test>]
    member this.``Dollar inside should parse``() =
        "a$bc"
        |> Util.parserTest Token.pIdentifier true

    [<Test>]
    member this.``Number inside should parse``() =
        "a3bc"
        |> Util.parserTest Token.pIdentifier true

    [<Test>]
    member this.``Dollar at start should not parse``() =
        "$abc"
        |> Util.parserTest Token.pIdentifier false

    [<Test>]
    member this.``Number at start should not parse``() =
        "1abc"
        |> Util.parserTest Token.pIdentifier false


[<TestFixture>]
type CommentTests () =

    [<Test>]
    member this.``Single line comment should parse``() =
        "// this is a comment"
        |> Util.parserTest Token.pComment true

    [<Test>]
    member this.``Multi line comment should parse``() =
        @"/* this is a 
        multi line comment test
        yo yo */"
        |> Util.parserTest Token.pComment true

    [<Test>]
    member this.``Multi line comment without end should not parse``() =
        @"/* this is a 
        multi line comment test
        yo yo"
        |> Util.parserTest Token.pComment false
