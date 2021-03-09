namespace ParserTests

open NUnit.Framework
open Parser
open AST
open FParsec

module Util =
    let parserTest parser shouldParse input =
        if shouldParse
        then
            run parser input
            |> function
            | Success _ -> ()
            | Failure (a, _, _) -> failwithf "Expected parse to be successful. Result: \n%s" a
        else
            run Token.pIdentifier input
            |> function
            | Success (a, _, _) -> failwithf "Expected parse to be unsuccessful. Result: %s" a
            | Failure _ -> ()

    let contentsTest (parser: Parser<'a,unit>) (expected: 'a) input =
        run parser input
        |> function
        | Success (res, _, _) -> Assert.That(res, Is.EqualTo(expected))
        | Failure (err, _, _) -> failwithf "Failed to parse. Result: \n%s" err



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

    [<Test>]
    member this.``Collects correct contents``() =
        let iden = "abc"
        iden
        |> Util.contentsTest Token.pIdentifier iden

    [<Test>]
    member this.``Collects correct contents with extra``() =
        let iden = "abc yo yo"
        iden
        |> Util.contentsTest Token.pIdentifier "abc"



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



[<TestFixture>]
type NumberTests () =

    [<Test>]
    member this.``Normal number should parse``() =
        "123"
        |> Util.parserTest Token.pNumber true

    [<Test>]
    member this.``Normal number should return value``() =
        "123"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Decimal base parses correctly``() =
        "'d123"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Binary base parses correctly``() =
        "'b101"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 5u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Octal base parses correctly``() =
        "'o123"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 83u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Hex base lowercase number parses correctly``() =
        "'hab3"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 2739u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Hex base uppercase number parses correctly``() =
        "'hAB3"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 2739u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Decimal uppercase base parses correctly``() =
        "'D123"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Binary uppercase base parses correctly``() =
        "'B101"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 5u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Octal uppercase base parses correctly``() =
        "'O123"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 83u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Hex uppercase base lowercase number parses correctly``() =
        "'Hab3"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 2739u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Signed lowercase parses correctly``() =
        "'sd123"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = true }

    [<Test>]
    member this.``Signed uppercase parses correctly``() =
        "'Sd123"
        |> Util.contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = true }

    [<Test>]
    member this.``Size parsed correctly``() =
        "20'sd123"
        |> Util.contentsTest Token.pNumber { Size = Some 20u; Value = 123u; UnknownBits = []; Signed = true }
