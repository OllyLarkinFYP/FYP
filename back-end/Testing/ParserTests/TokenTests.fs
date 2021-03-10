namespace ParserTests

open NUnit.Framework
open Parser
open AST
open Utils

[<TestFixture>]
type IdentifierTests () =
    
    [<Test>]
    member this.``Just lowecase letters should parse``() =
        "abc"
        |> parserTest Token.pIdentifier true

    [<Test>]
    member this.``Just uppercase letters should parse``() =
        "ABC"
        |> parserTest Token.pIdentifier true

    [<Test>]
    member this.``Uppercase and lowercase should parse``() =
        "abcDEF"
        |> parserTest Token.pIdentifier true

    [<Test>]
    member this.``Underscore at start should parse``() =
        "_abc"
        |> parserTest Token.pIdentifier true

    [<Test>]
    member this.``Underscore inside should parse``() =
        "a_bc"
        |> parserTest Token.pIdentifier true

    [<Test>]
    member this.``Dollar inside should parse``() =
        "a$bc"
        |> parserTest Token.pIdentifier true

    [<Test>]
    member this.``Number inside should parse``() =
        "a3bc"
        |> parserTest Token.pIdentifier true

    [<Test>]
    member this.``Dollar at start should not parse``() =
        "$abc"
        |> parserTest Token.pIdentifier false

    [<Test>]
    member this.``Number at start should not parse``() =
        "1abc"
        |> parserTest Token.pIdentifier false

    [<Test>]
    member this.``Collects correct contents``() =
        let iden = "abc"
        iden
        |> contentsTest Token.pIdentifier iden

    [<Test>]
    member this.``Collects correct contents with extra``() =
        let iden = "abc yo yo"
        iden
        |> contentsTest Token.pIdentifier "abc"



[<TestFixture>]
type CommentTests () =

    [<Test>]
    member this.``Single line comment should parse``() =
        "// this is a comment"
        |> parserTest Token.pComment true

    [<Test>]
    member this.``Multi line comment should parse``() =
        @"/* this is a 
        multi line comment test
        yo yo */"
        |> parserTest Token.pComment true

    [<Test>]
    member this.``Multi line comment without end should not parse``() =
        @"/* this is a 
        multi line comment test
        yo yo"
        |> parserTest Token.pComment false



[<TestFixture>]
type NumberTests () =

    [<Test>]
    member this.``Normal number should parse``() =
        "123"
        |> parserTest Token.pNumber true

    [<Test>]
    member this.``Normal number should return value``() =
        "123"
        |> contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Decimal base parses correctly``() =
        "'d123"
        |> contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Binary base parses correctly``() =
        "'b101"
        |> contentsTest Token.pNumber { Size = None; Value = 5u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Octal base parses correctly``() =
        "'o123"
        |> contentsTest Token.pNumber { Size = None; Value = 83u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Hex base lowercase number parses correctly``() =
        "'hab3"
        |> contentsTest Token.pNumber { Size = None; Value = 2739u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Hex base uppercase number parses correctly``() =
        "'hAB3"
        |> contentsTest Token.pNumber { Size = None; Value = 2739u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Decimal uppercase base parses correctly``() =
        "'D123"
        |> contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Binary uppercase base parses correctly``() =
        "'B101"
        |> contentsTest Token.pNumber { Size = None; Value = 5u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Octal uppercase base parses correctly``() =
        "'O123"
        |> contentsTest Token.pNumber { Size = None; Value = 83u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Hex uppercase base lowercase number parses correctly``() =
        "'Hab3"
        |> contentsTest Token.pNumber { Size = None; Value = 2739u; UnknownBits = []; Signed = false }

    [<Test>]
    member this.``Signed lowercase parses correctly``() =
        "'sd123"
        |> contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = true }

    [<Test>]
    member this.``Signed uppercase parses correctly``() =
        "'Sd123"
        |> contentsTest Token.pNumber { Size = None; Value = 123u; UnknownBits = []; Signed = true }

    [<Test>]
    member this.``Size parsed correctly``() =
        "20'sd123"
        |> contentsTest Token.pNumber { Size = Some 20u; Value = 123u; UnknownBits = []; Signed = true }
