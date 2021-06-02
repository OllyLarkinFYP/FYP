namespace Tests

open System
open Expecto
open Helper
open Parser.Token
open Parser.Utils
open CommonTypes

module TokenTests = 

    let kewordTests =
        parserSuccTests "Parsing Keywords Tests" [
            "Always",
                Keyword.pAlways,
                    "always"

            "Assign",
                Keyword.pAssign,
                    "assign"

            "Begin",
                Keyword.pBegin,
                    "begin"

            "Case",
                Keyword.pCase,
                    "case"

            "Default",
                Keyword.pDefault,
                    "default"

            "Else",
                Keyword.pElse,
                    "else"

            "End",
                Keyword.pEnd,
                    "end"

            "Endcase",
                Keyword.pEndCase,
                    "endcase"

            "Endmodule",
                Keyword.pEndModule,
                    "endmodule"

            "If",
                Keyword.pIf,
                    "if"

            "Initial",
                Keyword.pInitial,
                    "initial"

            "Input",
                Keyword.pInput,
                    "input"

            "Module",
                Keyword.pModule,
                    "module"

            "Negedge",
                Keyword.pNegedge,
                    "negedge"

            "Or",
                Keyword.pOr,
                    "or"

            "Output",
                Keyword.pOutput,
                    "output"

            "Posedge",
                Keyword.pPosedge,
                    "posedge"

            "Reg",
                Keyword.pReg,
                    "reg"

            "Wire",
                Keyword.pWire,
                    "wire"
        ]

    let symbolTests =
        parserSuccTests "Symbol Tests" [
            "Semi-colon",
                Symbol.pSemiColon,
                    ";"
            
            "Colon",
                Symbol.pColon,
                    ":"

            "Comma",
                Symbol.pComma,
                    ","

            "Period",
                Symbol.pPeriod,
                    "."

            "Assign",
                Symbol.pAssign,
                    "="
            
            "Non-block assign",
                Symbol.pNonBlockAssign,
                    "<="

            "At",
                Symbol.pAt,
                    "@"

            "Star",
                Symbol.pStar,
                    "*"

            "Question mark",
                Symbol.pQMark,
                    "?"

            "Open round bracket",
                Symbol.pOpenRBrac,
                    "("

            "Close round bracket",
                Symbol.pCloseRBrac,
                    ")"

            "Open square bracket",
                Symbol.pOpenSBrac,
                    "["

            "Close square bracket",
                Symbol.pCloseSBrac,
                    "]"

            "Open curly bracket",
                Symbol.pOpenCBrac,
                    "{"

            "Close curly bracket",
                Symbol.pCloseCBrac,
                    "}"
        ]

    let identifierTests =
        testList "Identifier Tests" [
            specParserSuccTests "Identifier Success Tests" pIdentifier [
                "Alphabetic characters only",
                    "abcdefghijklmnopqrstuvwxyz"

                "Underscore included",
                    "_test__name_"

                "Digits included",
                    "test123"

                "Dollar included",
                    "te$t"
            ]

            specParserFailTests "Identifier Fail Tests" pIdentifier [
                "No starting with a digit",
                    "1test"
            ]
        ]

    let numberTests =
        specParserMatchOutTests "Number Tests" pNumber [
            "No base number",
                "12",
                    VNum 12

            "Decimal base",
                "'d12",
                    VNum 12

            "Binary base",
                "'b1100",
                    VNum 12

            "Octal base",
                "'o14",
                    VNum 12

            "Hexadecimal base lower case",
                "'hf6",
                    VNum 246

            "Hexadecimal base upper case",
                "'hF6",
                    VNum 246

            "Truncates when size is too small",
                "2'd256",
                    VNum 0

            "Unknown numbers in binary",
                "4'b1x11",
                    VNum.bin "1111"

            "Unknown numbers in binary 2",
                "4'b1x11",
                    VNum.bin "1011"
        ]

    let whiteSpaceTests =
        parserSuccTests "Testing ignoring of white space" [
            "skipStrWs removes spaces after token",
                skipStrWs "test",
                    "test    "

            "skipCharWs removes spaces after token",
                skipCharWs 'a',
                    "a   "
        ]

    let allTests =
        testList "All Token Parsing Tests" [ 
            kewordTests
            symbolTests
            identifierTests
            numberTests
            whiteSpaceTests
        ]
