namespace Tests

open System
open Expecto
open Helper
open Parser.ConstantExpression
open Parser.Expression
open AST
open CommonTypes

module private Helpers =
    type ce = ConstantExpressionT
    type cp = ConstantPrimaryT
    let constPrim = ce.Primary
    let constPrimNum = cp.Number >> constPrim

module ExpressionsTests =

    open Helpers

    let constExprTests = 
        testList "Constant Expression Tests" [
            specParserMatchOutTests "Primary Const Expr Tests" pConstantExpression [
                "Parses a number as expected",
                    "32",
                        constPrimNum (VNum 32)

                "Brackets",
                    "(32)",
                        constPrim (cp.Brackets (constPrimNum (VNum 32)))

                "Concatenation",
                    "{32, 31}",
                        constPrim
                            (cp.Concat [ constPrimNum (VNum 32); constPrimNum (VNum 31) ])
            
                "Nested concatenation",
                    "{32, {31, 30}}",
                        constPrim
                            (cp.Concat
                                [ constPrimNum (VNum 32)
                                  constPrim (cp.Concat [ constPrimNum (VNum 31); constPrimNum (VNum 30) ]) ])
            ]
        ]

    let exprTests =
        specParserMatchOutTests "Expression Tests" pExpression [

        ]

    let allTests =
        testList "All Expressions Parsing Tests" [ 
            constExprTests
            exprTests
        ]

