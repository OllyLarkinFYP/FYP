namespace ParserTests

open NUnit.Framework
open Parser
open AST
open Utils

[<TestFixture>]
type ConstantExpressionTests () =

    [<Test>]
    member this.``Constant number term parses correctly``() =
        let input = "123"
        let expected = 
            ConstantExpressionT.Primary (
                ConstantPrimaryT.Number { 
                    Size = None
                    Value = 123UL
                    UnknownBits = []
                    Signed = false })
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant bracket term parses correctly``() =
        let input = "(123)"
        let expected = 
            ConstantExpressionT.Primary (
                ConstantPrimaryT.Brackets (
                    ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 123UL
                            UnknownBits = []
                            Signed = false })))
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant concat expression term parses correctly``() =
        let input = "{1,2}"
        let expected = 
            ConstantExpressionT.Primary (
                ConstantPrimaryT.Concat [
                    ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false }); 
                    ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 2UL
                            UnknownBits = []
                            Signed = false })])
        contentsTest ConstantExpression.pConstantExpression expected input
        
    [<Test>]
    member this.``Constant concat parses correctly``() =
        let input = "{1,2}"
        let expected = [
            ConstantExpressionT.Primary (
                ConstantPrimaryT.Number { 
                    Size = None
                    Value = 1UL
                    UnknownBits = []
                    Signed = false }); 
            ConstantExpressionT.Primary (
                ConstantPrimaryT.Number { 
                    Size = None
                    Value = 2UL
                    UnknownBits = []
                    Signed = false })]
        contentsTest ConstantExpression.pConstantConcatenation expected input
        
    [<Test>]
    member this.``Constant range expression parses correctly``() =
        let input = "1:2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantRangeExpressionT.Range {| LHS = LHS; RHS = RHS |}
        contentsTest ConstantExpression.pConstantRangeExpression expected input

    [<Test>]
    member this.``Constant ternary expression parses correctly``() =
        let input = "1 ? 2 : 3"
        let condition = ConstantExpressionT.Primary (
                            ConstantPrimaryT.Number { 
                                Size = None
                                Value = 1UL
                                UnknownBits = []
                                Signed = false })
        let trueVal = ConstantExpressionT.Primary (
                            ConstantPrimaryT.Number { 
                                Size = None
                                Value = 2UL
                                UnknownBits = []
                                Signed = false })
        let falseVal = ConstantExpressionT.Primary (
                            ConstantPrimaryT.Number { 
                                Size = None
                                Value = 3UL
                                UnknownBits = []
                                Signed = false })
        let expected = 
            ConstantExpressionT.CondExpression {| Condition = condition; TrueVal = trueVal; FalseVal = falseVal |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant logical or expression parses correctly``() =
        let input = "1 || 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalOr |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant logical and expression parses correctly``() =
        let input = "1 && 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalAnd |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant bitwise or expression parses correctly``() =
        let input = "1 | 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseOr |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant bitwise xor expression parses correctly``() =
        let input = "1 ^ 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseXor |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant bitwise xnor expression parses correctly``() =
        let input = "1 ~^ 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseXnor |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant bitwise xnor 2 expression parses correctly``() =
        let input = "1 ^~ 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseXnor |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant bitwise and expression parses correctly``() =
        let input = "1 & 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseAnd |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant logical equality expression parses correctly``() =
        let input = "1 == 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalEquality |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant logical inequality expression parses correctly``() =
        let input = "1 != 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalInequality |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant case equality expression parses correctly``() =
        let input = "1 === 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = CaseEquality |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant case inequality expression parses correctly``() =
        let input = "1 !== 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = CaseInequality |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant less than expression parses correctly``() =
        let input = "1 < 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LessThan |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant greater than expression parses correctly``() =
        let input = "1 > 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = GreaterThan |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant less than or equal expression parses correctly``() =
        let input = "1 <= 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LessThanOrEqual |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant greater than or equal expression parses correctly``() =
        let input = "1 >= 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = GreaterThanOrEqual |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant logical left shift expression parses correctly``() =
        let input = "1 << 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalLeftShift |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant logical right shift expression parses correctly``() =
        let input = "1 >> 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalRightShift |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant arithmatic left shift expression parses correctly``() =
        let input = "1 <<< 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = ArithmaticLeftShift |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant arithmatic right shift expression parses correctly``() =
        let input = "1 >>> 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = ArithmaticRightShift |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant plus expression parses correctly``() =
        let input = "1 + 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Plus |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant minus expression parses correctly``() =
        let input = "1 - 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Minus |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant multiply expression parses correctly``() =
        let input = "1 * 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Multiply |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant divide expression parses correctly``() =
        let input = "1 / 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Divide |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant modulus expression parses correctly``() =
        let input = "1 % 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Modulus |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant power expression parses correctly``() =
        let input = "1 ** 2"
        let LHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = ConstantExpressionT.Primary (
                    ConstantPrimaryT.Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            ConstantExpressionT.BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Power |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary plus expression parses correctly``() =
        let input = "+1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = UnaryOperatorT.Plus |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary minus expression parses correctly``() =
        let input = "-1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = UnaryOperatorT.Minus |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary logical negation expression parses correctly``() =
        let input = "!1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = LogicalNegation |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary bitwise negation expression parses correctly``() =
        let input = "~1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = BitwiseNegation |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary reduction and expression parses correctly``() =
        let input = "&1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = ReductionAnd |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary reduction nand expression parses correctly``() =
        let input = "~&1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = ReductionNand |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary reduction or expression parses correctly``() =
        let input = "|1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = ReductionOr |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary reduction nor expression parses correctly``() =
        let input = "~|1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = ReductionNor |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary reduction xor expression parses correctly``() =
        let input = "^1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = ReductionXor |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary reduction xnor expression parses correctly``() =
        let input = "~^1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = ReductionXnor |}
        contentsTest ConstantExpression.pConstantExpression expected input

    [<Test>]
    member this.``Constant unary reduction xnor 2 expression parses correctly``() =
        let input = "^~1"
        let value = ConstantExpressionT.Primary (
                        ConstantPrimaryT.Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            ConstantExpressionT.UniExpression {| Expression = value; Operator = ReductionXnor |}
        contentsTest ConstantExpression.pConstantExpression expected input



[<TestFixture>]
type ExpressionTests () =

    [<Test>]
    member this.``number term parses correctly``() =
        let input = "123"
        let expected = 
            Primary (
                Number { 
                    Size = None
                    Value = 123UL
                    UnknownBits = []
                    Signed = false })
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``bracket term parses correctly``() =
        let input = "(123)"
        let expected = 
            Primary (
                Brackets (
                    Primary (
                        Number { 
                            Size = None
                            Value = 123UL
                            UnknownBits = []
                            Signed = false })))
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``concat expression term parses correctly``() =
        let input = "{1,2}"
        let expected = 
            Primary (
                PrimaryT.Concat [
                    Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false }); 
                    Primary (
                        Number { 
                            Size = None
                            Value = 2UL
                            UnknownBits = []
                            Signed = false })])
        contentsTest Expression.pExpression expected input
        
    [<Test>]
    member this.``concat parses correctly``() =
        let input = "{1,2}"
        let expected = [
            Primary (
                Number { 
                    Size = None
                    Value = 1UL
                    UnknownBits = []
                    Signed = false }); 
            Primary (
                Number { 
                    Size = None
                    Value = 2UL
                    UnknownBits = []
                    Signed = false })]
        contentsTest Expression.pConcatenation expected input
        
    [<Test>]
    member this.``range expression parses correctly``() =
        let input = "1:2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            Range {| LHS = LHS; RHS = RHS |}
        contentsTest Expression.pRangeExpression expected input

    [<Test>]
    member this.``ternary expression parses correctly``() =
        let input = "1 ? 2 : 3"
        let condition = Primary (
                            Number { 
                                Size = None
                                Value = 1UL
                                UnknownBits = []
                                Signed = false })
        let trueVal = Primary (
                            Number { 
                                Size = None
                                Value = 2UL
                                UnknownBits = []
                                Signed = false })
        let falseVal = Primary (
                            Number { 
                                Size = None
                                Value = 3UL
                                UnknownBits = []
                                Signed = false })
        let expected = 
            CondExpression {| Condition = condition; TrueVal = trueVal; FalseVal = falseVal |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``logical or expression parses correctly``() =
        let input = "1 || 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalOr |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``logical and expression parses correctly``() =
        let input = "1 && 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalAnd |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``bitwise or expression parses correctly``() =
        let input = "1 | 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseOr |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``bitwise xor expression parses correctly``() =
        let input = "1 ^ 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseXor |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``bitwise xnor expression parses correctly``() =
        let input = "1 ~^ 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseXnor |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``bitwise xnor 2 expression parses correctly``() =
        let input = "1 ^~ 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseXnor |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``bitwise and expression parses correctly``() =
        let input = "1 & 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = BitwiseAnd |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``logical equality expression parses correctly``() =
        let input = "1 == 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalEquality |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``logical inequality expression parses correctly``() =
        let input = "1 != 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalInequality |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``case equality expression parses correctly``() =
        let input = "1 === 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = CaseEquality |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``case inequality expression parses correctly``() =
        let input = "1 !== 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = CaseInequality |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``less than expression parses correctly``() =
        let input = "1 < 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LessThan |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``greater than expression parses correctly``() =
        let input = "1 > 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = GreaterThan |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``less than or equal expression parses correctly``() =
        let input = "1 <= 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LessThanOrEqual |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``greater than or equal expression parses correctly``() =
        let input = "1 >= 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = GreaterThanOrEqual |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``logical left shift expression parses correctly``() =
        let input = "1 << 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalLeftShift |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``logical right shift expression parses correctly``() =
        let input = "1 >> 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = LogicalRightShift |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``arithmatic left shift expression parses correctly``() =
        let input = "1 <<< 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = ArithmaticLeftShift |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``arithmatic right shift expression parses correctly``() =
        let input = "1 >>> 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = ArithmaticRightShift |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``plus expression parses correctly``() =
        let input = "1 + 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Plus |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``minus expression parses correctly``() =
        let input = "1 - 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Minus |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``multiply expression parses correctly``() =
        let input = "1 * 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Multiply |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``divide expression parses correctly``() =
        let input = "1 / 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Divide |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``modulus expression parses correctly``() =
        let input = "1 % 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Modulus |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``power expression parses correctly``() =
        let input = "1 ** 2"
        let LHS = Primary (
                    Number { 
                        Size = None
                        Value = 1UL
                        UnknownBits = []
                        Signed = false })
        let RHS = Primary (
                    Number { 
                        Size = None
                        Value = 2UL
                        UnknownBits = []
                        Signed = false })
        let expected = 
            BinaryExpression {| LHS = LHS; RHS = RHS; BinOperator = Power |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary plus expression parses correctly``() =
        let input = "+1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = UnaryOperatorT.Plus |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary minus expression parses correctly``() =
        let input = "-1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = UnaryOperatorT.Minus |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary logical negation expression parses correctly``() =
        let input = "!1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = LogicalNegation |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary bitwise negation expression parses correctly``() =
        let input = "~1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = BitwiseNegation |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary reduction and expression parses correctly``() =
        let input = "&1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = ReductionAnd |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary reduction nand expression parses correctly``() =
        let input = "~&1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = ReductionNand |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary reduction or expression parses correctly``() =
        let input = "|1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = ReductionOr |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary reduction nor expression parses correctly``() =
        let input = "~|1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = ReductionNor |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary reduction xor expression parses correctly``() =
        let input = "^1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = ReductionXor |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary reduction xnor expression parses correctly``() =
        let input = "~^1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = ReductionXnor |}
        contentsTest Expression.pExpression expected input

    [<Test>]
    member this.``unary reduction xnor 2 expression parses correctly``() =
        let input = "^~1"
        let value = Primary (
                        Number { 
                            Size = None
                            Value = 1UL
                            UnknownBits = []
                            Signed = false })
        let expected = 
            UniExpression {| Expression = value; Operator = ReductionXnor |}
        contentsTest Expression.pExpression expected input
