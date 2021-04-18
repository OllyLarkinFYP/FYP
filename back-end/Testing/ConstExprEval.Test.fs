namespace Tests

open Expecto
open Helper
open CommonHelpers.ConstExprEval
open AST
open CommonTypes
open System

module ConstExprTests = 

    type ce = ConstantExpressionT
    type cp = ConstantPrimaryT

    let private ceEvalList name (tests: (string * VNum * ConstantExpressionT) list) =
        tests
        |> List.map (fun (n, e, a) -> n, e, evalConstExpr a)
        |> equalityTests name

    let private num value size = 
        ce.Primary 
            (cp.Number 
                { Size = Some (uint size); Value = uint64 value; UnknownBits = List.empty; Signed = false })

    let private bin (str: string) =
        let rec getNum (chars: char list) =
            match chars with
            | [] -> 0
            | hd::tl -> 
                if (hd = '1' || hd = '0')
                then (int hd - int '0') + ((getNum tl) <<< 1)
                else raise <| ArgumentException()
        let charLst =
            str
            |> Seq.toList
            |> List.rev
        num (getNum charLst) charLst.Length

    let private uniExpr op expr =
        ce.UniExpression
            {| Operator = op
               Expression = expr |}

    let private binExpr op expr1 expr2 =
        ce.BinaryExpression
            {| BinOperator = op
               LHS = expr1
               RHS = expr2 |}

    let constExprEvalPrimaryTests =
        ceEvalList "ConstExprEval Primary Tests" [ 
            "Evaluates number correctly",
                VNum 1,
                    num 1 32

            "Evaluates concat correctly",
                VNum 15,
                    ce.Primary
                        (cp.Concat [ num 3 2; num 3 2 ])

            "Concat truncates correctly",
                VNum 15,
                    ce.Primary
                        (cp.Concat [ num 7 2; num 7 2 ])

            "Evaluates brackets correctly",
                VNum 1,
                    ce.Primary
                        (cp.Brackets (num 1 32))

            "Brackets take precedence",
                VNum 14,
                    ce.BinaryExpression
                        {| BinOperator = Multiply
                           LHS = num 2 32
                           RHS = ce.BinaryExpression
                               {| BinOperator = Plus
                                  LHS = num 3 32
                                  RHS = num 4 32 |} |}
        ]

    let constExprEvalUniTests = 
        ceEvalList "ConstExprEval Unary Tests" [
            "Plus evaluates correctly",
                VNum 1,
                    uniExpr
                        UnaryOperatorT.Plus
                       (num 1 32)

            "Minus evaluates correctly",
                VNum 15,
                    uniExpr
                        UnaryOperatorT.Minus
                        (num 1 4)

            "Logical negation evaluates correctly",
                VNum 0,
                    uniExpr
                        LogicalNegation
                        (num 12 32)

            "Logical negation evals correctly - goes to 1 when 0",
                VNum 1,
                    uniExpr
                        LogicalNegation
                        (num 0 32)

            "Bitwise negation evals correctly",
                VNum 15,
                    uniExpr
                        BitwiseNegation
                        (num 0 4)

            "Reduction AND evals correctly - case 1",
                VNum 1,
                    uniExpr
                        ReductionAnd
                        (num 15 4)

            "Reduction AND evals correctly - case 2",
                VNum 0,
                    uniExpr
                        ReductionAnd
                        (num 14 4)

            "Reduction NAND evals correctly - case 1",
                VNum 0,
                    uniExpr
                        ReductionNand
                        (num 15 4)

            "Reduction NAND evals correctly - case 2",
                VNum 1,
                    uniExpr
                        ReductionNand
                        (num 14 4)

            "Reduction OR evals correctly - case 1",
                VNum 0,
                    uniExpr
                        ReductionOr
                        (num 0 4)

            "Reduction OR evals correctly - case 2",
                VNum 1,
                    uniExpr
                        ReductionOr
                        (num 1 4)

            "Reduction NOR evals correctly - case 1",
                VNum 1,
                    uniExpr
                        ReductionNor
                        (num 0 4)

            "Reduction NOR evals correctly - case 2",
                VNum 0,
                    uniExpr
                        ReductionNor
                        (num 1 4)

            "Reduction XOR evals correctly - case 1",
                VNum 0,
                    uniExpr
                        ReductionXor
                        (num 5 4)

            "Reduction XOR evals correctly - case 2",
                VNum 1,
                    uniExpr
                        ReductionXor
                        (num 8 4)

            "Reduction XNOR evals correctly - case 1",
                VNum 1,
                    uniExpr
                        ReductionXnor
                        (num 5 4)

            "Reduction XNOR evals correctly - case 2",
                VNum 0,
                    uniExpr
                        ReductionXnor
                        (num 8 4)
        ]

    let constExprEvalBinaryTests =
        ceEvalList "ConstExprEval Binary Tests" [
            "Binary plus evaluates correctly",
                VNum 10,
                    binExpr
                        Plus
                        (num 7 32)
                        (num 3 32)

            "Binary minus evaluates correctly",
                VNum 4,
                    binExpr
                        Minus
                        (num 7 32)
                        (num 3 32)

            "Multiply evaluates correctly",
                VNum 21,
                    binExpr
                        Multiply
                        (num 7 32)
                        (num 3 32)

            "Divide evaluates correctly",
                VNum 4,
                    binExpr
                        Divide
                        (num 20 32)
                        (num 5 32)

            "Modulus evaluates correctly",
                VNum 3,
                    binExpr
                        Modulus
                        (num 23 32)
                        (num 4 32)

            "Logical equality evaluates correctly - case 1",
                VNum 0,
                    binExpr
                        LogicalEquality
                        (num 23 32)
                        (num 4 32)

            "Logical equality evaluates correctly - case 2",
                VNum 1,
                    binExpr
                        LogicalEquality
                        (num 23 32)
                        (num 23 32)

            "Logical inequality evaluates correctly - case 1",
                VNum 1,
                    binExpr
                        LogicalInequality
                        (num 23 32)
                        (num 4 32)

            "Logical inequality evaluates correctly - case 2",
                VNum 0,
                    binExpr
                        LogicalInequality
                        (num 23 32)
                        (num 23 32)

            // TODO: re-evaluate the case equality tests when implemented correctly
            "Case equality evaluates correctly - case 1",
                VNum 0,
                    binExpr
                        CaseEquality
                        (num 23 32)
                        (num 4 32)

            // TODO: re-evaluate the case equality tests when implemented correctly
            "Case equality evaluates correctly - case 2",
                VNum 1,
                    binExpr
                        CaseEquality
                        (num 23 32)
                        (num 23 32)

            // TODO: re-evaluate the case equality tests when implemented correctly
            "Case inequality evaluates correctly - case 1",
                VNum 1,
                    binExpr
                        CaseInequality
                        (num 23 32)
                        (num 4 32)

            // TODO: re-evaluate the case equality tests when implemented correctly
            "Case inequality evaluates correctly - case 2",
                VNum 0,
                    binExpr
                        CaseInequality
                        (num 23 32)
                        (num 23 32)

            "Logical AND evaluates correctly - case 1",
                VNum 1,
                    binExpr
                        LogicalAnd
                        (num 23 32)
                        (num 23 32)

            "Logical AND evaluates correctly - case 2",
                VNum 0,
                    binExpr
                        LogicalAnd
                        (num 0 32)
                        (num 23 32)

            "Logical OR evaluates correctly - case 1",
                VNum 1,
                    binExpr
                        LogicalOr
                        (num 23 32)
                        (num 0 32)

            "Logical OR evaluates correctly - case 2",
                VNum 0,
                    binExpr
                        LogicalOr
                        (num 0 32)
                        (num 0 32)

            "Power evaluates correctly",
                VNum 8,
                    binExpr
                        Power
                        (num 2 32)
                        (num 3 32)

            "LessThan evaluates correctly - v1 = v2",
                VNum 0,
                    binExpr
                        LessThan
                        (num 2 32)
                        (num 2 32)

            "LessThan evaluates correctly - v1 < v2",
                VNum 1,
                    binExpr
                        LessThan
                        (num 1 32)
                        (num 2 32)

            "LessThan evaluates correctly - v1 > v2",
                VNum 0,
                    binExpr
                        LessThan
                        (num 3 32)
                        (num 2 32)

            "LessThanOrEqual evaluates correctly - v1 = v2",
                VNum 1,
                    binExpr
                        LessThanOrEqual
                        (num 2 32)
                        (num 2 32)

            "LessThanOrEqual evaluates correctly - v1 < v2",
                VNum 1,
                    binExpr
                        LessThanOrEqual
                        (num 1 32)
                        (num 2 32)

            "LessThanOrEqual evaluates correctly - v1 > v2",
                VNum 0,
                    binExpr
                        LessThanOrEqual
                        (num 3 32)
                        (num 2 32)

            "GreaterThan evaluates correctly - v1 = v2",
                VNum 0,
                    binExpr
                        GreaterThan
                        (num 2 32)
                        (num 2 32)

            "GreaterThan evaluates correctly - v1 < v2",
                VNum 0,
                    binExpr
                        GreaterThan
                        (num 1 32)
                        (num 2 32)

            "GreaterThan evaluates correctly - v1 > v2",
                VNum 1,
                    binExpr
                        GreaterThan
                        (num 3 32)
                        (num 2 32)

            "GreaterThanOrEqual evaluates correctly - v1 = v2",
                VNum 1,
                    binExpr
                        GreaterThanOrEqual
                        (num 2 32)
                        (num 2 32)

            "GreaterThanOrEqual evaluates correctly - v1 < v2",
                VNum 0,
                    binExpr
                        GreaterThanOrEqual
                        (num 1 32)
                        (num 2 32)

            "GreaterThanOrEqual evaluates correctly - v1 > v2",
                VNum 1,
                    binExpr
                        GreaterThanOrEqual
                        (num 3 32)
                        (num 2 32)

            "Bitwise AND evaluates correctly",
                VNum 8,
                    binExpr
                        BitwiseAnd
                        (bin "1011")
                        (bin "1000")

            "Bitwise OR evaluates correctly",
                VNum 11,
                    binExpr
                        BitwiseOr
                        (bin "1011")
                        (bin "1000")

            "Bitwise XOR evaluates correctly",
                VNum 3,
                    binExpr
                        BitwiseXor
                        (bin "1011")
                        (bin "1000")

            "Bitwise XNOR evaluates correctly",
                VNum 12,
                    binExpr
                        BitwiseXnor
                        (bin "1011")
                        (bin "1000")

            "Lofical right shift evaluates correctly",
                VNum 2,
                    binExpr
                        LogicalRightShift
                        (num 8 32)
                        (num 2 32)

            "Lofical left shift evaluates correctly",
                VNum 32,
                    binExpr
                        LogicalLeftShift
                        (num 8 32)
                        (num 2 32)

            "Arithmetic right shift evaluates correctly",
                VNum 2,
                    binExpr
                        ArithmeticRightShift
                        (num 8 32)
                        (num 2 32)

            "Arithmetic left shift evaluates correctly",
                VNum 32,
                    binExpr
                        ArithmeticLeftShift
                        (num 8 32)
                        (num 2 32)
        ]

    let constExprEvalTernaryTests =
        ceEvalList "ConstExprEval Ternary Tests" [
            "Conditional evaluates true correctly when true",
                VNum 5,
                    ce.CondExpression 
                        {| Condition = num 1 1
                           TrueVal = num 5 32
                           FalseVal = num 10 32 |}
                           
            "Conditional evaluates true correctly when false",
                VNum 10,
                    ce.CondExpression 
                        {| Condition = num 0 1
                           TrueVal = num 5 32
                           FalseVal = num 10 32 |}
        ]

    let allTests =
        testList "All Const Expression Tests" [
            constExprEvalPrimaryTests
            constExprEvalUniTests
            constExprEvalBinaryTests
            constExprEvalTernaryTests
        ]