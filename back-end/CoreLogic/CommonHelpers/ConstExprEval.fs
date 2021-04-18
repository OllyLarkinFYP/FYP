namespace CommonHelpers

open System
open CommonTypes
open AST

module rec ConstExprEval =
    let evalConstExpr (expr: ConstantExpressionT) : VNum =
        match expr with
        | ConstantExpressionT.Primary ex -> evalConstPrimary ex
        | ConstantExpressionT.UniExpression ex -> evalConstUni ex.Operator ex.Expression
        | ConstantExpressionT.BinaryExpression ex -> evalConstBin ex.BinOperator ex.LHS ex.RHS
        | ConstantExpressionT.CondExpression ex -> evalConstCond ex.Condition ex.TrueVal ex.FalseVal

    let private evalConstPrimary (expr: ConstantPrimaryT) : VNum =
        match expr with
        | ConstantPrimaryT.Number n -> n
        | ConstantPrimaryT.Brackets ex -> evalConstExpr ex
        | ConstantPrimaryT.Concat c -> 
            // TODO: add checks for size of concat (dont want size to overflow)
            c
            |> List.map evalConstExpr
            |> VNum.concat

    let private evalConstUni (op: UnaryOperatorT) (expr: ConstantExpressionT) = 
        let num = evalConstExpr expr
        let negate a = if a = VNum(0, 1) then VNum(1, 1) else VNum(0, 1)
        match op with
        | UnaryOperatorT.Plus -> num
        | UnaryOperatorT.Minus -> - num
        | UnaryOperatorT.LogicalNegation -> if num = VNum(0, 1) then VNum(1, 1) else VNum(0, 1)
        | UnaryOperatorT.BitwiseNegation -> ~~~ num
        | UnaryOperatorT.ReductionAnd -> num.reduce (&&&) 
        | UnaryOperatorT.ReductionNand -> (num.reduce (&&&)) |> negate
        | UnaryOperatorT.ReductionOr -> num.reduce (|||)
        | UnaryOperatorT.ReductionNor -> (num.reduce (|||)) |> negate
        | UnaryOperatorT.ReductionXor -> num.reduce (^^^)
        | UnaryOperatorT.ReductionXnor -> (num.reduce (^^^)) |> negate

    let private evalConstBin (op: BinaryOperatorT) (expr1: ConstantExpressionT) (expr2: ConstantExpressionT) = 
        let lhs = evalConstExpr expr1
        let rhs = evalConstExpr expr2
        match op with
        | Plus -> lhs + rhs
        | Minus -> lhs - rhs
        | Multiply -> lhs * rhs
        | Divide -> lhs / rhs   // TODO: maybe div 0 check here?
        | Modulus -> lhs % rhs
        | LogicalEquality -> lhs = rhs |> VNum
        | LogicalInequality -> lhs <> rhs |> VNum
        | CaseEquality -> lhs = rhs |> VNum     // TODO: correct when x is allowed: https://stackoverflow.com/questions/5927615/what-is-the-difference-between-and-in-verilog/5929076
        | CaseInequality -> lhs <> rhs |> VNum  // TODO: same as above
        | LogicalAnd -> (lhs.toBool() && rhs.toBool()) |> VNum
        | LogicalOr -> (lhs.toBool() || rhs.toBool()) |> VNum
        | Power -> lhs ^^ rhs
        | LessThan -> lhs < rhs |> VNum
        | LessThanOrEqual -> lhs <= rhs |> VNum
        | GreaterThan -> lhs > rhs |> VNum
        | GreaterThanOrEqual -> lhs >= rhs |> VNum
        | BitwiseAnd -> lhs &&& rhs
        | BitwiseOr -> lhs ||| rhs
        | BitwiseXor -> lhs ^^^ rhs
        | BitwiseXnor -> ~~~ (lhs ^^^ rhs)
        | LogicalRightShift -> VNum.(>>>) (lhs, rhs)
        | LogicalLeftShift -> VNum.(<<<) (lhs, rhs)
        | ArithmeticRightShift -> VNum.(>>>) (lhs, rhs)  // TODO: make arithmatic - check its actually needed
        | ArithmeticLeftShift -> VNum.(<<<) (lhs, rhs)    // TODO: same as above

    let private evalConstCond (cond: ConstantExpressionT) (trueExpr: ConstantExpressionT) (falseExpr: ConstantExpressionT) = 
        if ((evalConstExpr cond).toBool())
        then evalConstExpr trueExpr
        else evalConstExpr falseExpr
  