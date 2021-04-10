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

    let evalConstPrimary (expr: ConstantPrimaryT) : VNum =
        match expr with
        | ConstantPrimaryT.Number n -> VNum(n.Value, Option.defaultValue VNum.defaultSize n.Size)
        | ConstantPrimaryT.Brackets ex -> evalConstExpr ex
        | ConstantPrimaryT.Concat c -> 
            // TODO: add checks for size of concat (dont want size to overflow)
            c
            |> List.map evalConstExpr
            |> VNum.concat

    let evalConstUni (op: UnaryOperatorT) (expr: ConstantExpressionT) = 
        let num = evalConstExpr expr
        match op with
        | UnaryOperatorT.Plus -> num
        | UnaryOperatorT.Minus -> - num
        | UnaryOperatorT.LogicalNegation -> if num = VNum(0, 1) then VNum(1, 1) else VNum(0, 1)
        | UnaryOperatorT.BitwiseNegation -> ~~~ num
        | UnaryOperatorT.ReductionAnd -> num.reduce (&&&) 
        | UnaryOperatorT.ReductionNand -> (num.reduce (&&&)) |> (fun a -> if a = VNum(0, 1) then VNum(1, 1) else VNum(0, 1))
        | UnaryOperatorT.ReductionOr -> num.reduce (|||)
        | UnaryOperatorT.ReductionNor -> (num.reduce (|||)) |> (fun a -> if a = VNum(0, 1) then VNum(1, 1) else VNum(0, 1))
        | UnaryOperatorT.ReductionXor -> num.reduce (^^^)
        | UnaryOperatorT.ReductionXnor -> (num.reduce (^^^)) |> (fun a -> if a = VNum(0, 1) then VNum(1, 1) else VNum(0, 1))

    let evalConstBin (op: BinaryOperatorT) (expr1: ConstantExpressionT) (expr2: ConstantExpressionT) = 
        raise <| NotImplementedException()

    let evalConstCond (cond: ConstantExpressionT) (trueExpr: ConstantExpressionT) (falseExpr: ConstantExpressionT) = 
        raise <| NotImplementedException() 
  