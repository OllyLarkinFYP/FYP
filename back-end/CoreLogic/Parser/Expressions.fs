namespace Parser

open System
open FParsec
open AST
open Token

module ConstantExpression =
    let private opp = new OperatorPrecedenceParser<ConstantExpressionT, unit, unit>()

    let pConstantExpression: Parser<ConstantExpressionT, unit> = opp.ExpressionParser

    let pConstantConcatenation: Parser<ConstantConcatenationT, unit> =
        Symbol.pOpenCBrac >>. sepBy1 pConstantExpression Symbol.pComma .>> Symbol.pCloseCBrac

    let pConstantRangeExpression: Parser<ConstantRangeExpressionT, unit> =
        pConstantExpression .>>. opt (Symbol.pColon >>. pConstantExpression)
        |>> function
        | (a, Some b) -> ConstantRangeExpressionT.Range {| LHS = a; RHS = b |}
        | (a, None) -> ConstantRangeExpressionT.Expr a

    // Constant Primary parsing
    opp.TermParser <- choice [
        pNumber |>> ConstantPrimaryT.Number
        Symbol.pOpenRBrac >>. pConstantExpression .>> Symbol.pCloseRBrac |>> ConstantPrimaryT.Brackets
        pConstantConcatenation |>> ConstantPrimaryT.Concat
    ] |>> ConstantExpressionT.Primary

    let private binExpr e a b =
        ConstantExpressionT.BinaryExpression {| LHS = a; BinOperator = e; RHS = b |}

    let private uniExpr e a =
        ConstantExpressionT.UniExpression {| Operator = e; Expression = a |}

    opp.AddOperator(TernaryOperator("?", spaces, ":", spaces, 1, Associativity.Right, fun a b c ->
        ConstantExpressionT.CondExpression {| Condition = a; TrueVal = b; FalseVal = c |}))

    opp.AddOperator(InfixOperator("||", spaces, 2, Associativity.Left, binExpr LogicalOr))
    opp.AddOperator(InfixOperator("&&", spaces, 3, Associativity.Left, binExpr LogicalAnd))
    opp.AddOperator(InfixOperator("|", spaces, 4, Associativity.Left, binExpr BitwiseOr))
    opp.AddOperator(InfixOperator("^", spaces, 5, Associativity.Left, binExpr BitwiseXor))
    opp.AddOperator(InfixOperator("~^", spaces, 5, Associativity.Left, binExpr BitwiseXnor))
    opp.AddOperator(InfixOperator("^~", spaces, 5, Associativity.Left, binExpr BitwiseXnor))
    opp.AddOperator(InfixOperator("&", spaces, 6, Associativity.Left, binExpr BitwiseAnd))
    opp.AddOperator(InfixOperator("==", spaces, 7, Associativity.Left, binExpr LogicalEquality))
    opp.AddOperator(InfixOperator("!=", spaces, 7, Associativity.Left, binExpr LogicalInequality))
    opp.AddOperator(InfixOperator("===", spaces, 7, Associativity.Left, binExpr CaseEquality))
    opp.AddOperator(InfixOperator("!==", spaces, 7, Associativity.Left, binExpr CaseInequality))
    opp.AddOperator(InfixOperator("<", spaces, 8, Associativity.Left, binExpr LessThan))
    opp.AddOperator(InfixOperator(">", spaces, 8, Associativity.Left, binExpr GreaterThan))
    opp.AddOperator(InfixOperator("<=", spaces, 8, Associativity.Left, binExpr LessThanOrEqual))
    opp.AddOperator(InfixOperator(">=", spaces, 8, Associativity.Left, binExpr GreaterThanOrEqual))
    opp.AddOperator(InfixOperator("<<", spaces, 9, Associativity.Left, binExpr LogicalLeftShift))
    opp.AddOperator(InfixOperator(">>", spaces, 9, Associativity.Left, binExpr LogicalRightShift))
    opp.AddOperator(InfixOperator("<<<", spaces, 9, Associativity.Left, binExpr ArithmeticLeftShift))
    opp.AddOperator(InfixOperator(">>>", spaces, 9, Associativity.Left, binExpr ArithmeticRightShift))
    opp.AddOperator(InfixOperator("+", spaces, 10, Associativity.Left, binExpr Plus))
    opp.AddOperator(InfixOperator("-", spaces, 10, Associativity.Left, binExpr Minus))
    opp.AddOperator(InfixOperator("*", spaces, 11, Associativity.Left, binExpr Multiply))
    opp.AddOperator(InfixOperator("/", spaces, 11, Associativity.Left, binExpr Divide))
    opp.AddOperator(InfixOperator("%", spaces, 11, Associativity.Left, binExpr Modulus))
    opp.AddOperator(InfixOperator("**", spaces, 12, Associativity.Left, binExpr Power))
    
    // All unary operators have the highest precedence
    opp.AddOperator(PrefixOperator("+", spaces, 13, true, uniExpr UnaryOperatorT.Plus))
    opp.AddOperator(PrefixOperator("-", spaces, 13, true, uniExpr UnaryOperatorT.Minus))
    opp.AddOperator(PrefixOperator("!", spaces, 13, true, uniExpr LogicalNegation))
    opp.AddOperator(PrefixOperator("~", spaces, 13, true, uniExpr BitwiseNegation))
    opp.AddOperator(PrefixOperator("&", spaces, 13, true, uniExpr ReductionAnd))
    opp.AddOperator(PrefixOperator("~&", spaces, 13, true, uniExpr ReductionNand))
    opp.AddOperator(PrefixOperator("|", spaces, 13, true, uniExpr ReductionOr))
    opp.AddOperator(PrefixOperator("~|", spaces, 13, true, uniExpr ReductionNor))
    opp.AddOperator(PrefixOperator("^", spaces, 13, true, uniExpr ReductionXor))
    opp.AddOperator(PrefixOperator("~^", spaces, 13, true, uniExpr ReductionXnor))
    opp.AddOperator(PrefixOperator("^~", spaces, 13, true, uniExpr ReductionXnor))



module Expression =
    let private opp = new OperatorPrecedenceParser<ExpressionT, unit, unit>()

    let pExpression: Parser<ExpressionT, unit> = opp.ExpressionParser

    let pConcatenation: Parser<ConcatenationT, unit> =
        Symbol.pOpenCBrac >>. sepBy1 pExpression Symbol.pComma .>> Symbol.pCloseCBrac

    let pRangeExpression: Parser<RangeExpressionT, unit> =
        pExpression .>>. opt (Symbol.pColon >>. pExpression)
        |>> function
        | (a, Some b) -> Range {| LHS = a; RHS = b |}
        | (a, None) -> Expr a

    // Constant Primary parsing
    opp.TermParser <- choice [
        pNumber |>> Number
        Symbol.pOpenRBrac >>. pExpression .>> Symbol.pCloseRBrac |>> Brackets
        pConcatenation |>> PrimaryT.Concat
        pIdentifier .>>. opt (Symbol.pOpenSBrac >>. pRangeExpression .>> Symbol.pCloseSBrac) |>> fun (iden, range) ->
            PrimaryT.Ranged {| Name = iden; Range = range |}
    ] |>> Primary

    let private binExpr e a b =
        BinaryExpression {| LHS = a; BinOperator = e; RHS = b |}

    let private uniExpr e a =
        UniExpression {| Operator = e; Expression = a |}

    opp.AddOperator(TernaryOperator("?", spaces, ":", spaces, 1, Associativity.Right, fun a b c ->
        CondExpression {| Condition = a; TrueVal = b; FalseVal = c |}))

    opp.AddOperator(InfixOperator("||", spaces, 2, Associativity.Left, binExpr LogicalOr))
    opp.AddOperator(InfixOperator("&&", spaces, 3, Associativity.Left, binExpr LogicalAnd))
    opp.AddOperator(InfixOperator("|", spaces, 4, Associativity.Left, binExpr BitwiseOr))
    opp.AddOperator(InfixOperator("^", spaces, 5, Associativity.Left, binExpr BitwiseXor))
    opp.AddOperator(InfixOperator("~^", spaces, 5, Associativity.Left, binExpr BitwiseXnor))
    opp.AddOperator(InfixOperator("^~", spaces, 5, Associativity.Left, binExpr BitwiseXnor))
    opp.AddOperator(InfixOperator("&", spaces, 6, Associativity.Left, binExpr BitwiseAnd))
    opp.AddOperator(InfixOperator("==", spaces, 7, Associativity.Left, binExpr LogicalEquality))
    opp.AddOperator(InfixOperator("!=", spaces, 7, Associativity.Left, binExpr LogicalInequality))
    opp.AddOperator(InfixOperator("===", spaces, 7, Associativity.Left, binExpr CaseEquality))
    opp.AddOperator(InfixOperator("!==", spaces, 7, Associativity.Left, binExpr CaseInequality))
    opp.AddOperator(InfixOperator("<", spaces, 8, Associativity.Left, binExpr LessThan))
    opp.AddOperator(InfixOperator(">", spaces, 8, Associativity.Left, binExpr GreaterThan))
    opp.AddOperator(InfixOperator("<=", spaces, 8, Associativity.Left, binExpr LessThanOrEqual))
    opp.AddOperator(InfixOperator(">=", spaces, 8, Associativity.Left, binExpr GreaterThanOrEqual))
    opp.AddOperator(InfixOperator("<<", spaces, 9, Associativity.Left, binExpr LogicalLeftShift))
    opp.AddOperator(InfixOperator(">>", spaces, 9, Associativity.Left, binExpr LogicalRightShift))
    opp.AddOperator(InfixOperator("<<<", spaces, 9, Associativity.Left, binExpr ArithmeticLeftShift))
    opp.AddOperator(InfixOperator(">>>", spaces, 9, Associativity.Left, binExpr ArithmeticRightShift))
    opp.AddOperator(InfixOperator("+", spaces, 10, Associativity.Left, binExpr Plus))
    opp.AddOperator(InfixOperator("-", spaces, 10, Associativity.Left, binExpr Minus))
    opp.AddOperator(InfixOperator("*", spaces, 11, Associativity.Left, binExpr Multiply))
    opp.AddOperator(InfixOperator("/", spaces, 11, Associativity.Left, binExpr Divide))
    opp.AddOperator(InfixOperator("%", spaces, 11, Associativity.Left, binExpr Modulus))
    opp.AddOperator(InfixOperator("**", spaces, 12, Associativity.Left, binExpr Power))
    
    // All unary operators have the highest precedence
    opp.AddOperator(PrefixOperator("+", spaces, 13, true, uniExpr UnaryOperatorT.Plus))
    opp.AddOperator(PrefixOperator("-", spaces, 13, true, uniExpr UnaryOperatorT.Minus))
    opp.AddOperator(PrefixOperator("!", spaces, 13, true, uniExpr LogicalNegation))
    opp.AddOperator(PrefixOperator("~", spaces, 13, true, uniExpr BitwiseNegation))
    opp.AddOperator(PrefixOperator("&", spaces, 13, true, uniExpr ReductionAnd))
    opp.AddOperator(PrefixOperator("~&", spaces, 13, true, uniExpr ReductionNand))
    opp.AddOperator(PrefixOperator("|", spaces, 13, true, uniExpr ReductionOr))
    opp.AddOperator(PrefixOperator("~|", spaces, 13, true, uniExpr ReductionNor))
    opp.AddOperator(PrefixOperator("^", spaces, 13, true, uniExpr ReductionXor))
    opp.AddOperator(PrefixOperator("~^", spaces, 13, true, uniExpr ReductionXnor))
    opp.AddOperator(PrefixOperator("^~", spaces, 13, true, uniExpr ReductionXnor))
