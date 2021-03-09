namespace Parser

open FParsec
open AST
open Utils

module Token =
    module Keyword =
        let pAlways: Parser<unit, unit> = skipStrWs "always"
        let pAssign: Parser<unit, unit> = skipStrWs "assign"
        let pBegin: Parser<unit, unit> = skipStrWs "begin"
        let pCase: Parser<unit, unit> = skipStrWs "case"
        let pElse: Parser<unit, unit> = skipStrWs "else"
        let pEnd: Parser<unit, unit> = skipStrWs "end"
        let pEndCase: Parser<unit, unit> = skipStrWs "endcase"
        let pEndModule: Parser<unit, unit> = skipStrWs "endmodule"
        let pIf: Parser<unit, unit> = skipStrWs "if"
        let pInitial: Parser<unit, unit> = skipStrWs "initial"
        let pInput: Parser<unit, unit> = skipStrWs "input"
        let pLogic: Parser<unit, unit> = skipStrWs "logic"
        let pModule: Parser<unit, unit> = skipStrWs "module"
        let pNegedge: Parser<unit, unit> = skipStrWs "negedge"
        let pOr: Parser<unit, unit> = skipStrWs "or"
        let pOutput: Parser<unit, unit> = skipStrWs "output"
        let pPosedge: Parser<unit, unit> = skipStrWs "posedge"
        let pReg: Parser<unit, unit> = skipStrWs "reg"
        let pSigned: Parser<unit, unit> = skipStrWs "signed"
        let pWire: Parser<unit, unit> = skipStrWs "wire"

    module Symbols =
        let pSemiColon: Parser<unit, unit> = skipCharWs ';'
        let pColon: Parser<unit, unit> = skipCharWs ':'
        let pComma: Parser<unit, unit> = skipCharWs ','
        let pPeriod: Parser<unit, unit> = skipCharWs '.'
        let pAssign: Parser<unit, unit> = skipCharWs '='
        let pNonBlockAssign: Parser<unit, unit> = skipStrWs "<="
        let pAt: Parser<unit, unit> = skipCharWs '@'
        let pStar: Parser<unit, unit> = skipCharWs '*'
        let pQMark: Parser<unit, unit> = skipCharWs '?'
        let pOpenRBrac: Parser<unit, unit> = skipCharWs '('
        let pCloseRBrac: Parser<unit, unit> = skipCharWs ')'
        let pOpenSBrac: Parser<unit, unit> = skipCharWs '['
        let pCloseSBrac: Parser<unit, unit> = skipCharWs ']'
        let pOpenCBrac: Parser<unit, unit> = skipCharWs '{'
        let pCloseCBrac: Parser<unit, unit> = skipCharWs '}'

    module Operator =
        module Unary =
            let pPlus: Parser<UnaryOperatorT, unit> = charReturnWs '+' UnaryOperatorT.Plus
            let pMinus: Parser<UnaryOperatorT, unit> = charReturnWs '-' UnaryOperatorT.Minus
            let pLogicalNegation: Parser<UnaryOperatorT, unit> = charReturnWs '!' LogicalNegation
            let pBitwiseNegation: Parser<UnaryOperatorT, unit> = charReturnWs '~' BitwiseNegation
            let pReductionAnd: Parser<UnaryOperatorT, unit> = charReturnWs '&' ReductionAnd
            let pReductionNand: Parser<UnaryOperatorT, unit> = stringReturnWs "~&" ReductionNand
            let pReductionOr: Parser<UnaryOperatorT, unit> = charReturnWs '|' ReductionOr
            let pReductionNor: Parser<UnaryOperatorT, unit> = stringReturnWs "~|" ReductionNor
            let pReductionXor: Parser<UnaryOperatorT, unit> = charReturnWs '^' ReductionXor
            let pReductionXnor: Parser<UnaryOperatorT, unit> = stringReturnChoiceWs ["~^"; "^~"] ReductionXnor

        module Binary =
            let pPlus: Parser<BinaryOperatorT, unit> = charReturnWs '+' Plus
            let pMinus: Parser<BinaryOperatorT, unit> = charReturnWs '-' Minus
            let pMultiply: Parser<BinaryOperatorT, unit> = charReturnWs '*' Multiply
            let pDivide: Parser<BinaryOperatorT, unit> = charReturnWs '/' Divide
            let pModulus: Parser<BinaryOperatorT, unit> = charReturnWs '%' Modulus
            let pLogicalEquality: Parser<BinaryOperatorT, unit> = stringReturnWs "==" LogicalEquality
            let pLogicalInequality: Parser<BinaryOperatorT, unit> = stringReturnWs "!=" LogicalInequality
            let pCaseEquality: Parser<BinaryOperatorT, unit> = stringReturnWs "===" CaseEquality
            let pCaseInequality: Parser<BinaryOperatorT, unit> = stringReturnWs "!==" CaseInequality
            let pLogicalAnd: Parser<BinaryOperatorT, unit> = stringReturnWs "&&" LogicalAnd
            let pLogicalOr: Parser<BinaryOperatorT, unit> = stringReturnWs "||" LogicalOr
            let pPower: Parser<BinaryOperatorT, unit> = stringReturnWs "**" Power
            let pLessThan: Parser<BinaryOperatorT, unit> = charReturnWs '<' LessThan
            let pLessThanOrEqual: Parser<BinaryOperatorT, unit> = stringReturnWs "<=" LessThanOrEqual
            let pGreaterThan: Parser<BinaryOperatorT, unit> = charReturnWs '>' GreaterThan
            let pGreaterThanOrEqual: Parser<BinaryOperatorT, unit> = stringReturnWs ">=" GreaterThanOrEqual
            let pBitwiseAnd: Parser<BinaryOperatorT, unit> = charReturnWs '&' BitwiseAnd
            let pBitwiseOr: Parser<BinaryOperatorT, unit> = charReturnWs '|' BitwiseOr
            let pBitwiseXor: Parser<BinaryOperatorT, unit> = charReturnWs '^' BitwiseXor
            let pBitwiseXnor: Parser<BinaryOperatorT, unit> = stringReturnChoiceWs ["~^"; "^~"] BitwiseXnor
            let pLogicalRightShift: Parser<BinaryOperatorT, unit> = stringReturnWs ">>" LogicalRightShift
            let pLogicalLeftShift: Parser<BinaryOperatorT, unit> = stringReturnWs "<<" LogicalLeftShift
            let pArithmaticRightShift: Parser<BinaryOperatorT, unit> = stringReturnWs ">>>" ArithmaticRightShift
            let pArithmaticLeftShift: Parser<BinaryOperatorT, unit> = stringReturnWs "<<<" ArithmaticLeftShift
