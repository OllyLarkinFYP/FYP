namespace Parser

open System
open FParsec
open AST
open Utils
open CommonTypes

module Token =
    module Keyword =
        let pAlways: Parser<unit, unit> = skipStrWs "always"
        let pAssign: Parser<unit, unit> = skipStrWs "assign"
        let pBegin: Parser<unit, unit> = skipStrWs "begin"
        let pCase: Parser<unit, unit> = skipStrWs "case"
        let pDefault: Parser<unit, unit> = skipStrWs "dfault"
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

    module Symbol =
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

    let pIdentifier: Parser<string, unit> =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '_'
        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_' || c = '$'
        IdentifierOptions(isAsciiIdStart=isAsciiIdStart, isAsciiIdContinue=isAsciiIdContinue)
        |> identifier
        .>> spaces

    let pNumber : Parser<VNum, unit> =
        let upperAndLower c =
            if isLower c
            then [c; Char.ToUpper c]
            else [Char.ToLower c; c] 
        let charToNum c =
            match isDigit c, isLower c with
            | true, _ -> uint64 c - uint64 '0'
            | _, true -> uint64 c - uint64 'a' + 10UL   // plus 10 so hex value
            | _, false -> uint64 c - uint64 'A' + 10UL  // plus 10 so hex value
        let concatChars (cLst: char list) =
            new string [| for c in cLst -> c |]
        let endOfNumber =
            notFollowedBy (hex <|> pchar '_' <|> anyOf ['x';'X'])
        let binaryValue = sepBy1 (many1 <| anyOf ['0';'1';'x';'X']) (skipChar '_') |>> (List.collect id >> concatChars) .>>? endOfNumber
        let octalValue = sepBy1 (many1 <| anyOf ['0';'1';'2';'3';'4';'5';'6';'7';'x';'X']) (skipChar '_') |>> (List.collect id >> concatChars) .>>? endOfNumber
        let decimalValue = sepBy1 (many1 digit) (skipChar '_') |>> (List.collect id >> concatChars) .>>? endOfNumber
        let hexValue = sepBy1 (many1 (hex <|> anyOf ['x';'X'])) (skipChar '_') |>> (List.collect id >> concatChars) .>>? endOfNumber
        let numBase b = 
            skipChar '\'' >>? opt (anyOf ['s';'S']) .>>? skipAnyOf (upperAndLower b) 
            |>> function
            | Some _ -> true
            | None -> false
        // TODO: use signed
        let numberWithBase =
            opt decimalValue 
            .>>.? choice [
                numBase 'b' .>>.? binaryValue |>> fun (signed, str) -> VNum.bin str
                numBase 'o' .>>.? octalValue |>> fun (signed, str) -> VNum.oct str
                numBase 'd' .>>.? decimalValue |>> fun (signed, str) -> VNum (uint64 str, 64u)
                numBase 'd' .>>? skipAnyOf ['x';'X'] .>>? endOfNumber |>> fun signed -> VNum.unknown 64u  // can't predict the size here
                numBase 'h' .>>.? hexValue |>> fun (signed, str) -> VNum.hex str
            ]
            |>> function
            | Some (sizeStr), num -> VNum(num.value, uint sizeStr, num.unknownBits)
            | None, num -> VNum(num.value, VNum.defaultSize, num.unknownBits)
        let numberWithoutBase =
            decimalValue |>> fun str -> VNum (uint64 str, VNum.defaultSize)
        (numberWithBase <|> numberWithoutBase) .>> spaces |>> fun num -> num.trim()
