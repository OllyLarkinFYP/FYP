namespace Parser

open System
open FParsec
open AST
open Utils
open CommonTypes

module Token =
    module Keyword =
        let pAlways: Parser<unit, UserState> = skipStrWs "always"
        let pAssign: Parser<unit, UserState> = skipStrWs "assign"
        let pBegin: Parser<unit, UserState> = skipStrWs "begin"
        let pCase: Parser<unit, UserState> = skipStrWs "case"
        let pDefault: Parser<unit, UserState> = skipStrWs "default"
        let pElse: Parser<unit, UserState> = skipStrWs "else"
        let pEnd: Parser<unit, UserState> = skipStrWs "end"
        let pEndCase: Parser<unit, UserState> = skipStrWs "endcase"
        let pEndModule: Parser<unit, UserState> = skipStrWs "endmodule"
        let pIf: Parser<unit, UserState> = skipStrWs "if"
        let pInitial: Parser<unit, UserState> = skipStrWs "initial"
        let pInput: Parser<unit, UserState> = skipStrWs "input"
        let pModule: Parser<unit, UserState> = skipStrWs "module"
        let pNegedge: Parser<unit, UserState> = skipStrWs "negedge"
        let pOr: Parser<unit, UserState> = skipStrWs "or"
        let pOutput: Parser<unit, UserState> = skipStrWs "output"
        let pPosedge: Parser<unit, UserState> = skipStrWs "posedge"
        let pReg: Parser<unit, UserState> = skipStrWs "reg"
        let pWire: Parser<unit, UserState> = skipStrWs "wire"

    module Symbol =
        let pSemiColon: Parser<unit, UserState> = skipCharWs ';'
        let pColon: Parser<unit, UserState> = skipCharWs ':'
        let pComma: Parser<unit, UserState> = skipCharWs ','
        let pPeriod: Parser<unit, UserState> = skipCharWs '.'
        let pAssign: Parser<unit, UserState> = skipCharWs '='
        let pNonBlockAssign: Parser<unit, UserState> = skipStrWs "<="
        let pAt: Parser<unit, UserState> = skipCharWs '@'
        let pStar: Parser<unit, UserState> = skipCharWs '*'
        let pQMark: Parser<unit, UserState> = skipCharWs '?'
        let pOpenRBrac: Parser<unit, UserState> = skipCharWs '('
        let pCloseRBrac: Parser<unit, UserState> = skipCharWs ')'
        let pOpenSBrac: Parser<unit, UserState> = skipCharWs '['
        let pCloseSBrac: Parser<unit, UserState> = skipCharWs ']'
        let pOpenCBrac: Parser<unit, UserState> = skipCharWs '{'
        let pCloseCBrac: Parser<unit, UserState> = skipCharWs '}'

    let pIdentifier: Parser<string, UserState> =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '_'
        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_' || c = '$'
        IdentifierOptions(isAsciiIdStart=isAsciiIdStart, isAsciiIdContinue=isAsciiIdContinue)
        |> identifier
        .>> spaces

    let pNumber : Parser<VNum, UserState> =
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
            skipChar '\'' >>? skipAnyOf (upperAndLower b)
        let numberWithBase =
            opt decimalValue 
            .>>.? choice [
                numBase 'b' >>? binaryValue |>> VNum.bin
                numBase 'o' >>? octalValue |>> VNum.oct
                numBase 'd' >>? decimalValue |>> fun str -> VNum (uint64 str, 64u)
                numBase 'd' >>? skipAnyOf ['x';'X'] >>? endOfNumber >>% VNum.unknown 64u  // can't predict the size here
                numBase 'h' >>? hexValue |>> VNum.hex
            ]
            |>> function
            | Some (sizeStr), num -> VNum(num.value, uint sizeStr, num.unknownBits)
            | None, num -> VNum(num.value, VNum.defaultSize, num.unknownBits)
        let numberWithoutBase =
            decimalValue |>> fun str -> VNum (uint64 str, VNum.defaultSize)
        (numberWithBase <|> numberWithoutBase) .>> spaces |>> fun num -> num.trim()
