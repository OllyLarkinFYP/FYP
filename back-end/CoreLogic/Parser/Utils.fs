namespace Parser

open FParsec
open AST

module Utils =
    let pComment: Parser<unit, unit> =
        let singleLine = skipString "//" .>> skipRestOfLine true
        let multiLine = skipString "/*" .>> skipCharsTillString "*/" true 100000
        (singleLine <|> multiLine) .>> spaces

    let skipWhiteSpaceAndComments = spaces .>> opt pComment

    let skipStrWs s = skipString s .>> skipWhiteSpaceAndComments

    let skipCharWs c = skipChar c .>> skipWhiteSpaceAndComments

    let stringReturnWs s r = skipStrWs s >>% r

    let charReturnWs c r = skipCharWs c >>% r

    let stringReturnChoiceWs sList r =
        sList
        |> List.map skipStrWs
        |> choice
        >>% r 

    let charReturnChoiceWs cList r =
        cList
        |> List.map skipCharWs
        |> choice
        >>% r 

    let notImplementedParser (chrStream: CharStream<'a>) : Reply<'b> =
        raise <| System.NotImplementedException()

    let printParser str (chrStream: CharStream<'a>) : Reply<unit> =
        printfn str
        preturn () chrStream
        