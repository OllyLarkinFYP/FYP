namespace Parser

open FParsec
open CommonTypes
open AST

module Utils =

    let withPos (p: Parser<'T,UserState>) : Parser<WithPos<'T>,UserState> =
        pipe4 getUserState getPosition p getPosition
        <| fun file sPos value fPos ->
            { file = file
              start = sPos
              finish = fPos
              value = value }

    let skipStrWs s = skipString s .>> spaces

    let skipCharWs c = skipChar c .>> spaces

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
