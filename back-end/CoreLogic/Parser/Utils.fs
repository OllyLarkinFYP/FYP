namespace Parser

open FParsec
open AST

module Utils =
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