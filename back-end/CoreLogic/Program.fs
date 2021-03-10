open System
open Parser
open FParsec

[<EntryPoint>]
let main argv =
    let testString = "test[123:1'd4]"
    let parser = Expression.pExpression
    let result = run parser testString
    printfn "%A" result
    0 // return an integer exit code