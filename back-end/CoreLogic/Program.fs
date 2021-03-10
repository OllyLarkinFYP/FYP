open System
open Parser
open FParsec

[<EntryPoint>]
let main argv =
    let testString = "1 + 2"
    let parser = ConstantExpression.pConstantExpression
    let result = run parser testString
    printfn "%A" result
    0 // return an integer exit code