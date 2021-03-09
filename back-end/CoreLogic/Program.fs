open System
open Parser
open FParsec

[<EntryPoint>]
let main argv =
    let testString = "always"
    let parser = Token.Keyword.pAlways
    let result = run parser testString
    printfn "%A" result
    0 // return an integer exit code