open System
open MethodDispatcher

let help = """
The CLI was used incorrectly, either by not providing arguments or providing invalid arguments.
Help is provided below:

arguments:
    -d       : Use to print the exposed module declarations to stdout in JSON format
    -j <job> : Use to process a job - the output will be printed to stdout
"""

[<EntryPoint>]
let main argv =
    if argv.Length >= 1 && argv.[0] = "-d"
    then printfn "%s" <| MethodDispatcher.GetSerializedExternalDeclaration()
    else 
        if argv.Length >= 2 && argv.[0] = "-j"
        then
            let job = argv.[1]
            match MethodDispatcher.ProcessMethodRequest job with
            | Ok reply -> printfn "%s" reply
            | Error err -> eprintfn "%s" err
        else printfn "%s" help 
    0
