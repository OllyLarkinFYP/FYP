open System
open MethodDispatcher
open StdComm

[<EntryPoint>]
let main _ =
    printfn "%s" <| MethodDispatcher.GetSerializedExternalDeclaration()
    
    let io = IO(Console.OpenStandardInput(), Console.OpenStandardOutput())
    MethodDispatcher(io.receive, io.send).Start()
    0
