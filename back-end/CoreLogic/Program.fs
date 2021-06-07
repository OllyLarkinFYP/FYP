open System
open MethodDispatcher
open StdComm

[<EntryPoint>]
let main _ =
    let io = IO(Console.OpenStandardInput(), Console.OpenStandardOutput())
    io.send(MethodDispatcher.GetSerializedExternalDeclaration())
    MethodDispatcher(io.receive, io.send).Start()
    0
