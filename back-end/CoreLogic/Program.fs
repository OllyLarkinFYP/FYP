open System
open MethodDispatcher

let getJob () = Console.ReadLine()
let postReply reply = printfn "%s" reply

[<EntryPoint>]
let main _ =
    printfn "%s" <| MethodDispatcher.GetSerializedExternalDeclaration()
    MethodDispatcher(getJob, postReply).Start()
    0
