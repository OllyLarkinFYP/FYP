open System
open MethodDispatcher

let getJob () =
    // TODO: get 4 bytes stating how long the message will be
    // TODO: get n bytes of message
    // TODO: potential issues: if they go out of sync from poorly constructed message?
    Console.ReadLine()

let postReply reply = 
    // TODO: reply should be posed using same format as getJob():
    // TODO: sned 4 bytes stating how long the message will be
    // TODO: send n bytes of message
    printfn "%s" reply

[<EntryPoint>]
let main _ =
    printfn "%s" <| MethodDispatcher.GetSerializedExternalDeclaration()
    MethodDispatcher(getJob, postReply).Start()
    0
