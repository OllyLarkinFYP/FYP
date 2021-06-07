open System
open System.Net
open System.Net.Sockets
open MethodDispatcher
open StdComm

[<EntryPoint>]
let main argv =
    if argv.Length > 0
    then
        let port = int argv.[0]
        let ipAddress = IPAddress.Loopback
        let endPoint = IPEndPoint(ipAddress, port)

        let s = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        s.Bind(endPoint)
        s.Listen()
        printfn "Listening on port %i..." port

        let handler = s.Accept()
        let stream = new NetworkStream(handler)
        printfn "Connected. Writable: %b, Readable: %b" stream.CanWrite stream.CanRead

        let io = IO(stream, stream)
        io.send(MethodDispatcher.GetSerializedExternalDeclaration())
        MethodDispatcher(io.receive, io.send).Start()
    0
