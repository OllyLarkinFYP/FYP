open System
open System.Net
open System.Net.Sockets
open MethodDispatcher
open StdComm

let log (str: string) =
    if str.Length > 0
    then
        str.Split([|'\n'|])
        |> Array.map (fun line -> "[CORE] " + line + "\n")
        |> Array.reduce (+)
        |> printf "%s"

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
        log <| sprintf "Listening on port %i..." port

        let handler = s.Accept()
        let stream = new NetworkStream(handler)
        log "Connected."

        let io = IO(stream, stream)
        io.send(MethodDispatcher.GetSerializedExternalDeclaration())
        MethodDispatcher(io.receive, io.send).Start()
    0
