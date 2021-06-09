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

let help = """
The CLI was used incorrectly, either by not providing arguments or providing invalid arguments.
Help is provided below:

arguments:
    -d            : Use to print the exposed module declarations to stdout in JSON format
    -p <port_num> : Use to start the backend server on port <port_num> (this typically would
                    not be used when running the CLI by hand as it will be communicating via
                    sockets on localhost)
"""

[<EntryPoint>]
let main argv =
    if argv.Length >= 1 && argv.[0] = "-d"
    then printfn "%s" <| MethodDispatcher.GetSerializedExternalDeclaration()
    else 
        if argv.Length >= 2 && argv.[0] = "-p"
        then
            let port = int argv.[1]
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
            MethodDispatcher(io.receive, io.send).Start()
        else printfn "%s" help 
    0
