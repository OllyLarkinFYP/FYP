namespace Netlist

open AST
open CommonTypes

type RegContent =
    { range: Range
      mutable initVal: VNum }

type Component =
    | InputComp of Range
    | OutputReg of RegContent
    | OutputWire of Range
    | ModuleInst of IdentifierT
    | Expression of ExpressionT * Map<string,uint>
    | Always of AlwaysConstructT
    | RegComp of RegContent
    | WireComp of Range

type Connection = 
    { myRange: Range
      theirName: string
      theirPinNum: uint
      theirRange: Range }

type Port = 
    { range: Range
      mutable connections: Connection array }
    with
        member this.addConnection (myName: string) myRange theirName theirPinNum theirRange =
            (false, this.connections)
            ||> Array.fold (fun overlap connection -> 
                if Range.overlap connection.myRange myRange
                then true
                else overlap)
            |> function
            // No overlap
            | false -> 
                this.connections <-
                    [|{ myRange = myRange; theirName = theirName; theirPinNum = theirPinNum; theirRange = theirRange }|]
                    |> Array.append this.connections
                Ok()
            // overlap with existing connections
            // TODO: improve this
            | true -> Error <| sprintf "%A has bits being driven by multiple components. Cannot also drive using %A." myName theirName 

type ModuleDeclaration =
    { name: IdentifierT
      ports: (IdentifierT * PortDirAndType * Range) array }

type Node = 
    { comp: Component
      inputs: Port array }
    with
        static member initInputComp range = { comp = InputComp range; inputs = [||] }
        static member initOutputReg (range: Range) = { comp = OutputReg { range = range; initVal = VNum.unknown <| range.size }; inputs = [|{ range = range; connections = [||] }|] }
        static member initOutputWire range = { comp = OutputWire range; inputs = [|{ range = range; connections = [||] }|] }
        static member initRegComp (range: Range) = { comp = RegComp { range = range; initVal = VNum.unknown <| range.size }; inputs = [|{ range = range; connections = [||] }|] }
        static member initWireComp range = { comp = WireComp range; inputs = [|{ range = range; connections = [||] }|] }

type Netlist = 
    { modDec: ModuleDeclaration
      nodes: Map<IdentifierT,Node> }

/// Different modules listed with their names as the key
type NetlistCollection = Map<IdentifierT,Netlist>
