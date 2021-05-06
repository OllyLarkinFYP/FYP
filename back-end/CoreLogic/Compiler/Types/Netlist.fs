namespace Netlist

open AST
open CommonTypes

type Range = 
    | Single of uint32
    | Ranged of uint32 * uint32
    with
        member this.size () =
            match this with
            | Single _ -> 1u
            | Ranged (msb, lsb) -> msb - lsb + 1u

type RegContent =
    { range: Range
      mutable initVal: VNum }

type Component =
    | InputComp of Range
    | OutputReg of RegContent
    | OutputWire
    | ModuleInst of IdentifierT
    | Expression of ExpressionT
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

type ModuleDeclaration =
    { name: IdentifierT
      ports: (IdentifierT * PortDirAndType * Range) array }

type Node = 
    { comp: Component
      inputs: Port array }
    with
        static member initInputComp range = { comp = InputComp range; inputs = [||] }
        static member initOutputReg (range: Range) = { comp = OutputReg { range = range; initVal = VNum.unknown <| range.size() }; inputs = [|{ range = range; connections = [||] }|] }
        static member initOutputWire range = { comp = OutputWire; inputs = [|{ range = range; connections = [||] }|] }
        static member initRegComp (range: Range) = { comp = RegComp { range = range; initVal = VNum.unknown <| range.size() }; inputs = [|{ range = range; connections = [||] }|] }
        static member initWireComp range = { comp = WireComp range; inputs = [|{ range = range; connections = [||] }|] }

type Netlist = 
    { modDec: ModuleDeclaration
      nodes: Map<IdentifierT,Node> }

/// Different modules listed with their names as the key
type NetlistCollection = Map<IdentifierT,Netlist>
