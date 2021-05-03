namespace Netlist

open AST
open CommonTypes

type Range = 
    | Single
    | Ranged of uint32 * uint32
    with
        member this.size () =
            match this with
            | Single -> 1u
            | Ranged (msb, lsb) -> msb - lsb + 1u

type OutputRegContent =
    { mutable initVal: VNum }

type RegCompContent =
    { range: Range
      mutable initVal: VNum }

type Component =
    | InputComp of Range
    | OutputReg of OutputRegContent
    | OutputWire
    | ModuleInst of IdentifierT
    | Expression of ExpressionT
    | Always of AlwaysConstructT
    | RegComp of RegCompContent
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
        static member initOutputReg range = { comp = OutputReg; inputs = [|{ range = range; connections = [||] }|] }
        static member initOutputWire range = { comp = OutputWire; inputs = [|{ range = range; connections = [||] }|] }

type Netlist = 
    { modDec: ModuleDeclaration
      nodes: Map<IdentifierT,Node> }

/// Different modules listed with their names as the key
type NetlistCollection = Map<IdentifierT,Netlist>
