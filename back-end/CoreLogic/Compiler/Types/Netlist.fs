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

type Component =
    | InputComp of Range
    | OutputReg of Range
    | OutputWire of Range
    | ModuleInst of IdentifierT
    | Expression of ExpressionT
    | Always of AlwaysConstructT
    | RegComp of {| range: Range; initVal: VNum |}
    | WireComp of {| range: Range; initVal: VNum |}

type Connection = 
    { myRange: Range
      theirName: string
      theirPinNum: uint
      theirRange: Range }

type Port = 
    { range: Range
      connections: Connection array }

type ModuleDeclaration =
    { name: IdentifierT
      ports: (IdentifierT * PortDirAndType * Range) array }

type Node = 
    { comp: Component
      inputs: Port array }

type Netlist = 
    { modDec: ModuleDeclaration
      nodes: Map<IdentifierT,Node> }

/// Different modules listed with their names as the key
type NetlistCollection = Map<IdentifierT,Netlist>
