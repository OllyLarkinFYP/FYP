namespace rec Netlist

open System
open AST
open CommonTypes

/// Different modules listed with their names as the key
type NetlistCollection = Map<IdentifierT,Netlist>

type PortType =
    | Wire
    | Logic
    | Reg

type Direction =
    | Input
    | Output

type ModuleDeclaration =
    { name: IdentifierT
      ports: (IdentifierT * Direction) array }

type Netlist = 
    { modDec: ModuleDeclaration
      nodes: Map<IdentifierT,Node> }

type Node = 
    { comp: Component
      inputs: Port array }

type Component =
    | InputNode of IdentifierT
    | OutputNode of IdentifierT
    | ModuleInst of IdentifierT
    | Expression of ExpressionT
    | Always of AlwaysConstructT
    | Declaration of {| range: Range; portType: PortType; initVal: VNum |}

type Port = 
    { range: Range
      connections: Connection array }

type Range = 
    | Single
    | Ranged of uint32 * uint32
    with
        member this.size () =
            match this with
            | Single -> 1u
            | Ranged (msb, lsb) -> msb - lsb + 1u

type Connection = 
    { myRange: Range
      theirName: string
      theirPinNum: uint
      theirRange: Range }
