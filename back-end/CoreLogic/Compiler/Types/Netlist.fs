namespace rec Netlist

open System
open AST
open CommonTypes

/// Different modules listed with their names as the key
type NetlistCollection = Map<string,Netlist>

type PortType =
    | Wire
    | Logic
    | Reg

type Netlist = 
    { name: IdentifierT
      inputs: (string * PortType * Port) array
      outputs: (string * PortType * Port) array
      /// Map from instance name to node
      nodes: Map<string,Node> }

type Node = 
    { comp: Component
      inputs: Port array
      outputs: Port array }

type Component =
    | ModuleInst of IdentifierT
    // | UnaryOp of UnaryOperatorT
    // | BinaryOp of BinaryOperatorT
    // | TernaryOp
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
            | Ranged (msb, lsb) -> msb - lsb

type Connection = 
    { myRange: Range
      theirName: string
      theirPinNum: uint
      theirRange: Range }
