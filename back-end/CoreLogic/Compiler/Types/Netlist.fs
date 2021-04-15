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
    { name: string
      inputs: (string * Range * PortType) array
      outputs: (string * Range * PortType) array
      /// Map from instance name to node
      nodes: Map<string,Node> }

type Node = 
    { comp: Component
      inputs: Input array
      outputs: Output array }

type Component =
    | ModuleInst of Netlist
    | UnaryOp of UnaryOperatorT
    | BinaryOp of BinaryOperatorT
    | TernaryOp
    | Always of AlwaysConstructT

type Input = 
    { range: Range
      connections: Connection array }

type Output = 
    { range: Range
      connections: Connection array }

type Range = 
    | Single
    | Ranged of uint32 * uint32

type Connection = 
    { myRange: Range
      theirName: string
      theirPinNum: uint
      theirRange: Range }
