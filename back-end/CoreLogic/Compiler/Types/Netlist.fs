namespace Netlist

open AST
open CommonTypes

type ModuleDeclaration =
    { name: IdentifierT
      ports: (IdentifierT * PortDirAndType * Range) list }

type ModuleOutputContent =
    { instanceName: IdentifierT
      portName: IdentifierT
      range: Range }

type RegDriverType =
    | RegExpressionOutput of ExpressionT * IdentifierT list
    | RegModuleOutput of ModuleOutputContent
    | RegAlwaysOutput of uint 
    | RegUnassigned

type RegDriver = Range * RegDriverType

type RegContent =
    { initVal: VNum
      range: Range
      mutable drivers: RegDriver }

type WireDriverType =
    | WireExpressionOutput of ExpressionT * IdentifierT list
    | WireModuleOutput of ModuleOutputContent
    | WireUnassigned

type WireDriver = Range * WireDriverType

type WireContent =
    { range: Range
      mutable drivers: WireDriver }

type VariableComp =
    | Input of Range
    | Reg of RegContent
    | Wire of WireContent

type ModuleInputDriver =
    | ModExpressionOutput of ExpressionT * IdentifierT list
    | ModUnassigned

type ModuleInstanceComp =
    { moduleName: IdentifierT
      mutable drivers: ModuleInputDriver }

type AlwaysComp =
    { eventControl: EventControlT
      statement: StatementOrNullT
      inputs: IdentifierT list }

type Netlist = 
    { moduleDeclaration: ModuleDeclaration
      variables: Map<IdentifierT,VariableComp> // includes input/wire/reg
      moduleInstances: Map<IdentifierT,ModuleInstanceComp>
      alwaysBlocks: Map<uint,AlwaysComp> }

/// Different modules listed with their names as the key
type NetlistCollection = Map<IdentifierT,Netlist>
