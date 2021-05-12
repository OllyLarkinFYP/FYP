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
    { range: Range
      mutable initVal: VNum
      mutable drivers: RegDriver list }

type WireDriverType =
    | WireExpressionOutput of ExpressionT * IdentifierT list
    | WireModuleOutput of ModuleOutputContent
    | WireUnassigned

type WireDriver = Range * WireDriverType

type WireContent =
    { range: Range
      mutable drivers: WireDriver list }

type VariableComp =
    | InputComp of Range
    | RegComp of RegContent
    | WireComp of WireContent

type ModuleInputDriver =
    | ModExpressionOutput of ExpressionT * IdentifierT list
    | ModUnassigned

type ModuleInstanceComp =
    { moduleName: IdentifierT
      mutable drivers: ModuleInputDriver list }

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
