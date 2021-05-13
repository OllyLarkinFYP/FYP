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

// The range here represents the range of the output from the expression used
type ExpressionOutputContent =
    { expression: ExpressionT
      vars: IdentifierT list
      range: Range }

type RegDriverType =
    | RegExpressionOutput of ExpressionOutputContent
    | RegModuleOutput of ModuleOutputContent
    | RegAlwaysOutput of uint

type RegDriver = Range * RegDriverType

type RegContent =
    { range: Range
      mutable initVal: VNum
      mutable drivers: RegDriver list }

type WireDriverType =
    | WireExpressionOutput of ExpressionOutputContent
    | WireModuleOutput of ModuleOutputContent

// The range here represents the range of the wire that is driven
type WireDriver = Range * WireDriverType

type WireContent =
    // The range here represents the ramge of the wire
    { range: Range
      mutable drivers: WireDriver list }

type VariableComp =
    | InputComp of Range
    | RegComp of RegContent
    | WireComp of WireContent
    with 
        member this.range =
            match this with
            | InputComp r -> r
            | RegComp rc -> rc.range
            | WireComp wc -> wc.range
        static member getRange (vc: VariableComp) = vc.range

// The IdentifierT refers to the port name
type ModuleInputDriver = IdentifierT * Range * ExpressionOutputContent

type ModuleInstanceComp =
    { moduleName: IdentifierT
      drivers: ModuleInputDriver list }

type AlwaysComp =
    { eventControl: EventControlT
      statement: StatementOrNullT
      inputs: IdentifierT list }

type Netlist = 
    { moduleDeclaration: ModuleDeclaration
      variables: Map<IdentifierT,VariableComp> // includes input/wire/reg
      moduleInstances: Map<IdentifierT,ModuleInstanceComp>
      alwaysBlocks: Map<uint,AlwaysComp> }
    with
        override this.ToString () =
            let displayMap (m: Map<'a,'b>) =
                m
                |> Map.toList
                |> List.map (fun elem -> "\t" + elem.ToString() + "\n")
                |> function
                | [] -> ""
                | a -> List.reduce (+) a
            let title = "Netlist:\n"
            let modDec = sprintf "ModuleDeclaration: \n\t%A\n" this.moduleDeclaration
            let variables = "Variables: \n" + displayMap this.variables
            let moduleInstances = "Module Instances: \n" + displayMap this.moduleInstances
            let alwaysBlocks = "Always Blocks: \n" + displayMap this.alwaysBlocks
            title + modDec + variables + moduleInstances + alwaysBlocks
            

/// Different modules listed with their names as the key
type NetlistCollection = Map<IdentifierT,Netlist>
