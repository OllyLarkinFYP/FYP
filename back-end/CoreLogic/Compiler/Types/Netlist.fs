namespace Compiler

open AST
open CommonTypes

module Netlist =

    type ReqVarList = (IdentifierT * Range) list

    type ExpContent =
        { expression: ExpressionT
          reqVars: ReqVarList }

    type Driver =
        { drivenRange: Range
          expRange: Range
          exp: ExpContent }

    type VarElem =
        | Input
        | Reg
        | Wire of Driver list

    type VarMap = Map<IdentifierT, VarElem * Range>

    module VarMap =
        let inputs (vm: VarMap) =
            vm
            |> Map.filter (fun _ v ->
                match v with
                | (Input, _) -> true
                | _ -> false)
        let wires (vm: VarMap) =
            vm
            |> Map.filter (fun _ v ->
                match v with
                | (Wire _, _) -> true
                | _ -> false)
        let regs (vm: VarMap) =
            vm
            |> Map.filter (fun _ v ->
                match v with
                | (Reg, _) -> true
                | _ -> false)

    type InitItem =
        { lhs: IdentifierT * Range
          rhs: VNum }

    type EventControlContent =
        { ec: EventControlT
          reqVars: ReqVarList }

    type StatementContent =
        { s: StatementOrNullT
          reqVars: ReqVarList }

    type AlwaysBlock = 
        { eventControl: EventControlContent
          statement: StatementContent }

    type Netlist =
        { varMap: VarMap
          initial: InitItem list
          alwaysBlocks: AlwaysBlock list }
        