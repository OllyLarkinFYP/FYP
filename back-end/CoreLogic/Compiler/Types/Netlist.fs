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
        with
            override this.ToString() =
                let vm =
                    this.varMap
                    |> Map.toList
                    |> List.map (fun (key, value) -> sprintf "\t[%A, %A]\n" key value)
                    |> fun lst ->
                        if lst.Length <> 0
                        then List.reduce (+) lst 
                        else ""
                let i =
                    this.initial
                    |> List.map (fun item -> sprintf "\t%A\n" item)
                    |> fun lst ->
                        if lst.Length <> 0
                        then List.reduce (+) lst 
                        else ""
                let a =
                    this.alwaysBlocks
                    |> List.map (fun item -> sprintf "\t%A\n" item)
                    |> fun lst ->
                        if lst.Length <> 0
                        then List.reduce (+) lst 
                        else ""
                sprintf " varMap =\n%s\n initial =\n%s\n alwaysBlocks =\n%s\n" vm i a

        