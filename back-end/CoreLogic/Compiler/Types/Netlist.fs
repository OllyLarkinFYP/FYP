namespace Compiler

open AST
open CommonTypes
open Compiler.Utils
open Compiler.CompResult

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
        with
            member this.getDriversFor range =
                match this with
                | Wire dl ->
                    dl
                    |> List.filter (fun driver ->
                        Range.overlap driver.drivenRange range)
                | _ -> []

    type Variable =
        { var: VarElem
          range: Range }
        with
            override this.ToString() =
                sprintf "{ var = %s; range = %s }" (this.var.ToString()) (this.range.ToString())

    type VarMap = Map<IdentifierT, Variable>

    module VarMap =
        let inputs (vm: VarMap) =
            vm
            |> Map.filter (fun _ v ->
                match v.var with
                | Input -> true
                | _ -> false)
        let wires (vm: VarMap) =
            vm
            |> Map.filter (fun _ v ->
                match v.var with
                | Wire _ -> true
                | _ -> false)
        let regs (vm: VarMap) =
            vm
            |> Map.filter (fun _ v ->
                match v.var with
                | Reg -> true
                | _ -> false)
                
        let addDriver (vm: VarMap) (name: IdentifierT) (driver: Driver) =
            if vm.ContainsKey name
            then
                match vm.[name].var with
                | Wire currDrivers ->
                    currDrivers
                    |> List.compResMap (fun d ->
                        if Range.overlap d.drivenRange driver.drivenRange
                        then Errors.ProcessContAssign.multiDrivenRanges name d.drivenRange driver.drivenRange
                        else Succ ())
                    ?>> fun _ ->
                        let newDrivers = driver::currDrivers
                        let newComp = { vm.[name] with var = Wire newDrivers }
                        vm.Add(name, newComp)
                | _ -> Errors.ProcessContAssign.canOnlyDriveWire name
            else Errors.General.varDoesNotExist name

    type InitItem =
        { lhs: {| varName: IdentifierT; range: Range |}
          rhs: VNum }

    type AlwaysEventControl = (EventControlType * IdentifierT * Range) List

    type EventControlContent =
        { ec: AlwaysEventControl
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
          alwaysBlocks: AlwaysBlock list
          modInstNames: IdentifierT list }
        with
            override this.ToString() =
                let vm =
                    this.varMap
                    |> Map.toList
                    |> List.map (fun (key, value) -> sprintf "\t[%s, %s]\n" (key.ToString()) (value.ToString()))
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

        