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
                
        let addDriver (vm: VarMap) (name: WithPos<IdentifierT>) (driver: Driver) =
            if vm.ContainsKey name.value
            then
                match vm.[name.value].var with
                | Wire currDrivers ->
                    currDrivers
                    |> List.compResMap (fun d ->
                        if Range.overlap d.drivenRange driver.drivenRange
                        then Errors.ProcessContAssign.multiDrivenRanges name d.drivenRange driver.drivenRange
                        else Succ ())
                    ?>> fun _ ->
                        let newDrivers = driver::currDrivers
                        let newComp = { vm.[name.value] with var = Wire newDrivers }
                        vm.Add(name.value, newComp)
                | _ -> Errors.ProcessContAssign.canOnlyDriveWire name
            else Errors.General.varDoesNotExist name

    type Assignment =
        { name: IdentifierT
          driver: Driver }

    type ConditionalStatement =
        { condition: ExpContent
          trueBody: Statement
          falseBody: Statement }

    and CaseItem =
        { conditions: ExpContent list
          body: Statement }

    and CaseStatement =
        { caseExpr: ExpContent
          items: CaseItem list
          defaultCase: Statement }

    and Statement =
        | Null
        | BlockingAssignment of Assignment list 
        | NonblockingAssignment of Assignment list
        | Case of CaseStatement
        | Conditional of ConditionalStatement
        | SeqBlock of Statement list 

    type AssignItem =
        { lhs: {| varName: IdentifierT; range: Range |}
          rhs: VNum }

    type AlwaysEventControl = (EventControlType * IdentifierT * Range) List

    type EventControlContent =
        { ec: AlwaysEventControl
          reqVars: ReqVarList }

    type AlwaysBlock = 
        { eventControl: EventControlContent
          statement: Statement }

    type IndexedAlwaysBlocks = (int * AlwaysBlock) list

    type Netlist =
        { varMap: VarMap
          initial: AssignItem list
          alwaysBlocks: IndexedAlwaysBlocks
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

        