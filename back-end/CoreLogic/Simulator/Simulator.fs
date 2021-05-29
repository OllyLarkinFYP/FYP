namespace Simulator

open System
open CommonTypes
open CommonHelpers
open Compiler.Netlist
open AST

module private rec Internal =
    let runInitial (initial: InitItem list) (state: SimState) =
        (state, initial)
        ||> List.fold (fun s initItem -> 
            SimState.addReg s initItem.lhs.varName initItem.lhs.range initItem.rhs)

    let evalExpression (varMap: VarMap) (state: SimState) (exp: ExpContent) : SimState * VNum =
        let (state', valMap) =
            (state, exp.reqVars)
            ||> List.mapFold (fun s (iden, range) ->
                let (s', value) = evalVar varMap s iden range
                // shifted value is required as evalVar will ground the requested range, which we don't want here
                let shiftedVal = VNum.(<<<) (value, VNum range.lower)
                (iden, shiftedVal), s')
            |> function
            | valueLst, s -> s, Map.ofList valueLst
        let constExpr = Util.expToConstExpr valMap exp.expression
        state', ConstExprEval.evalConstExpr constExpr

    let evalVar (varMap: VarMap) (state: SimState) (iden: IdentifierT) (range: Range) : SimState * VNum =
        if SimState.contains state iden range
        then state, SimState.get state iden range
        else
            let drivers = varMap.[iden].var.getDriversFor range
            let state' =
                (state, drivers)
                ||> List.fold (fun s driver ->
                    let (s', expVal) = evalExpression varMap s driver.exp
                    let addVal = expVal.getRange driver.expRange
                    SimState.addWire s' varMap iden driver.drivenRange addVal)
            state', SimState.get state' iden range

    let evalEventControl (varMap: VarMap) (prevState: SimState) (currState: SimState) (eventControl: EventControlContent) : SimState * bool =
        (currState, eventControl.ec)
        ||> List.existsFold (fun state (ect, iden, range) ->
            let (_, prevVal) = evalVar varMap prevState iden range
            let (state', currVal) = evalVar varMap state iden range
            let trigger =
                match ect with
                | Neither -> prevVal <> currVal
                | Posedge -> prevVal.getRange (Single 0u) = VNum 0 && currVal.getRange (Single 0u) = VNum 1
                | Negedge -> prevVal.getRange (Single 0u) = VNum 1 && currVal.getRange (Single 0u) = VNum 0
            state', trigger)

    let simAlwaysStatement (varMap: VarMap) (state: SimState) (statement: StatementContent) : SimState =
        raise <| NotImplementedException() // TODO: this

    let simNetlist (netlist: Netlist) (prevState: SimState) (currState: SimState) =
        let (state, triggeringAlways) =
            (currState, netlist.alwaysBlocks)
            ||> List.chooseFold (fun state (_, alwaysBlock) ->
                match evalEventControl netlist.varMap prevState state alwaysBlock.eventControl with
                | state', true -> state', Some alwaysBlock
                | state', false -> state', None)
        if triggeringAlways.Length = 0
        then state
        else 
            (state, triggeringAlways)
            ||> List.fold (fun s always -> simAlwaysStatement netlist.varMap s always.statement)
            |> simNetlist netlist state


module Simulate =
    let runSimulation (netlist: Netlist) (inputs: SimInputs) (cycles: uint) =
        let allInputs =
            VarMap.inputs netlist.varMap
            |> Map.toList
            |> List.map (fun (name, variable) -> name, variable.range)
        (SimState.init netlist inputs, [0u .. cycles])
        ||> List.fold (fun prevState cycle ->
            let inp = SimInputs.getCycle allInputs cycle inputs
            let currState =
                let s = SimState.newState prevState inp
                if cycle = 0u
                then Internal.runInitial netlist.initial s
                else s
            Internal.simNetlist netlist prevState currState)
        