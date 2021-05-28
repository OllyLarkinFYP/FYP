namespace Simulator

open System
open CommonTypes
open CommonHelpers
open Compiler.Netlist
open AST

module private Internal =
    let runInitial (initial: InitItem list) (state: SimState) =
        (state, initial)
        ||> List.fold (fun s initItem -> 
            SimState.addReg s initItem.lhs.varName initItem.lhs.range initItem.rhs)

    let rec evalExpression (varMap: VarMap) (state: SimState) (exp: ExpressionT) : SimState * VNum =
        raise <| NotImplementedException()

    let evalVar (varMap: VarMap) (state: SimState) (iden: IdentifierT) (range: Range) : SimState * VNum =
        // TODO: if in state return that value
        // TODO: if not in state, find relevant drivers from var map and eval them
        // TODO: return new state and value
        raise <| NotImplementedException()

    let simAlwaysStatement (varMap: VarMap) (state: SimState) (statement: StatementContent) : SimState =
        raise <| NotImplementedException()

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

    let rec simNetlist (netlist: Netlist) (prevState: SimState) (currState: SimState) =
        let (state, triggeringAlways) =
            (currState, netlist.alwaysBlocks)
            ||> List.chooseFold (fun state alwaysBlock ->
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
        