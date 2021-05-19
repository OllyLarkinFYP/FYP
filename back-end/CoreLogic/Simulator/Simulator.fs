module Simulator

open System
open CommonTypes
open CommonHelpers
open SimTypes
open Netlist
open System.Collections.Generic

module private Helpers =
    let getCycle cycle = Map.map (fun _ (value: InputValue) -> value.getValue cycle)

    let varInState state variable range =
        state.varMap.ContainsKey variable && 
            List.exists (fun (r: Range) -> r.hasSubRange range) (fst state.varMap.[variable])

    let getVarFromState state variable range =
        (snd state.varMap.[variable]).getRange range

module private rec Internal =

    let simulateEOC netlistCollection prevState currState inputs netlist eoc varName varRange driverRange =
        let newState = simulateVars netlistCollection prevState currState inputs netlist eoc.vars
        let expInputs =
            eoc.vars
            |> List.map (fun (name, _) -> (name, snd newState.varMap.[name]))
            |> Map.ofList
        let value = (Util.expToConstExpr expInputs eoc.expression |> ConstExprEval.evalConstExpr).getRange driverRange
        newState.addVar varName netlist.variables.[varName].range varRange value 

    let simulateMOC netlistCollection prevState currState inputs netlist moc varName varRange driverRange =
        // TODO: evaluate all input ranges (crossover ranges are not allowed to be inputs and outputs so no loops)
        // TODO: using the above inputs, evaluate the outputs recursively using simulateVars on the netlist
        raise <| NotImplementedException() //TODO: this

    let simulateVars netlistCollection prevState currState (inputs: Map<IdentifierT,VNum>) netlist variables =
        let simRC state varName varRange (rc: RegContent) =
            let folder state (driverRange, driverType) =
                match driverType with
                | RegExpressionOutput eoc -> simulateEOC netlistCollection prevState currState inputs netlist eoc varName varRange driverRange
                | RegModuleOutput moc -> simulateMOC netlistCollection prevState currState inputs netlist moc varName varRange driverRange
                | RegAlwaysOutput i -> raise <| NotImplementedException() //TODO: this

            (state, rc.drivers)
            ||> List.fold folder

        let simWC state varName varRange (wc: WireContent) =
            let folder state (driverRange, driverType) =
                match driverType with
                | WireExpressionOutput eoc -> simulateEOC netlistCollection prevState currState inputs netlist eoc varName varRange driverRange
                | WireModuleOutput moc -> simulateMOC netlistCollection prevState currState inputs netlist moc varName varRange driverRange

            (state, wc.drivers)
            ||> List.fold folder

        let folder state (varName, varRange) =
            if Helpers.varInState state varName varRange
            then state
            else
                match netlist.variables.[varName] with
                | InputComp r ->
                    // TODO: this assumes that the input exists in the provided map 
                    // TODO: should always be the case but maybe specialise exception if not? 
                    { state with varMap = state.varMap.Add(varName, ([varRange], inputs.[varName])) }
                | RegComp rc -> simRC state varName varRange rc
                | WireComp wc -> simWC state varName varRange wc

        (currState, variables)
        ||> List.fold folder
