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

    let modInState state modName =
        state.modInstMap.ContainsKey modName

    let getVarFromState state variable range =
        (snd state.varMap.[variable]).getRange range

    let simulateIndiVar netlistCollection prevState currState inputs netlist varName varRange drivers f =
        let folder state' (drivenRange, driverType) =
            if Range.overlap varRange drivenRange
            then f netlistCollection prevState state' inputs netlist varName drivenRange driverType
            else state'

        (currState, drivers)
        ||> List.fold folder

module private rec Internal =

    let simulateEOC netlistCollection prevState currState inputs netlist eoc varName drivenRange =
        let newState = simulateVars netlistCollection prevState currState inputs netlist eoc.vars
        let expInputs =
            eoc.vars
            |> List.map (fun (name, _) -> (name, snd newState.varMap.[name]))
            |> Map.ofList
        let value = (Util.expToConstExpr expInputs eoc.expression |> ConstExprEval.evalConstExpr).getRange eoc.range
        newState.addVar netlist varName drivenRange value 

    let simulateMOC netlistCollection prevState currState inputs netlist moc varName drivenRange =
        let state =
            if Helpers.modInState currState moc.instanceName
            then currState
            else 
                // TODO: evaluate all inputs to module
                // TODO: providing inputs to module, simulate all output vars
                // TODO: insert the returned state into currState and return it
                raise <| NotImplementedException()
        let value = Helpers.getVarFromState state.modInstMap.[moc.instanceName] moc.portName moc.range
        state.addVar netlist varName drivenRange value

    let simulateAlways netlistCollection prevState currState inputs netlist alwaysID varName drivenRange =
        raise <| NotImplementedException() // TODO: this

    let simulateRC netlistCollection prevState currState inputs netlist varName varRange (rc: RegContent) =
        let f nc ps cs inp n vn dr dt =
            match dt with
            | RegExpressionOutput eoc -> simulateEOC nc ps cs inp n eoc vn dr
            | RegModuleOutput moc -> simulateMOC nc ps cs inp n moc vn dr
            | RegAlwaysOutput i -> simulateAlways nc ps cs inp n i vn dr
        Helpers.simulateIndiVar netlistCollection prevState currState inputs netlist varName varRange rc.drivers f

    let simulateWC netlistCollection prevState currState inputs netlist varName varRange (wc: WireContent) =
        let f nc ps cs inp n vn dr dt =
            match dt with
            | WireExpressionOutput eoc -> simulateEOC nc ps cs inp n eoc vn dr
            | WireModuleOutput moc -> simulateMOC nc ps cs inp n moc vn dr
        Helpers.simulateIndiVar netlistCollection prevState currState inputs netlist varName varRange wc.drivers f

    let simulateVars netlistCollection prevState currState (inputs: Map<IdentifierT,VNum>) netlist variables =
        let folder state (varName, varRange) =
            if Helpers.varInState state varName varRange
            then state
            else
                match netlist.variables.[varName] with
                | InputComp r ->
                    // TODO: this assumes that the input exists in the provided map 
                    // TODO: should always be the case but maybe specialise exception if not? 
                    { state with varMap = state.varMap.Add(varName, ([varRange], inputs.[varName])) }
                | RegComp rc -> simulateRC netlistCollection prevState state inputs netlist varName varRange rc
                | WireComp wc -> simulateWC netlistCollection prevState state inputs netlist varName varRange wc

        (currState, variables)
        ||> List.fold folder
