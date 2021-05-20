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

    let simulateEOC netlistCollection prevState currState inputs netlist eoc =
        let newState = simulateVars netlistCollection prevState currState inputs netlist eoc.vars
        let expInputs =
            eoc.vars
            |> List.map (fun (name, _) -> (name, snd newState.varMap.[name]))
            |> Map.ofList
        let value = (Util.expToConstExpr expInputs eoc.expression |> ConstExprEval.evalConstExpr).getRange eoc.range
        (newState, value)

    let simulateEOCIntoVar netlistCollection prevState currState inputs netlist eoc varName drivenRange =
        let (newState, value) = simulateEOC netlistCollection prevState currState inputs netlist eoc
        newState.addVar netlist varName drivenRange value 

    let simulateMOCIntoVar netlistCollection prevState currState inputs netlist moc varName drivenRange =
        let state =
            if Helpers.modInState currState moc.instanceName
            then currState
            else 
                let modInst = netlist.moduleInstances.[moc.instanceName]
                let modNetlist = netlistCollection.netlists.[modInst.moduleName]
                let (modInputs, newState) =
                    (currState, modInst.drivers)
                    ||> List.mapFold (fun state (portName, range, eoc) ->
                        let (s, v) = simulateEOC netlistCollection prevState state inputs netlist eoc
                        let value = v.getRange range
                        (portName, value), s)
                    |> function
                    | inp, state -> Map.ofList inp, state
                let simVars = modNetlist.moduleDeclaration.getOutputs()
                let modState = simulateVars netlistCollection prevState.modInstMap.[moc.instanceName] SimState.empty modInputs modNetlist simVars
                { newState with modInstMap = newState.modInstMap.Add(moc.instanceName, modState) }
        let value = Helpers.getVarFromState state.modInstMap.[moc.instanceName] moc.portName moc.range
        state.addVar netlist varName drivenRange value

    let shouldEventControlTrigger netlistCollection prevState currState inputs netlist eventControl =
        // TODO: for star, check all variables
        // TODO: for not star, check each event expression
        // TODO: evaluate the previous value of the expression
        // TODO: evaluate the current value of the expression
        // TODO: posedge -> if bit 0 goes from 0 to 1 -> true
        // TODO: posedge -> if bit 0 goes from 1 to 0 -> true
        raise <| NotImplementedException()

    let simulateAlways netlistCollection prevState currState inputs netlist alwaysID varName drivenRange =
        // TODO: simulate inputs so that they are in state
        // TODO: check if the block should run (based on timing statement)
        // TODO: if so, update state by evaluating body statement
        // TODO: if not return anyway
        let alwaysBlock = netlist.alwaysBlocks.[alwaysID]
        let newState = simulateVars netlistCollection prevState currState inputs netlist alwaysBlock.inputs
        if shouldEventControlTrigger netlistCollection prevState newState inputs netlist alwaysBlock.eventControl
        then raise <| NotImplementedException() // TODO: should run
        else newState

    let simulateRC netlistCollection prevState currState inputs netlist varName varRange (rc: RegContent) =
        let f nc ps cs inp n vn dr dt =
            match dt with
            | RegExpressionOutput eoc -> simulateEOCIntoVar nc ps cs inp n eoc vn dr
            | RegModuleOutput moc -> simulateMOCIntoVar nc ps cs inp n moc vn dr
            | RegAlwaysOutput i -> simulateAlways nc ps cs inp n i vn dr
        Helpers.simulateIndiVar netlistCollection prevState currState inputs netlist varName varRange rc.drivers f

    let simulateWC netlistCollection prevState currState inputs netlist varName varRange (wc: WireContent) =
        let f nc ps cs inp n vn dr dt =
            match dt with
            | WireExpressionOutput eoc -> simulateEOCIntoVar nc ps cs inp n eoc vn dr
            | WireModuleOutput moc -> simulateMOCIntoVar nc ps cs inp n moc vn dr
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
