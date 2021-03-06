namespace Simulator

open System
open CommonTypes
open Compiler.Netlist

type SimInput =
    | Once of VNum list
    | Repeating of VNum list
    with
        member this.length =
            match this with
            | Once vs -> vs.Length
            | Repeating vs -> vs.Length
        member this.getCycle (c: uint) =
            // specified to be uint as positive values would not make sense here
            let cInt = int c 
            if this.length = 0
            then VNum.unknown 63u
            else 
                match this with
                | Once vs ->
                    if cInt >= vs.Length
                    then vs.[vs.Length - 1]
                    else vs.[cInt]
                | Repeating vs -> vs.[cInt % vs.Length]
        static member private getValueList (inp: (VNum * int) list) =
            let rec createList (value, occ) =
                if occ <= 1
                then [ value ]
                else value::(createList (value, occ-1))
            inp
            |> List.collect createList
        static member onceWithLengths = SimInput.getValueList >> Once
        static member repeatWithLengths = SimInput.getValueList >> Repeating

type SimInputs = Map<IdentifierT, SimInput> 

module SimInputs =
    let getCycle (allInputs: (IdentifierT * Range) list) c (inp: SimInputs) = 
        let initMap = Map.map (fun _ (v: SimInput) -> v.getCycle c) inp
        (initMap, allInputs)
        ||> List.fold (fun m (inpName, inpRange) ->
            // add all inputs as unknown that aren't provided
            if m.ContainsKey inpName 
            then m
            else m.Add(inpName, VNum.unknown inpRange.size))

type SimVarType =
    | SimStateReg
    | SimStateInput
    | SimStateWire of Range list
    with
        member this.ranges =
            match this with
            | SimStateReg -> []
            | SimStateInput -> []
            | SimStateWire rl -> rl

type SimStateContent =
    { simType: SimVarType
      value: VNum }
    with
        member this.contains range =
            match this.simType with
            | SimStateWire rl -> List.exists (fun (r: Range) -> r.hasSubRange range) rl
            | _ -> false 

type SimState = Map<IdentifierT,SimStateContent>

module SimState =
    let empty : SimState = Map.empty

    let init (netlist: Netlist) =
        (empty, Map.toList netlist.varMap)
        ||> List.fold (fun state (iden, variable) ->
            match variable.var with
            | Reg -> state.Add(iden, { simType = SimStateReg; value = VNum.unknown variable.range.size })
            | Input -> state.Add(iden, { simType = SimStateReg; value = VNum.unknown variable.range.size })
            | _ -> state)

    let nextState (varMap: VarMap) (prevState: SimState) (inputs: Map<IdentifierT, VNum>) =
        let initReg = Map.filter (fun _ cont -> cont.simType = SimStateReg) prevState
        (initReg, Map.toList inputs)
        ||> List.fold (fun state (inpName, inpValue) ->
            state.Add(inpName, { simType = SimStateInput; value = inpValue.getRange varMap.[inpName].range }))

    let contains (state: SimState) iden range =
        if state.ContainsKey iden
        then
            match state.[iden].simType with
            | SimStateReg | SimStateInput -> true
            | SimStateWire rl -> List.exists (fun (r: Range) -> r.hasSubRange range) rl
        else false 

    let get (state: SimState) iden range = 
        state.[iden].value.getRange range

    let addReg (state: SimState) iden range value =
        let currVal = state.[iden].value
        let cont =
            { simType = SimStateReg
              value = currVal.setRange range value }
        state.Add(iden, cont)

    let addWire (varMap: VarMap) (state: SimState) iden range value =
        let (currVal, currRanges) =
            if state.ContainsKey iden
            then state.[iden].value, state.[iden].simType.ranges
            else VNum.unknown varMap.[iden].range.size, []
        let cont =
            { simType = SimStateWire (range::currRanges)
              value = currVal.setRange range value }
        state.Add(iden, cont)
