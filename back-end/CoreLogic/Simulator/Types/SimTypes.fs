namespace SimTypes

open CommonTypes
open Netlist

type InputValue =
    | Constant of VNum
    | Once of VNum list
    | Repeating of VNum list
    with
        member this.getValue (cycle: uint) =
            match this with
            | Constant v -> v
            | Once vals -> if cycle >= uint vals.Length then vals.[vals.Length-1] else vals.[int cycle]
            | Repeating vals -> vals.[int cycle % vals.Length]

type SimState =
    { varMap: Map<IdentifierT, Range list * VNum>
      modInstMap: Map<IdentifierT,SimState> }
    with
        static member empty = { varMap = Map.empty; modInstMap = Map.empty }
        static member init =
            let rec initRec netlistCollection name =
                let netlist = Map.find name netlistCollection.netlists
                let varMap =
                    netlist.variables
                    |> Map.map (fun _ varComp ->
                        match varComp with
                        | InputComp r -> [r], VNum.unknown r.size
                        | RegComp regCont -> [regCont.range], regCont.initVal
                        | WireComp wireCont -> [wireCont.range], VNum.unknown wireCont.range.size)
                let modInstMap =
                    netlist.moduleInstances
                    |> Map.map (fun _ modInstComp -> initRec netlistCollection modInstComp.moduleName)
                { varMap = varMap; modInstMap = modInstMap }
            initRec
        member this.addVar netlist name range value =
            let ranges, currVal =
                if this.varMap.ContainsKey name
                then this.varMap.[name]
                else [], VNum.unknown netlist.variables.[name].range.size
            let newVal = currVal.setRange range value
            let newRanges = Range.mergeInto ranges range
            { this with varMap = this.varMap.Add(name, (newRanges, newVal)) }
