namespace Simulator

open System
open CommonTypes
open CommonHelpers
open Compiler.Netlist
open AST

module private rec Internal =
    let expToConstExpr (varMap: VarMap) (inputMap: Map<IdentifierT, VNum>) exp =
        let rec toConstExprRec exp =
            let toConstPrimary = 
                function
                | PrimaryT.Number v -> ConstantPrimaryT.Number v
                | PrimaryT.Ranged r ->
                    let range = Util.optRangeTToRangeDefault varMap.[r.name].range r.range
                    let value = inputMap.[r.name].getRange range
                    ConstantPrimaryT.Number value
                | PrimaryT.Concat c -> ConstantPrimaryT.Concat <| List.map toConstExprRec c
                | PrimaryT.Brackets b -> ConstantPrimaryT.Brackets <| toConstExprRec b
            match exp with
            | Primary p -> ConstantExpressionT.Primary <| toConstPrimary p
            | UniExpression u ->
                ConstantExpressionT.UniExpression
                    {| Operator = u.Operator
                       Expression = toConstExprRec u.Expression |}
            | BinaryExpression b -> 
                ConstantExpressionT.BinaryExpression
                    {| LHS = toConstExprRec b.LHS
                       BinOperator = b.BinOperator
                       RHS = toConstExprRec b.RHS |}
            | CondExpression c ->
                ConstantExpressionT.CondExpression
                    {| Condition = toConstExprRec c.Condition
                       TrueVal = toConstExprRec c.TrueVal
                       FalseVal = toConstExprRec c.FalseVal |}
        toConstExprRec exp

    let runInitial (initial: AssignItem list) (state: SimState) =
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
        let constExpr = expToConstExpr varMap valMap exp.expression
        state', ConstExprEval.evalConstExpr constExpr

    let evalVar (varMap: VarMap) (state: SimState) (iden: IdentifierT) (range: Range) : SimState * VNum =
        if SimState.contains state iden range
        then 
            state, SimState.get state iden range
        else
            let drivers = varMap.[iden].var.getDriversFor range
            let state' =
                (state, drivers)
                ||> List.fold (fun s driver ->
                    let (s', expVal) = evalExpression varMap s driver.exp
                    let addVal = expVal.getRange driver.expRange
                    SimState.addWire varMap s' iden driver.drivenRange addVal)
            state', SimState.get state' iden range

    let evalEventControl (varMap: VarMap) (prevState: SimState) (currState: SimState) (eventControl: EventControlContent) : SimState * bool =
        (currState, eventControl.ec)
        ||> List.existsFold (fun state (ect, iden, range) ->
            let (_, prevVal) = evalVar varMap prevState iden range
            let (state', currVal) = evalVar varMap state iden range
            let trigger =
                // TODO: conditions on posedge and negedge may have to change for x values - investigate
                match ect with
                | Neither -> prevVal <> currVal
                | Posedge -> prevVal.getRange (Single 0u) = VNum 0 && currVal.getRange (Single 0u) = VNum 1
                | Negedge -> prevVal.getRange (Single 0u) = VNum 1 && currVal.getRange (Single 0u) = VNum 0
            state', trigger)

    let simAlwaysStatement (varMap: VarMap) (state: SimState) (statement: StatementContent) : SimState * AssignItem list =
        raise <| NotImplementedException() // TODO: this

    let triggeringAlwaysBlocks (netlist: Netlist) (prevState: SimState) (currState: SimState) (blackList: int list) : IndexedAlwaysBlocks =
        (currState, netlist.alwaysBlocks)
        ||> List.chooseFold (fun state (i,alwaysBlock) ->
            match evalEventControl netlist.varMap prevState state alwaysBlock.eventControl with
            | state', true when not (List.contains i blackList) -> state', Some (i, alwaysBlock)
            | state', _ -> state', None)
        |> snd  // do not want the wire vals in the state outside of this evaluation chain

    let runAlwaysBlocks (netlist: Netlist) (currState: SimState) (blocks: IndexedAlwaysBlocks) : SimState * AssignItem list * IndexedAlwaysBlocks =
        let blockIDs = List.map fst blocks
        ((currState, [], []), blocks)
        ||> List.fold (fun (state, nonBlockAssigns, triggering) (_, block) ->
            let (state', nba) = simAlwaysStatement netlist.varMap state block.statement
            let trig = triggeringAlwaysBlocks netlist state state' blockIDs
            state', nba @ nonBlockAssigns, trig @ triggering)
        |> function
        | state, nba, triggering -> state, nba, List.distinctBy fst triggering

    let runNBA (state: SimState) (nba: AssignItem list) : SimState =
        (state, nba)
        ||> List.fold (fun state' nbAssign ->
            SimState.addReg state' nbAssign.lhs.varName nbAssign.lhs.range nbAssign.rhs)

    let simNetlist (netlist: Netlist) (prevState: SimState) (currState: SimState) : SimState =
        let rec runInternal (currState: SimState) (nba: AssignItem list) (triggered: IndexedAlwaysBlocks) =
            if triggered.Length = 0
            then currState, nba
            else
                runAlwaysBlocks netlist currState triggered
                |||> runInternal
                |> function
                | state, nba' -> state, nba' @ nba
        
        let rec run prevState currState =
            let (state, nba) =
                triggeringAlwaysBlocks netlist prevState currState []
                |> runInternal currState []
            if nba.Length = 0
            then state
            else run state (runNBA state nba)

        run prevState currState

    let getReqVars (varMap: VarMap) (reqVars: IdentifierT list) (states: SimState list) =
        states
        |> List.map (fun state ->
            (state, reqVars)
            ||> List.fold (fun state' reqVar ->
                evalVar varMap state' reqVar Range.max |> fst)
            |> Map.filter (fun iden _ -> List.contains iden reqVars)
            |> Map.map (fun _ ssc -> ssc.value))

module Simulate =
    let runSimulation (netlist: Netlist) (inputs: SimInputs) (reqVars: IdentifierT list) (cycles: uint) =
        let allInputs =
            VarMap.inputs netlist.varMap
            |> Map.toList
            |> List.map (fun (name, variable) -> name, variable.range)
        reqVars
        |> List.map (fun reqVar ->
            if netlist.varMap.ContainsKey reqVar
            then reqVar
            else raise <| ArgumentException(sprintf "The requested variable %A was not present in the netlist." reqVar))
        |> ignore
        (([], SimState.init netlist inputs), [0u .. cycles-1u])
        ||> List.fold (fun (prevStateLst, prevState) cycle ->
            let inp = SimInputs.getCycle allInputs cycle inputs
            let newState =
                let s = SimState.nextState netlist.varMap prevState inp
                if cycle = 0u
                then Internal.runInitial netlist.initial s
                else s
                |> Internal.simNetlist netlist prevState 
            (newState::prevStateLst), newState)
        |> fst
        |> Internal.getReqVars netlist.varMap reqVars
        