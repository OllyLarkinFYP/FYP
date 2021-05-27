namespace Compiler

open System
open AST
open CommonTypes
open Compiler.CompResult
open Compiler.Netlist
open Compiler.Utils
open CommonHelpers

module private Helpers =
    let netLValToRangeList (netlist: Netlist) (netLVal: NetLValueT) =
        let rec toRangeListRec (offset, currLst) =
            function
            | NetLValueT.Ranged rangedNLV ->
                if netlist.varMap.ContainsKey rangedNLV.name
                then
                    match netlist.varMap.[rangedNLV.name].var with
                    | Wire _ -> 
                        let range = Util.optRangeTToRangeDefault netlist.varMap.[rangedNLV.name].range rangedNLV.range
                        let entry = (rangedNLV.name, range, range.ground().offset offset)
                        Succ (offset + range.size, entry::currLst)
                    | _ -> Errors.NetLValueE.shouldBeWire rangedNLV.name
                else Errors.NetLValueE.doesNotExist rangedNLV.name
            | NetLValueT.Concat c ->
                (List.rev c, (offset, currLst))
                ||> List.compResFold toRangeListRec
        // Returns result of list of (name, idenRange, valueRange)
        toRangeListRec (0u,[]) netLVal ?>> snd

    let squashIdenRangeList (idenRangeLst: (IdentifierT * Range) list) =
        idenRangeLst
        |> List.map (fun (iden, range) ->
            let newRange = 
                (range, idenRangeLst)
                ||> List.fold (fun currR (iden2, range2) ->
                    if iden = iden2
                    then
                        match Range.merge currR range2 with
                        | Some r -> r
                        | None -> currR
                    else currR)
            (iden, newRange))
        |> List.distinct

    let getExprVars exp =
        let rec getExprVarsRec exp =
            let rec getPrimaryVars primary =
                match primary with
                | PrimaryT.Ranged r -> [r.name, Util.optRangeTToRangeDefault Range.max r.range]
                | PrimaryT.Concat c -> List.collect getExprVarsRec c
                | PrimaryT.Brackets b -> getExprVarsRec b
                | _ -> []
            match exp with
            | Primary p -> getPrimaryVars p
            | UniExpression u -> getExprVarsRec u.Expression
            | BinaryExpression b -> getExprVarsRec b.LHS @ getExprVarsRec b.RHS
            | CondExpression c -> getExprVarsRec c.Condition @ getExprVarsRec c.TrueVal @ getExprVarsRec c.FalseVal
        getExprVarsRec exp |> squashIdenRangeList

    let getEventControlVars (statementVars: (IdentifierT * Range) list) (ec: EventControlT) =
        match ec with
        | Star -> statementVars
        | EventList el ->
            el
            |> List.map (fun (_, rangedVar) -> 
                let range = Util.optRangeTToRangeDefault Range.max rangedVar.range
                (rangedVar.name, range))
        |> squashIdenRangeList

    let getStatementVars (statementOrNull: StatementOrNullT) =
        match statementOrNull with
        | None -> []
        | Some statement ->
            let rec varsRec statement =
                let optStatementVars =
                    function
                    | None -> []
                    | Some s -> varsRec s
                match statement with
                | BlockingAssignment ba -> getExprVars ba.RHS
                | NonblockingAssignment nba -> getExprVars nba.RHS
                | SeqBlock sb -> List.collect varsRec sb
                | Case c -> 
                    let caseExprVars = getExprVars c.CaseExpr
                    let itemVars =
                        c.Items
                        |> List.collect
                            (function
                            | Default s -> optStatementVars s
                            | Item i -> 
                                let expVars = List.collect getExprVars i.Elems
                                let bodyVars = optStatementVars i.Body
                                expVars @ bodyVars)
                    caseExprVars @ itemVars
                | Conditional c ->
                    let condVars = getExprVars c.Condition
                    let bodyVars = optStatementVars c.Body
                    let elseBodyVars = optStatementVars c.ElseBody
                    let elseIfVars =
                        c.ElseIf
                        |> List.collect (fun elseIf ->
                            let condVars' = getExprVars elseIf.Condition
                            let bodyVars' = optStatementVars elseIf.Body
                            condVars' @ bodyVars')
                    condVars @ bodyVars @ elseBodyVars @ elseIfVars
            varsRec statement
        |> squashIdenRangeList

    let applyPrefix (prefix: IdentifierT) (netlist: Netlist) =
        let rec prefixExpression exp =
            match exp with
            | Primary p ->
                match p with
                | PrimaryT.Ranged r -> Primary (PrimaryT.Ranged { r with name = prefix + r.name })
                | Brackets b -> Primary (Brackets (prefixExpression b))
                | PrimaryT.Concat c -> Primary (PrimaryT.Concat (List.map prefixExpression c))
                | _ -> Primary p
            | UniExpression ue -> UniExpression {| ue with Expression = prefixExpression ue.Expression |}
            | BinaryExpression be ->
                BinaryExpression
                    {| be with
                        LHS = prefixExpression be.LHS
                        RHS = prefixExpression be.RHS |}
            | CondExpression ce ->
                CondExpression
                    {| Condition = prefixExpression ce.Condition
                       TrueVal = prefixExpression ce.TrueVal
                       FalseVal = prefixExpression ce.FalseVal |}
        let prefixReqVars = List.map (fun (iden, range) -> prefix + iden, range)
        let rec prefixNetLVal = 
            function
            | NetLValueT.Ranged rv -> NetLValueT.Ranged { rv with name = prefix + rv.name }
            | NetLValueT.Concat c -> NetLValueT.Concat (List.map prefixNetLVal c)
        let rec prefixStatement stmt = 
            match stmt with
            | None -> stmt
            | Some statement ->
                let rec prefixNonOptStmt s =
                    match s with
                    | BlockingAssignment ba ->
                        BlockingAssignment 
                            { LHS = prefixNetLVal ba.LHS 
                              RHS = prefixExpression ba.RHS }
                    | Case cs ->
                        let prefixCaseItem =
                            function
                            | Item i ->
                                let prefixedElems = List.map prefixExpression i.Elems
                                let prefixedBody = prefixStatement i.Body
                                Item
                                    {| Elems = prefixedElems
                                       Body = prefixedBody |}
                            | Default s -> Default <| prefixStatement s
                        Case
                            { CaseExpr = prefixExpression cs.CaseExpr
                              Items = List.map prefixCaseItem cs.Items }
                    | Conditional cs ->
                        let prefixElseIf (elseIf: {| Condition: ExpressionT; Body: StatementOrNullT |}) =
                            {| Condition = prefixExpression elseIf.Condition
                               Body = prefixStatement elseIf.Body |}
                        Conditional
                            { Condition = prefixExpression cs.Condition
                              Body = prefixStatement cs.Body
                              ElseIf = List.map prefixElseIf cs.ElseIf
                              ElseBody = prefixStatement cs.ElseBody }
                    | NonblockingAssignment nba -> 
                        NonblockingAssignment 
                            { LHS = prefixNetLVal nba.LHS
                              RHS = prefixExpression nba.RHS }
                    | SeqBlock sb -> SeqBlock (List.map prefixNonOptStmt sb)
                Some <| prefixNonOptStmt statement
        let prefixedVarMap =
            netlist.varMap
            |> Map.toList
            |> List.map (fun (key, variable) -> 
                let prefixedKey = prefix + key
                match variable.var with
                | Wire drivers ->
                    let prefixedDrivers =   
                        drivers
                        |> List.map (fun driver ->
                            let prefixedExp =
                                { expression = prefixExpression driver.exp.expression
                                  reqVars = prefixReqVars driver.exp.reqVars }
                            { driver with exp = prefixedExp })
                    (prefixedKey, { variable with var = Wire prefixedDrivers })
                | _ -> (prefixedKey, variable))
            |> Map.ofList
        let prefixedInitial =
            netlist.initial
            |> List.map (fun initItem ->
                { initItem with
                    lhs = {| initItem.lhs with 
                                varName = prefix + initItem.lhs.varName |}})
        let prefixedAlwaysBlocks = 
            netlist.alwaysBlocks
            |> List.map (fun alwaysBlock ->
                let prefixedEventControl =
                    let prefixedEC =
                        alwaysBlock.eventControl.ec
                        |> List.map (fun (ect, iden, range) -> ect, prefix + iden, range)
                    { ec = prefixedEC
                      reqVars = prefixReqVars alwaysBlock.eventControl.reqVars }
                let prefixedStatement =
                    { s = prefixStatement alwaysBlock.statement.s
                      reqVars = prefixReqVars alwaysBlock.statement.reqVars }
                { eventControl = prefixedEventControl
                  statement = prefixedStatement })

        { netlist with
            varMap = prefixedVarMap
            initial = prefixedInitial
            alwaysBlocks = prefixedAlwaysBlocks}


module private Validate =
    let portsMatchDecs ports portDecs =
        let portNames = List.map fst portDecs
        if List.sort ports <> List.sort portNames
        then Errors.ProcessPorts.portsDontMatchPortDecs ports portNames
        else Succ ()

    let uniqueIdentifier (varMap: VarMap) modInstNames name =
        // TODO: make sure name is not a keyword
        match varMap.ContainsKey name, List.contains name modInstNames with
        | true, _ -> Errors.UniqueNames.duplicateVarDefinition name
        | _, true -> Errors.UniqueNames.duplicateModInstDefinition name
        | _ -> Succ ()

    let uniquePorts ports =
        ports
        |> List.countBy fst
        |> List.compResMap (fun (name, num) ->
            if num > 1
            then Errors.ProcessPorts.duplicatePorts name
            else Succ ())
        ?>> ignore

    let isReg (varMap: VarMap) name =
        if varMap.ContainsKey name
        then
            match varMap.[name].var with
            | VarElem.Reg -> Succ ()
            | _ -> Errors.ProcessInitial.shouldBeReg name
        else Errors.ProcessInitial.regDoesNotExist name

    let varExists (varMap: VarMap) (var: IdentifierT) =
        if varMap.ContainsKey var
        then Succ ()
        else Errors.General.varDoesNotExist var

    let varsExist (varMap: VarMap) (vars: (IdentifierT * Range) list) =
        // TODO: check var range
        vars
        |> List.compResMap (fun (name, _) -> varExists varMap name)
        ?>> ignore

    let isUniqueModIsnt (modInstList: IdentifierT list) modInst =
        if List.contains modInst modInstList
        then Errors.ProcessModuleInstances.notUniqueModule modInst
        else Succ ()


module private Internal =

    let processInputOutput ast : CompRes<VarMap * NonPortModuleItemT List> =
        let processPD (pd: PortDeclarationT) =
            let range = Util.optRangeTToRange pd.range
            match pd.dir with
            | PortDirAndType.Input -> (pd.name, { var = Input; range = range })
            | PortDirAndType.Output PortType.Wire -> (pd.name, { var = Wire []; range = range })
            | PortDirAndType.Output PortType.Reg -> (pd.name, { var = Reg; range = range })
        match ast with
        | ModDec1 md1 ->
            let (ports, items) =
                (([], []), md1.body)
                ||> List.fold (fun (ports, items) modItem ->
                    match modItem with
                    | NonPortModuleItem item -> (ports, item::items)
                    | PortDeclaration pd -> ((processPD pd)::ports, items))
            Validate.portsMatchDecs md1.ports ports
            ?> fun _ -> Validate.uniquePorts ports
            ?>> fun _ -> (Map.ofList ports, items)
        | ModDec2 md2 ->
            let ports = List.map processPD md2.ports
            Validate.uniquePorts ports
            ?> fun _ -> 
                let vm = Map.ofList ports
                Succ (vm, md2.body)

    let processModuleVariable (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | ModuleItemDeclaration mid ->
            (mid.names, netlist.varMap)
            ||> List.compResFold (fun vMap name ->
                Validate.uniqueIdentifier vMap [] name
                ?>> fun _ ->
                    let newEntry = 
                        let range = Util.optRangeTToRange mid.range
                        match mid.decType with
                        | PortType.Wire -> { var = Wire []; range = range }
                        | PortType.Reg -> { var = Reg; range = range }
                    vMap.Add(name, newEntry))
            ?>> fun varMap -> { netlist with varMap = varMap }
        | _ -> Succ netlist

    let processInitialBlock (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | InitialConstruct ic ->
            ic
            |> List.compResMap (fun rca ->
                let rhs = ConstExprEval.evalConstExpr rca.RHS
                Validate.isReg netlist.varMap rca.LHS.name
                ?>> fun _ ->
                    // TODO: warning if range is not subset of var range
                    let lhs = 
                        {| varName = rca.LHS.name
                           range = Util.optRangeTToRangeDefault Range.max rca.LHS.range |}
                    { lhs = lhs
                      rhs = rhs })
            ?>> fun initialBlock ->
                { netlist with initial = initialBlock }
        | _ -> Succ netlist

    let processContinuousAssignments (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | ContinuousAssign ca ->
            (ca, netlist.varMap)
            ||> List.compResFold (fun vMap netAssign ->
                let expVars = Helpers.getExprVars netAssign.RHS
                Validate.varsExist vMap expVars
                ?> fun _ -> Helpers.netLValToRangeList netlist netAssign.LHS
                ?> fun varLst ->
                    (varLst, vMap)
                    ||> List.compResFold (fun vMap' (name, drivenRange, expRange) ->
                        let driver =
                            { drivenRange = drivenRange
                              expRange = expRange
                              exp = 
                                { expression = netAssign.RHS
                                  reqVars = expVars }}
                        VarMap.addDriver vMap' name driver))
            ?>> fun newVarMap -> { netlist with varMap = newVarMap }
        | _ -> Succ netlist

    let processAlwaysBlocks (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | AlwaysConstruct ac ->
            let statementVars = Helpers.getStatementVars ac.Statement
            let eventControlVars = Helpers.getEventControlVars statementVars ac.Control
            Validate.varsExist netlist.varMap statementVars
            ?> fun _ -> Validate.varsExist netlist.varMap eventControlVars
            ?>> fun _ ->
                let alwaysBlock = 
                    let eventList = 
                        match ac.Control with
                        | EventList el -> 
                            el
                            |> List.map (fun (ect, rangedVar) ->
                                let range = Util.optRangeTToRangeDefault Range.max rangedVar.range
                                ect, rangedVar.name, range)
                        | Star ->
                            eventControlVars
                            |> List.map (fun (var, range) -> Neither, var, range)
                    { eventControl =
                        { ec = eventList
                          reqVars = eventControlVars }
                      statement =
                        { s = ac.Statement
                          reqVars = statementVars }}
                { netlist with alwaysBlocks = alwaysBlock::netlist.alwaysBlocks }
        | _ -> Succ netlist

    let rec processModuleInstances (ast: ASTT) (asts: ASTT list) (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        // TODO: collect module instances, compile them with their netlist and current prefix + instance name prefix, then add and connect with master netlist
        match item with
        | ModuleInstantiation mi ->
            let moduleInstName = mi.Module.Name
            Validate.isUniqueModIsnt netlist.modInstNames moduleInstName
            ?> fun _ ->
                // TODO: compile module -> get netlist
                // TODO: apply instance name prefix to all vars in netlist
                // TODO: merge netlist into current netlist, connecting input and output ports
                // TODO:    - sub module netlist inputs should be converted to wires so they can be driven by the input exprs
                // TODO: update current netlist and add instance name to instance list
                asts
                |> List.tryFind (fun a -> a.name = mi.Name)
                |> function
                | None -> Errors.ProcessModuleInstances.moduleDoesNotExist mi.Name
                | Some modAST ->
                    let subNetlist = 
                        compileModule modAST asts
                        ?>> Helpers.applyPrefix moduleInstName
                    raise <| NotImplementedException()
        | _ -> Succ netlist

    and compileModule (ast: ASTT) (asts: ASTT list) : CompRes<Netlist> =
        processInputOutput ast.info
        ?> fun (initialVarMap, items) ->
            { varMap = initialVarMap
              initial = []
              alwaysBlocks = []
              modInstNames = [] }
            |> List.compResFold processModuleVariable items
            ?> List.compResFold processInitialBlock items
            ?> List.compResFold processContinuousAssignments items
            ?> List.compResFold processAlwaysBlocks items
            ?> List.compResFold (processModuleInstances ast asts) items

module Compile =
    let project modName (asts: ASTT list) : CompRes<Netlist> =
        asts
        |> List.tryFind (fun ast -> ast.name = modName)
        |> function
        | Some thisAst -> Internal.compileModule thisAst asts
        | None ->  Errors.CompilerAPI.astNotProvided modName