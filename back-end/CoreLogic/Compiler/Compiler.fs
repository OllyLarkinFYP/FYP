namespace Compiler

open System
open AST
open CommonTypes
open Compiler.CompResult
open Compiler.Netlist
open Compiler.Utils
open CommonHelpers

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

    let isUniqueModInst (modInstList: IdentifierT list) modInst =
        if List.contains modInst modInstList
        then Errors.ProcessModuleInstances.notUniqueModule modInst
        else Succ ()

    let allPortsProvided modInstName (ports: 'a list) (expLst: ExpressionT list) =
        if ports.Length = expLst.Length
        then Succ ()
        else Errors.ProcessModuleInstances.portsDoNotMatch modInstName ports.Length expLst.Length

    let rec varLValueOnlyWritesReg (varMap: VarMap) varLVal =
        match varLVal with
        | VarLValueT.Ranged rv ->
            if varMap.ContainsKey rv.name
            then
                match varMap.[rv.name].var with
                | Reg -> Succ()
                | _ -> Errors.VarLValueE.shouldBeReg rv.name
            else Errors.VarLValueE.doesNotExist rv.name
        | VarLValueT.Concat c -> List.compResMap (varLValueOnlyWritesReg varMap) c ?>> ignore


module private Helpers =
    let netLValToRangeList (varMap: VarMap) (netLVal: NetLValueT) =
        let rec toRangeListRec (offset, currLst) =
            function
            | NetLValueT.Ranged rangedNLV ->
                if varMap.ContainsKey rangedNLV.name
                then
                    match varMap.[rangedNLV.name].var with
                    | Wire _ -> 
                        let range = Util.optRangeTToRangeDefault varMap.[rangedNLV.name].range rangedNLV.range
                        let entry = (rangedNLV.name, range, range.ground().offset offset)
                        Succ (offset + range.size, entry::currLst)
                    | _ -> Errors.NetLValueE.shouldBeWire rangedNLV.name
                else Errors.NetLValueE.doesNotExist rangedNLV.name
            | NetLValueT.Concat c ->
                (List.rev c, (offset, currLst))
                ||> List.compResFold toRangeListRec
        // Returns result of list of (name, idenRange, valueRange)
        toRangeListRec (0u,[]) netLVal ?>> snd

    let varLValToRangeList (varMap: VarMap) (varLVal: VarLValueT) =
        let rec toRangeListRec (offset, currLst) =
            function
            | VarLValueT.Ranged rangedNLV ->
                if varMap.ContainsKey rangedNLV.name
                then
                    match varMap.[rangedNLV.name].var with
                    | Reg -> 
                        let range = Util.optRangeTToRangeDefault varMap.[rangedNLV.name].range rangedNLV.range
                        let entry = (rangedNLV.name, range, range.ground().offset offset)
                        Succ (offset + range.size, entry::currLst)
                    | _ -> Errors.VarLValueE.shouldBeReg rangedNLV.name
                else Errors.VarLValueE.doesNotExist rangedNLV.name
            | VarLValueT.Concat c ->
                (List.rev c, (offset, currLst))
                ||> List.compResFold toRangeListRec
        // Returns result of list of (name, idenRange, valueRange)
        toRangeListRec (0u,[]) varLVal ?>> snd

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
                | StatementT.BlockingAssignment ba -> getExprVars ba.RHS
                | StatementT.NonblockingAssignment nba -> getExprVars nba.RHS
                | StatementT.SeqBlock sb -> List.collect varsRec sb
                | StatementT.Case c -> 
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
                | StatementT.Conditional c ->
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
        let prefixExpCont (expCont: ExpContent) =
            { expression = prefixExpression expCont.expression
              reqVars = prefixReqVars expCont.reqVars }
        let rec prefixVarLVal = 
            function
            | VarLValueT.Ranged rv -> VarLValueT.Ranged { rv with name = prefix + rv.name }
            | VarLValueT.Concat c -> VarLValueT.Concat (List.map prefixVarLVal c)
        let rec prefixStatement stmt = 
            match stmt with
            | Null -> Null
            | BlockingAssignment ba ->
                ba
                |> List.map (fun assignment ->
                    { assignment with name = prefix + assignment.name })
                |> BlockingAssignment
            | NonblockingAssignment nba -> 
                nba
                |> List.map (fun assignment ->
                    { assignment with name = prefix + assignment.name })
                |> NonblockingAssignment
            | Case cs -> 
                let prefixCaseItem (ci: CaseItem) = 
                    { conditions = List.map prefixExpCont ci.conditions
                      body = prefixStatement ci.body }
                Case
                    { caseExpr = prefixExpCont cs.caseExpr
                      items = List.map prefixCaseItem cs.items
                      defaultCase = prefixStatement cs.defaultCase }
            | Conditional cs ->
                Conditional
                    { condition = prefixExpCont cs.condition
                      trueBody = prefixStatement cs.trueBody
                      falseBody = prefixStatement cs.falseBody }
            | SeqBlock sb ->
                sb
                |> List.map prefixStatement
                |> SeqBlock
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
            |> List.map (fun (i, alwaysBlock) ->
                let prefixedEventControl =
                    let prefixedEC =
                        alwaysBlock.eventControl.ec
                        |> List.map (fun (ect, iden, range) -> ect, prefix + iden, range)
                    { ec = prefixedEC
                      reqVars = prefixReqVars alwaysBlock.eventControl.reqVars }
                let prefixedStatement = prefixStatement alwaysBlock.statement 
                (i, 
                    { eventControl = prefixedEventControl
                      statement = prefixedStatement }))

        { netlist with
            varMap = prefixedVarMap
            initial = prefixedInitial
            alwaysBlocks = prefixedAlwaysBlocks}

    let rec expToNetLVal port modInst exp  =
        let primaryToNetLVal =
            function
            | PrimaryT.Ranged rv -> Succ <| NetLValueT.Ranged rv
            | Brackets b -> expToNetLVal port modInst b
            | PrimaryT.Concat c -> 
                c
                |> List.compResMap (expToNetLVal port modInst)
                ?>> NetLValueT.Concat
            | _ -> Errors.ProcessModuleInstances.cannotDriveExpression port modInst
        match exp with
        | Primary p -> primaryToNetLVal p
        | _ -> Errors.ProcessModuleInstances.cannotDriveExpression port modInst

    let orderPorts (order: IdentifierT list) (ports: (IdentifierT * 'a) list) =
        order
        |> List.map (fun name -> List.find (fun (n,_) -> name = n) ports)

    let getPortsAndItems fPort fPost astInfo =
        match astInfo with
        | ModDec1 md1 ->
            let (ports, items) =
                (([], []), md1.body)
                ||> List.fold (fun (ports, items) modItem ->
                    match modItem with
                    | NonPortModuleItem item -> (ports, item::items)
                    | PortDeclaration pd -> ((fPort pd) @ ports, items))
            Validate.portsMatchDecs md1.ports ports
            ?> fun _ -> Validate.uniquePorts ports
            ?>> fun _ -> 
                let processedPorts =
                    ports
                    |> orderPorts md1.ports
                    |> fPost
                (processedPorts, items)
        | ModDec2 md2 ->
            let ports = List.collect fPort md2.ports
            Validate.uniquePorts ports
            ?>> fun _ -> (fPost ports, md2.body)

    let registerNetAssigns varMap (netAssigns: NetAssignmentT list) =
        (netAssigns, varMap)
        ||> List.compResFold (fun vMap netAssign ->
            let expVars = getExprVars netAssign.RHS
            Validate.varsExist vMap expVars
            ?> fun _ -> netLValToRangeList varMap netAssign.LHS
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

    let rec statementTToStatement (varMap: VarMap) (statementOrNull: StatementOrNullT) : CompRes<Statement> =
        match statementOrNull with
        | None -> Succ Null
        | Some s ->
            let rec toStatementRec statement =
                match statement with
                | StatementT.BlockingAssignment ba ->
                    let expCont =
                        { expression = ba.RHS
                          reqVars = getExprVars ba.RHS }
                    varLValToRangeList varMap ba.LHS
                    ?>> List.map (fun (name, drivenRange, expRange) ->
                        { name = name
                          driver =
                            { drivenRange = drivenRange
                              expRange = expRange
                              exp = expCont } })
                    ?>> Statement.BlockingAssignment

                | StatementT.NonblockingAssignment nba -> 
                    let expCont =
                        { expression = nba.RHS
                          reqVars = getExprVars nba.RHS }
                    varLValToRangeList varMap nba.LHS
                    ?>> List.map (fun (name, drivenRange, expRange) ->
                        { name = name
                          driver =
                            { drivenRange = drivenRange
                              expRange = expRange
                              exp = expCont } })
                    ?>> Statement.NonblockingAssignment

                | StatementT.SeqBlock sb ->
                    sb
                    |> List.compResMap toStatementRec
                    ?>> Statement.SeqBlock

                | StatementT.Case cs -> 
                    let caseExpr =
                        { expression = cs.CaseExpr
                          reqVars = getExprVars cs.CaseExpr }
                    cs.Items
                    |> List.tryFind (fun item ->
                        match item with
                        | Default _ -> true
                        | _ -> false)
                    |> function
                    | Some (Default s) -> statementTToStatement varMap s
                    | _ -> Succ Null
                    ?> fun defaultCase ->
                        cs.Items
                        |> List.compResChoose (fun item ->
                            match item with
                            | Default _ -> None
                            | Item i -> 
                                statementTToStatement varMap i.Body
                                ?>> fun body ->
                                    let conditions =
                                        i.Elems
                                        |> List.map (fun exp ->
                                            { expression = exp
                                              reqVars = getExprVars exp })
                                    { conditions = conditions
                                      body = body }
                                |> Some)
                        ?>> fun items ->
                            Statement.Case   
                                { caseExpr = caseExpr
                                  items = items
                                  defaultCase = defaultCase }
                     
                | StatementT.Conditional cs -> 
                    let rec processElseIfs (elseIfs: {| Condition: ExpressionT; Body: StatementOrNullT |} List) finalElse =
                        match elseIfs with
                        | [] -> statementTToStatement varMap finalElse
                        | hd::tl ->
                            let cond =
                                { expression = hd.Condition
                                  reqVars = getExprVars hd.Condition }
                            statementTToStatement varMap hd.Body
                            ?> fun trueBody ->
                                processElseIfs tl finalElse
                                ?>> fun falseBody ->
                                    Statement.Conditional
                                        { condition = cond
                                          trueBody = trueBody
                                          falseBody = falseBody }
                    statementTToStatement varMap cs.Body
                    ?> fun trueBody ->
                        processElseIfs cs.ElseIf cs.ElseBody
                        ?>> fun falseBody ->
                            Statement.Conditional
                                { condition =
                                    { expression = cs.Condition
                                      reqVars = getExprVars cs.Condition }
                                  trueBody = trueBody
                                  falseBody = falseBody }

            toStatementRec s


module private Internal =

    let processInputOutput astInfo : CompRes<VarMap * NonPortModuleItemT List> =
        let processPD (pd: PortDeclarationT) =
            let range = Util.optRangeTToRange pd.range
            match pd.dir with
            | PortDirAndType.Input -> 
                pd.names
                |> List.map (fun name ->
                    (name, { var = Input; range = range }))
            | PortDirAndType.Output PortType.Wire -> 
                pd.names
                |> List.map (fun name ->
                    (name, { var = Wire []; range = range }))
            | PortDirAndType.Output PortType.Reg -> 
                pd.names
                |> List.map (fun name ->
                    (name, { var = Reg; range = range }))
        Helpers.getPortsAndItems processPD Map.ofList astInfo

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
            Helpers.registerNetAssigns netlist.varMap ca
            ?>> fun newVarMap -> { netlist with varMap = newVarMap }
        | _ -> Succ netlist

    let processAlwaysBlocks (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | AlwaysConstruct ac ->
            let statementVars = Helpers.getStatementVars ac.Statement
            let eventControlVars = Helpers.getEventControlVars statementVars ac.Control
            Validate.varsExist netlist.varMap eventControlVars
            ?> fun _ ->
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
                Helpers.statementTToStatement netlist.varMap ac.Statement
                ?>> (fun stmt -> 
                    { eventControl =
                        { ec = eventList
                          reqVars = eventControlVars }
                      statement = stmt})
                ?>> fun alwaysBlock ->
                    { netlist with alwaysBlocks = (netlist.alwaysBlocks.Length, alwaysBlock)::netlist.alwaysBlocks }
        | _ -> Succ netlist

    let rec processModuleInstances (asts: ASTT list) (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | ModuleInstantiation mi ->
            let moduleInstName = mi.Module.Name
            let prefix = moduleInstName + ":"
            Validate.isUniqueModInst netlist.modInstNames moduleInstName
            ?> fun _ ->
                asts
                |> List.tryFind (fun a -> a.name = mi.Name)
                |> function
                | None -> Errors.ProcessModuleInstances.moduleDoesNotExist mi.Name
                | Some modAST ->
                    compileModule modAST asts
                    ?>> Helpers.applyPrefix prefix
                    ?> fun subNetlist ->
                        let newInitial = netlist.initial @ subNetlist.initial
                        let newAlwaysBlocks = 
                            netlist.alwaysBlocks @ subNetlist.alwaysBlocks
                            |> List.indexed
                            |> List.map (fun (i, (_, a)) -> (i, a))
                        let processPD (pd: PortDeclarationT) =
                            pd.names
                            |> List.map (fun name -> name, pd.dir)
                        Helpers.getPortsAndItems processPD id modAST.info
                        ?> fun (ports, _) ->
                            let prefixedPorts = ports |> List.map (fun (iden, pt) -> prefix + iden, pt)
                            match mi.Module.PortConnections with
                            | Unnamed expLst ->
                                Validate.allPortsProvided moduleInstName prefixedPorts expLst
                                ?>> fun _ -> List.zip prefixedPorts expLst
                            | Named namedLst ->
                                namedLst
                                |> List.compResMap (fun namedPort ->
                                    let exp =
                                        match namedPort.Value with
                                        | Some e -> e
                                        | None -> Primary (Number (VNum.unknown 63u))
                                    prefixedPorts
                                    |> List.tryFind (fun (iden, _) -> iden = prefix + namedPort.Name)
                                    |> function
                                    | None -> Errors.ProcessModuleInstances.namedPortsDoNotMatch moduleInstName namedPort.Name
                                    | Some port -> Succ (port, exp))
                            ?> fun portExpLst ->
                                let inputs =
                                    portExpLst
                                    |> List.choose (fun ((pName, pType), exp) ->
                                        match pType with
                                        | PortDirAndType.Input -> Some (pName, exp)
                                        | _ -> None)
                                portExpLst
                                |> List.choose (fun ((pName, pType), exp) ->
                                    match pType with
                                    | PortDirAndType.Output _ -> Some (pName, exp)
                                    | _ -> None)
                                |> List.compResMap (fun (pName, exp) ->
                                    Helpers.expToNetLVal pName moduleInstName exp
                                    ?>> fun netVal ->
                                        let outExp = Primary (PrimaryT.Ranged { name = pName; range = None })
                                        { NetAssignmentT.LHS = netVal; RHS = outExp })
                                ?> fun outputNetAssigns ->
                                    let collectedVarMap = Map.fold (fun acc key value -> Map.add key value acc) subNetlist.varMap netlist.varMap
                                    let collectedVarMap' =
                                        (collectedVarMap, inputs)
                                        ||> List.fold (fun vMap (iden, exp) ->
                                            let expCont =
                                                { expression = exp
                                                  reqVars = Helpers.getExprVars exp }
                                            let varElem = Wire [{ drivenRange = Range.max; expRange = Range.max; exp = expCont }]
                                            let variable = { vMap.[iden] with var = varElem }
                                            vMap.Add(iden, variable))
                                    Helpers.registerNetAssigns collectedVarMap' outputNetAssigns
                        ?>> fun newVarMap ->
                            { varMap = newVarMap
                              initial = newInitial
                              alwaysBlocks = newAlwaysBlocks
                              modInstNames = moduleInstName::netlist.modInstNames }
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
            ?> List.compResFold (processModuleInstances asts) items

module Compile =
    let project modName (asts: ASTT list) : CompRes<Netlist> =
        asts
        |> List.tryFind (fun ast -> ast.name = modName)
        |> function
        | Some thisAst -> Internal.compileModule thisAst asts
        | None ->  Errors.CompilerAPI.astNotProvided modName