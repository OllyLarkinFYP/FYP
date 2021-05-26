namespace Compiler

open System
open AST
open CommonTypes
open Compiler.CompResult
open Compiler.Netlist
open Compiler.Utils
open CommonHelpers

// module private Helpers =
//     let safeMapAdd (map: Map<'a,'b>) (key: 'a, value: 'b) =
//         if map.ContainsKey key
//         then Error <| sprintf "%A is already a registered component. It cannot be declared again." key
//         else Ok <| map.Add(key,value)

//     let netLValToRangeList (netlist: Netlist) (netLVal: NetLValueT) =
//         let rec toRangeListRec (offset, currLst) =
//             function
//             | NetLValueT.Ranged rangedNLV ->
//                 if netlist.variables.ContainsKey rangedNLV.name
//                 then
//                     let range = Util.optRangeTToRangeDefault netlist.variables.[rangedNLV.name].range rangedNLV.range
//                     let entry = (rangedNLV.name, range, range.offset offset)
//                     Ok (offset + range.size, entry::currLst)
//                 else Error <| sprintf "Cannot find the component %A." rangedNLV.name
//             | NetLValueT.Concat c ->
//                 ((offset, currLst), List.rev c)
//                 ||> ResList.fold toRangeListRec
//         // Returns result of list of (name, idenRange, valueRange)
//         toRangeListRec (0u,[]) netLVal ?>> snd

//     let rec expToNetLVal (exp: ExpressionT) =
//         let error = Error "Expressions can not be driven. Only a reg/wire/concatenation can be driven."
//         let primaryToNetLVal prim =
//             match prim with
//             | PrimaryT.Number _ -> error
//             | PrimaryT.Ranged r -> Ok <| NetLValueT.Ranged r
//             | PrimaryT.Brackets b -> expToNetLVal b
//             | PrimaryT.Concat c -> ResList.map expToNetLVal c ?>> NetLValueT.Concat
//         match exp with
//         | Primary p -> primaryToNetLVal p
//         | _ -> error

//     let squashIdenRangeList (idenRangeLst: (IdentifierT * Range) list) =
//         idenRangeLst
//         |> List.map (fun (iden, range) ->
//             let newRange = 
//                 (range, idenRangeLst)
//                 ||> List.fold (fun currR (iden2, range2) ->
//                     if iden = iden2
//                     then
//                         match Range.merge currR range2 with
//                         | Some r -> r
//                         | None -> currR
//                     else currR)
//             (iden, newRange))
//         |> List.distinct

//     let getExprVars exp =
//         // let rec getExprVarsRec exp =
//         //     let rec getPrimaryVars primary =
//         //         match primary with
//         //         | PrimaryT.Ranged r -> [r.name, Option.bind (Util.rangeTToRange >> Some) r.range]
//         //         | PrimaryT.Concat c -> List.collect getExprVarsRec c
//         //         | PrimaryT.Brackets b -> getExprVarsRec b
//         //         | _ -> []
//         //     match exp with
//         //     | Primary p -> getPrimaryVars p
//         //     | UniExpression u -> getExprVarsRec u.Expression
//         //     | BinaryExpression b -> getExprVarsRec b.LHS @ getExprVarsRec b.RHS
//         //     | CondExpression c -> getExprVarsRec c.Condition @ getExprVarsRec c.TrueVal @ getExprVarsRec c.FalseVal
//         // getExprVarsRec exp |> squashIdenRangeList
//         raise <| NotImplementedException() // TODO: this

//     let getEventControlVars eventControl =
//         // match eventControl with
//         // | Star -> []
//         // | EventList eventExps ->
//         //     eventExps
//         //     |> List.collect (EventExpressionT.unwrap >> getExprVars)
//         //     |> squashIdenRangeList
//         raise <| NotImplementedException() // TODO: this

//     let getStatementOrNullVars sn =
//         let rec getStatementOrNullVarsRec sn =
//             match sn with
//             | None -> []
//             | Some statement -> 
//                 match statement with
//                 | BlockingAssignment a -> getExprVars a.RHS
//                 | Case c ->
//                     getExprVars c.CaseExpr @
//                         (c.Items
//                         |> List.collect
//                             (function 
//                             | Default s -> getStatementOrNullVarsRec s
//                             | Item i -> getStatementOrNullVarsRec i.Body @ (List.collect getExprVars i.Elems)))
//                 | Conditional c ->
//                     let cond = getExprVars c.Condition
//                     let body = getStatementOrNullVarsRec c.Body
//                     let elseBody = getStatementOrNullVarsRec c.ElseBody
//                     let elseIf =
//                         c.ElseIf
//                         |> List.collect (fun ei -> getExprVars ei.Condition @ getStatementOrNullVarsRec ei.Body)
//                     cond @ body @ elseIf @ elseBody
//                 | NonblockingAssignment a -> getExprVars a.RHS
//                 | SeqBlock s -> List.collect (Some >> getStatementOrNullVarsRec) s
//         getStatementOrNullVarsRec sn |> squashIdenRangeList

//     let getStatementOrNullOutputs sn =
//         let rec getStatementOrNullOutputsRec sn =
//             match sn with
//             | None -> []
//             | Some statement ->
//                 match statement with
//                 | BlockingAssignment a -> [a.LHS]
//                 | Case c ->
//                     c.Items
//                     |> List.collect
//                         (function
//                         | Default s -> getStatementOrNullOutputsRec s
//                         | Item i -> getStatementOrNullOutputsRec i.Body)
//                 | Conditional c ->
//                     let trueBody = getStatementOrNullOutputsRec c.Body
//                     let falseBody = getStatementOrNullOutputsRec c.ElseBody
//                     let elseIf = c.ElseIf |> List.collect (fun ei -> getStatementOrNullOutputsRec ei.Body)
//                     trueBody @ falseBody @ elseIf
//                 | NonblockingAssignment a -> [a.LHS]
//                 | SeqBlock s -> List.collect (Some >> getStatementOrNullOutputsRec) s
//         getStatementOrNullOutputsRec sn |> List.distinct

//     let getTimingControlVars tc =
//         getEventControlVars tc.Control @ getStatementOrNullVars tc.Statement |> squashIdenRangeList

//     let validateDrivingVars netlist idenLst =
//         idenLst
//         |> ResList.map (fun (iden,_) ->
//             if netlist.variables.ContainsKey iden
//             then Ok()
//             else Error <| sprintf "The component %A was used but this is not an input/reg/wire." iden)
//         ?>> fun _ -> idenLst

//     let getExprOutCont netlist exp range =
//         getExprVars exp
//         |> validateDrivingVars netlist
//         ?>> fun drivingVars ->
//             { expression = exp
//               vars = drivingVars
//               range = range }

//     let validateNamedModulePorts modDec (namedPorts: {| Name: IdentifierT; Value: ExpressionT option |} list) =
//         namedPorts
//         |> ResList.map (fun namedPort ->
//             modDec.ports
//             |> List.tryFind (fun (name,_,_) -> name = namedPort.Name)
//             |> function
//             | Some _ -> Ok()
//             | None -> Error <| sprintf "The module %A was instantiated with incorrect ports. %A is not a port of the module." modDec.name namedPort.Name) 
//         |> ResList.ignore

//     let validateNewDriver name (currDrivers: (Range * 'a) list) (newRange: Range, d: 'a) =
//         currDrivers
//         |> ResList.map (fun (range, _) ->
//             if Range.overlap range newRange
//             then Error <| sprintf "Cannot drive the range %s of %s, as the range %s of %s is already being driven." (newRange.ToString()) name (range.ToString()) name
//             else Ok())
//         ?>> fun _ -> (newRange, d)

//     let getTopLevels (netlists: Netlist list) =
//         let containsNetlist netlist containingNet =
//             containingNet.moduleInstances
//             |> Map.toList
//             |> List.exists (fun (_, modInst) -> modInst.moduleName = netlist.moduleDeclaration.name)
//         netlists
//         |> List.choose (fun netlist -> 
//             netlists
//             |> List.exists (containsNetlist netlist)
//             |> function
//             | true -> None
//             | false -> Some netlist.moduleDeclaration.name)
//         |> function
//         | [] -> Error "Cannot find a top level module. This implies that all modules are instantiated by another module. This cannot be simulated please provide a top level module."
//         | top -> Ok top


// module private Internal =
//     let processInputOutput md =
//         md.ports
//         |> List.map (fun (name, pType, range) ->
//             match pType with
//             | Input -> (name, InputComp range)
//             | Output Wire -> (name, WireComp { range = range; drivers = [] })
//             | Output Reg -> (name, RegComp { range = range; initVal = VNum.unknown range.size; drivers = [] }))
//         |> Map.ofList

//     let processVariables netlist =
//         function
//         | ModuleItemDeclaration mid ->
//             let newEntries =
//                 mid.names
//                 |> List.map (fun name ->
//                     let range = Util.optRangeTToRange mid.range
//                     let node =
//                         match mid.decType with
//                         | Wire -> WireComp { range = range; drivers = [] }
//                         | Reg -> RegComp { range = range; initVal = VNum.unknown range.size; drivers = [] }
//                     (name, node))
//             (netlist.variables, newEntries)
//             ||> ResList.fold Helpers.safeMapAdd
//             ?>> fun newVars -> { netlist with variables = newVars }
//         | _ -> Ok netlist

//     let processInitialBlock netlist =
//         // function
//         // | InitialConstruct ic ->
//         //     ic
//         //     |> ResList.map (fun assignment ->
//         //         let rhsValue = ConstExprEval.evalConstExpr assignment.RHS
//         //         assignment.LHS
//         //         |> Helpers.netLValToRangeList netlist 
//         //         ?> ResList.map (fun (name, idenRange, valueRange) ->
//         //             match netlist.variables.[name] with
//         //             | RegComp rc -> 
//         //                 rc.initVal <- rc.initVal.setRange idenRange (rhsValue.getRange valueRange)
//         //                 Ok()
//         //             | _ -> Error <| sprintf "Can only assign initial values to 'reg' types. %A is not a 'reg'." name))
//         //     ?>> fun _ -> netlist
//         // | _ -> Ok netlist
//         raise <| NotImplementedException()

//     let processContinuousAssigns netlist =
//         function
//         | ContinuousAssign ca ->
//             ca
//             |> ResList.map (fun netAssign ->
//                 netAssign.LHS
//                 |> Helpers.netLValToRangeList netlist
//                 ?> ResList.map (fun (name, idenRange, valueRange) ->
//                     Helpers.getExprOutCont netlist netAssign.RHS valueRange
//                     ?> fun exprOutCont ->
//                         match netlist.variables.[name] with
//                         | RegComp rc -> 
//                             RegExpressionOutput exprOutCont
//                             |> Util.tuple idenRange 
//                             |> Helpers.validateNewDriver name rc.drivers
//                             ?>> fun driver ->
//                                 rc.drivers <- driver::rc.drivers
//                         | WireComp wc -> 
//                             WireExpressionOutput exprOutCont
//                             |> Util.tuple idenRange 
//                             |> Helpers.validateNewDriver name wc.drivers
//                             ?>> fun driver ->
//                                 wc.drivers <- driver::wc.drivers
//                         | _ -> Error <| sprintf "Cannot drive %A as it is not a reg/wire. Only reg/wires can be driven." name))
//                 ?>> fun _ -> netlist
//         | _ -> Ok netlist

//     let processModuleInstances mds netlist =
//         function
//         | ModuleInstantiation mi ->
//             mds
//             |> List.tryFind (fun modDec -> modDec.name = mi.Name)
//             |> function
//             | None -> Error <| sprintf "The module %A could not be found. Make sure this module is a part of the project." mi.Name
//             | Some modDec ->
//                 match mi.Module.PortConnections with
//                 | Unnamed expLst -> 
//                     if expLst.Length <> modDec.ports.Length
//                     then Error <| sprintf "This module %A was instantiated with an incorrect number of ports. The module has %A ports but %A were provided." mi.Name modDec.ports.Length expLst.Length
//                     else Ok (List.zip modDec.ports expLst)
//                 | Named namedPorts ->
//                     Helpers.validateNamedModulePorts modDec namedPorts
//                     ?>> fun _ ->
//                         modDec.ports
//                         |> List.choose (fun (pName, pType, pRange) ->
//                             let port =
//                                 namedPorts
//                                 |> List.find (fun namedPort -> namedPort.Name = pName)
//                             match port.Value with
//                             | None -> None
//                             | Some exp -> Some ((pName, pType, pRange), exp))
//                 ?> ResList.choose (fun ((pName, pType, pRange), exp) ->
//                     match pType with
//                     | Input ->
//                         Helpers.getExprOutCont netlist exp pRange
//                         ?> fun vars -> Ok <| Some (pName, pRange, vars)
//                     | Output _ ->
//                         exp
//                         |> Helpers.expToNetLVal
//                         ?> Helpers.netLValToRangeList netlist
//                         ?> ResList.map (fun (name, idenRange, valueRange) ->
//                             let modOutCont =
//                                 { instanceName = mi.Module.Name
//                                   portName = pName
//                                   range = valueRange }
//                             match netlist.variables.[name] with
//                             | RegComp rc -> 
//                                 RegModuleOutput modOutCont
//                                 |> Util.tuple idenRange 
//                                 |> Helpers.validateNewDriver name rc.drivers
//                                 ?>> fun driver ->
//                                     rc.drivers <- driver::rc.drivers
//                             | WireComp wc -> 
//                                 WireModuleOutput modOutCont
//                                 |> Util.tuple idenRange 
//                                 |> Helpers.validateNewDriver name wc.drivers
//                                 ?>> fun driver ->
//                                     wc.drivers <- driver::wc.drivers
//                             | _ -> Error <| sprintf "Cannot drive %A as it is not a reg/wire. Only reg/wires can be driven." name)
//                         ?>> fun _ -> None)
//                 ?> fun driverLst ->
//                     let newEntry =
//                         mi.Module.Name,
//                             { moduleName = mi.Name
//                               drivers = driverLst }
//                     (netlist.moduleInstances, newEntry)
//                     ||> Helpers.safeMapAdd
//                     ?>> fun newModInstMap -> { netlist with moduleInstances = newModInstMap }
//         | _ -> Ok netlist

//     let processAlwaysBlocks =
//         let getID =
//             let mutable num = 0u
//             fun () ->
//                 num <- num + 1u
//                 num
//         fun netlist ->
//             function
//             | AlwaysConstruct ac -> 
//                 Helpers.getEventControlVars ac.Control @ Helpers.getStatementOrNullVars ac.Statement 
//                 |> List.distinct
//                 |> Helpers.validateDrivingVars netlist
//                 ?> fun vars ->
//                     let ID = getID()
//                     Helpers.getStatementOrNullOutputs ac.Statement
//                     |> ResList.collect (Helpers.netLValToRangeList netlist)
//                     ?> ResList.map (fun (name, idenRange, _) ->
//                         match netlist.variables.[name] with
//                         | RegComp rc -> 
//                             RegAlwaysOutput ID
//                             |> Util.tuple idenRange 
//                             |> Helpers.validateNewDriver name rc.drivers
//                             ?>> fun driver ->
//                                 rc.drivers <- driver::rc.drivers
//                         | _ -> Error <| sprintf "Cannot drive %A from an always block as it is not a 'reg'. Only 'reg' components can be driven." name)
//                     ?> fun _ ->
//                         let newEntry = (ID, { eventControl = ac.Control; statement = ac.Statement; inputs = vars })
//                         (netlist.alwaysBlocks, newEntry)
//                         ||> Helpers.safeMapAdd
//                         ?>> fun newMap -> { netlist with alwaysBlocks = newMap }
//             | _ -> Ok netlist

// let collectDecs (asts: ASTT list) : ModuleDeclaration list =
//     let orderList order lst =
//         order
//         |> List.map (fun name ->
//             lst
//             |> List.tryFind (fun (pname, _, _) -> name = pname)
//             |> function
//             // TODO: None here implies that the port list and port declarations don't match up
//             | None -> raise <| NotImplementedException() 
//             | Some p -> p)
//     let processPortDec (pd: PortDeclarationT) = (pd.name, pd.dir, Util.optRangeTToRange pd.range)
//     let processModDec1 (dec: {| ports: IdentifierT List; body: ModuleItemT List |}) =
//         dec.body
//         |> List.choose 
//             (function
//             | PortDeclaration pd -> Some <| processPortDec pd
//             | _ -> None)
//         |> orderList dec.ports
//     let processModDec2 (dec: {| ports: PortDeclarationT List; body: NonPortModuleItemT List |}) =
//         List.map processPortDec dec.ports
//     let getDec ast : ModuleDeclaration =
//         let ports = 
//             match ast.info with
//             | ModDec1 dec -> processModDec1 dec
//             | ModDec2 dec -> processModDec2 dec
//         { ModuleDeclaration.name = ast.name; ports = ports}
//     List.map getDec asts

// let compileAST (modDecs: ModuleDeclaration list) (ast: ASTT) =
//     modDecs
//     |> List.tryFind (fun md -> md.name = ast.name)
//     |> function
//     | None -> 
//         // This should be thrown if the provided module declarations do not include the ast to be compiled
//         raise <| ArgumentException()
//     | Some md ->
//         let init =
//             { moduleDeclaration = md
//               variables = Internal.processInputOutput md
//               moduleInstances = Map.empty
//               alwaysBlocks = Map.empty }
//         let items =
//             ast.info
//             |> function
//             | ModDec1 elems ->
//                 elems.body
//                 |> List.choose 
//                     (function
//                     | PortDeclaration _ -> None
//                     | NonPortModuleItem elem -> Some elem)
//             | ModDec2 elems -> elems.body

//         (init, items)
//         |> ResList.tupleFold Internal.processVariables
//         ?> ResList.tupleFold Internal.processInitialBlock
//         ?> ResList.tupleFold Internal.processContinuousAssigns
//         ?> ResList.tupleFold (Internal.processModuleInstances modDecs)
//         ?> ResList.tupleFold Internal.processAlwaysBlocks
//         ?>> fst

// let compileProject (asts: ASTT list) =
//     asts
//     |> ResList.map (compileAST <| collectDecs asts)
//     ?> fun netlists ->
//         netlists
//         |> Helpers.getTopLevels
//         ?>> fun topLevels ->
//             let netMap =
//                 netlists
//                 |> List.map (fun net -> (net.moduleDeclaration.name, net))
//                 |> Map.ofList
//             { netlists = netMap; topLevelMods = topLevels }

module private Helpers =
    let netLValToRangeList (netlist: Netlist) (netLVal: NetLValueT) =
        let rec toRangeListRec (offset, currLst) =
            function
            | NetLValueT.Ranged rangedNLV ->
                if netlist.varMap.ContainsKey rangedNLV.name
                then
                    match netlist.varMap.[rangedNLV.name] with
                    | Wire _, _ -> 
                        let range = Util.optRangeTToRangeDefault (snd netlist.varMap.[rangedNLV.name]) rangedNLV.range
                        let entry = (rangedNLV.name, range, range.offset offset)
                        Succ (offset + range.size, entry::currLst)
                    | _ -> Errors.NetLValueE.shouldBeWire rangedNLV.name
                else Errors.NetLValueE.doesNotExist rangedNLV.name
            | NetLValueT.Concat c ->
                (List.rev c, (offset, currLst))
                ||> List.compRetFold toRangeListRec
        // Returns result of list of (name, idenRange, valueRange)
        toRangeListRec (0u,[]) netLVal ?>> snd

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
        |> List.compRetMap (fun (name, num) ->
            if num > 1
            then Errors.ProcessPorts.duplicatePorts name
            else Succ ())
        ?>> ignore

    let isReg (varMap: VarMap) name =
        if varMap.ContainsKey name
        then
            match varMap.[name] with
            | (VarElem.Reg, _) -> Succ ()
            | _ -> Errors.ProcessInitial.shouldBeReg name
        else Errors.ProcessInitial.regDoesNotExist name


module private rec Internal =

    let processInputOutput ast : CompRes<VarMap * NonPortModuleItemT List> =
        let processPD (pd: PortDeclarationT) =
            let range = Util.optRangeTToRange pd.range
            match pd.dir with
            | PortDirAndType.Input -> (pd.name, (Input, range))
            | PortDirAndType.Output PortType.Wire -> (pd.name, (Wire [], range))
            | PortDirAndType.Output PortType.Reg -> (pd.name, (Reg, range))
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

    let processModuleVariable (ast: ASTT) (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | ModuleItemDeclaration mid ->
            (mid.names, netlist.varMap)
            ||> List.compRetFold (fun vMap name ->
                Validate.uniqueIdentifier vMap [] name
                ?>> fun _ ->
                    let newEntry = 
                        let range = Util.optRangeTToRange mid.range
                        match mid.decType with
                        | PortType.Wire -> (VarElem.Wire [], range)
                        | PortType.Reg -> (VarElem.Reg, range)
                    vMap.Add(name, newEntry))
            ?>> fun varMap -> { netlist with varMap = varMap }
        | _ -> Succ netlist

    let processInitialBlock (ast: ASTT) (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        match item with
        | InitialConstruct ic ->
            ic
            |> List.compRetMap (fun rca ->
                let rhs = ConstExprEval.evalConstExpr rca.RHS
                Validate.isReg netlist.varMap rca.LHS.name
                ?>> fun _ ->
                    // TODO: warning if range is not subset of var range
                    let lhs = (rca.LHS.name, Util.optRangeTToRangeDefault Range.max rca.LHS.range)
                    { lhs = lhs
                      rhs = rhs })
            ?>> fun initialBlock ->
                { netlist with initial = initialBlock }
        | _ -> Succ netlist

    let processContinuousAssignments (ast: ASTT) (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        // TODO: collect continuous assignments and register in vars
        match item with
        | ContinuousAssign ca -> Succ netlist
        | _ -> Succ netlist

    let processAlwaysBlocks (ast: ASTT) (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        // TODO: collect always blocks and register in alwaysBlocks
        match item with
        | AlwaysConstruct ac -> Succ netlist
        | _ -> Succ netlist

    let processModuleInstances (ast: ASTT) (asts: ASTT list) (netlist: Netlist) (item: NonPortModuleItemT) : CompRes<Netlist> =
        // TODO: collect module instances, compile them with their netlist and current prefix + instance name prefix, then add and connect with master netlist
        match item with
        | ModuleInstantiation mi -> Succ netlist
        | _ -> Succ netlist

    let compileModule (ast: ASTT) (asts: ASTT list) (prefix: string) : CompRes<Netlist> =
        processInputOutput ast.info
        ?> fun (initialVarMap, items) ->
            { varMap = initialVarMap
              initial = []
              alwaysBlocks = []
              modInstNames = [] }
            |> List.compRetFold (processModuleVariable ast) items
            ?> List.compRetFold (processInitialBlock ast) items
            ?> List.compRetFold (processContinuousAssignments ast) items
            ?> List.compRetFold (processAlwaysBlocks ast) items
            ?> List.compRetFold (processModuleInstances ast asts) items

module Compile =
    let project modName (asts: ASTT list) : CompRes<Netlist> =
        asts
        |> List.tryFind (fun ast -> ast.name = modName)
        |> function
        | Some thisAst -> Internal.compileModule thisAst asts ""
        | None ->  Errors.CompilerAPI.astNotProvided modName