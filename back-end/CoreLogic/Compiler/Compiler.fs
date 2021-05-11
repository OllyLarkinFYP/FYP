module Compiler

open System
open AST
open CommonTypes
open Netlist
open CommonHelpers
open CommonHelpers.Operators

module private Internal =
    let getExprInputs (exp: ExpressionT) =
        let rec getExprInputsRec exp =
            match exp with
            | Primary p ->
                match p with
                | Number _ -> []
                | PrimaryT.Ranged r -> [ r.Name ]
                | PrimaryT.Concat c -> List.collect getExprInputsRec c
                | Brackets b -> getExprInputsRec b
            | UniExpression u -> getExprInputsRec u.Expression
            | BinaryExpression b -> (getExprInputsRec b.LHS) @ (getExprInputsRec b.RHS)
            | CondExpression c -> (getExprInputsRec c.Condition) @ (getExprInputsRec c.TrueVal) @ (getExprInputsRec c.FalseVal)
        let names = 
            exp
            |> getExprInputsRec 
            |> List.distinct
        let map = 
            names
            |> List.indexed
            |> List.map (fun (a,b) -> (b, uint a))
            |> Map.ofList
        (map, names)

    let getRangedList nodeMap netLVal = // [ (myName, myRange, theirRange) ]
        let rec getRangedListRec offset =
            function
            | NetLValueT.Ranged r ->
                Util.optRangeTToRangeWithNodes nodeMap r.Name r.Range
                ?> fun range ->
                    Ok ([ (r.Name, range, range.ground().offset offset) ], (offset + range.size))
            | NetLValueT.Concat c ->
                (c, Ok ([], offset))
                ||> List.foldBack (fun netLVal state ->
                    state
                    ?> fun (lst, off) -> 
                        getRangedListRec off netLVal
                        ?> fun (curr, currOff) -> Ok (curr @ lst, currOff))
        netLVal
        |> getRangedListRec 0u
        ?> fun (lst, _) -> Ok lst

    let rec validateExpr (nodeMap: MutMap<IdentifierT,Node>) =
        let validatePrimary out =
            function
            | PrimaryT.Ranged r -> 
                let error = Error <| sprintf "%A is not a declared wire/reg/input and therefore cannot be used in an expression." r.Name
                if nodeMap.ContainsKey r.Name 
                then
                    match nodeMap.[r.Name].comp with
                    | InputComp _ | RegComp _ | WireComp _ -> Ok(out)
                    | _ -> error
                else error
            | Brackets b -> validateExpr nodeMap b
            | PrimaryT.Concat c -> ResList.map (validateExpr nodeMap) c ?> fun _ -> Ok(out)
            | _ -> Ok(out)
        function
        | Primary p as out -> validatePrimary out p
        | UniExpression u -> validateExpr nodeMap u.Expression
        | BinaryExpression b as out ->
            let exp1Val = validateExpr nodeMap b.LHS
            let exp2Val = validateExpr nodeMap b.RHS
            match exp1Val, exp2Val with
            | Error e, _ | _, Error e -> Error e
            | _ -> Ok (out)
        | CondExpression c as out -> 
            let exp1Val = validateExpr nodeMap c.Condition
            let exp2Val = validateExpr nodeMap c.TrueVal
            let exp3Val = validateExpr nodeMap c.FalseVal
            match exp1Val, exp2Val, exp3Val with
            | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
            | _ -> Ok (out)

    let validateDriven (nodeMap: MutMap<IdentifierT,Node>) name =
        let error = Error <| sprintf "%A cannot be driven as it is not an output/wire/reg. Only output/wire/reg can be driven." name
        name
        |> nodeMap.TryFind
        |> function
        | None -> error
        | Some node ->
            match node.comp with
            | OutputReg _ | OutputWire _ | WireComp _ | RegComp _ -> Ok()
            | _ -> error

    let validateDriver (nodeMap: MutMap<IdentifierT,Node>) name =
        let error = Error <| sprintf "%A cannot drive as it is not an input/wire/reg. Only input/wire/reg can drive." name
        name
        |> nodeMap.TryFind
        |> function
        | None -> error
        | Some node ->
            match node.comp with
            | InputComp _ | WireComp _ | RegComp _ -> Ok()
            | _ -> error

    let rec validateDrivenNetLVal (nodeMap: MutMap<IdentifierT,Node>) =
        function
        | Concat c as out -> ResList.map (validateDrivenNetLVal nodeMap) c ?> fun _ -> Ok(out)
        | NetLValueT.Ranged r as out ->
            let error = Error <| sprintf "%A cannot be driven as it is not an output/wire/reg. Only output/wire/reg can be driven." r.Name
            r.Name
            |> validateDriven nodeMap
            ?> fun _ -> Ok(out) 

    let rec expToNetLVal nodeMap exp =
        match exp with
        | Primary (PrimaryT.Ranged a) -> Ok <| NetLValueT.Ranged a
        | Primary (PrimaryT.Concat c) -> ResList.map (expToNetLVal nodeMap) c ?> (NetLValueT.Concat >> Ok)
        | _ -> Error "Cannot convert to NetLValue."
        ?> validateDrivenNetLVal nodeMap

    let validateModuleConnections (nodeMap: MutMap<IdentifierT,Node>) (modDec: ModuleDeclaration) (conns: PortConnectionT) =
        let rec checkOptExpIsPrim =
            function
            | None -> true
            | Some a ->
                match a with
                | Primary (Number _) -> true
                | Primary (PrimaryT.Ranged _)
                | Primary (PrimaryT.Concat _) -> 
                    expToNetLVal nodeMap a
                    |> function
                    | Ok _ -> true
                    | Error _ -> false
                | _ -> false
        match conns with
        | Unnamed expLst ->
            // If unnamed, there should be the same number of ports in declaration and instance
            if expLst.Length <> modDec.ports.Length
            then Error <| sprintf "The module %A has %i ports, while the created instance is providing %i." modDec.name modDec.ports.Length expLst.Length
            else 
                (modDec.ports, expLst)
                ||> List.zip
                |> ResList.map (fun ((pName,pType,_), exp) ->
                    match pType with
                    | Output _ ->
                        if checkOptExpIsPrim (Some exp)
                        then Ok()
                        else Error <| sprintf "Output ports can only drive output/reg/wires. An expression was assigned to port %A for module %A." pName modDec.name
                    | _ -> Ok())
                ?> fun _ -> Ok(conns)
        | Named namedLst ->
            namedLst
            |> ResList.map (fun n ->
                match List.tryFind (fun (name, _, _) -> name = n.Name) modDec.ports with
                | None -> Error <| sprintf "There is no port in the module %A called %A." modDec.name n.Name
                | Some (pName, pType, _) ->
                    match pType with
                    | Output _ ->
                        if checkOptExpIsPrim n.Value
                        then Ok()
                        else Error <| sprintf "Output ports can only drive output/reg/wires. An expression was assigned to port %A for module %A." pName modDec.name
                    | _ -> Ok())
            ?> fun _ ->
                // Check for duplicate name entries
                namedLst
                |> Util.duplicates (fun a b -> a.Name = b.Name)
                |> function
                | [] -> Ok(conns)
                | lst -> Error <| sprintf "The ports %A were declared multiple times." lst

    let getModulePortMaps (modDec: ModuleDeclaration) =
        let inputs =
            modDec.ports
            |> List.filter (fun (_,pType,_) ->
                match pType with
                | Input -> true
                | _ -> false)
            |> List.indexed
            |> List.map (fun (i, (name,_,_)) -> (name, uint i))
            |> Map.ofList
        let outputs =
            modDec.ports
            |> List.filter (fun (_,pType,_) ->
                match pType with
                | Output _ -> true
                | _ -> false)
            |> List.indexed
            |> List.map (fun (i, (name,_,_)) -> (name, uint i))
            |> Map.ofList
        (inputs, outputs)

    let getStatementInputs statementOpt =
        let rec getStatementInputsRec =
            function
            | None -> []
            | Some statement ->
                match statement with
                | BlockingAssignment a -> getExprInputs a.RHS |> snd
                | Case a -> 
                    let caseExprInp = getExprInputs a.CaseExpr |> snd
                    let caseBodyInp =
                        a.Items
                        |> List.collect
                            (function
                            | Item i ->
                                let elemInp =
                                    i.Elems
                                    |> List.collect (getExprInputs >> snd)
                                let bodyInp = getStatementInputsRec i.Body
                                elemInp @ bodyInp
                            | Default s -> getStatementInputsRec s)
                    caseExprInp @ caseBodyInp
                | Conditional a ->
                    let condInp = getExprInputs a.Condition |> snd
                    let bodyInp = getStatementInputsRec a.Body
                    let elseInp = getStatementInputsRec a.ElseBody
                    let elseIfInp =
                        a.ElseIf
                        |> List.collect (fun elem -> (getExprInputs elem.Condition |> snd) @ (getStatementInputsRec elem.Body))
                    condInp @ bodyInp @ elseInp @ elseIfInp
                | NonblockingAssignment a -> getExprInputs a.RHS |> snd
                | SeqBlock a ->
                    a
                    |> List.map Some
                    |> List.collect getStatementInputsRec
        let names = 
            statementOpt
            |> getStatementInputsRec
            |> List.distinct
        let map = 
            names
            |> List.indexed
            |> List.map (fun (a,b) -> (b, uint a + 1u)) // plus 1 is to account for the timing control port
            |> Map.ofList
        (map, names)

    let getStatementOutputs statementOpt =
        let rec getStatementOutputsRec =
            function
            | None -> []
            | Some statement ->
                match statement with
                | BlockingAssignment a -> a.LHS.getNames()
                | Case a ->
                    a.Items
                    |> List.collect
                        (function
                        | Item i -> getStatementOutputsRec i.Body
                        | Default d -> getStatementOutputsRec d)
                | Conditional a ->
                    let bodyOut = getStatementOutputsRec a.Body
                    let elseOut = getStatementOutputsRec a.ElseBody
                    let elseIfOut =
                        a.ElseIf
                        |> List.collect (fun elem -> getStatementOutputsRec elem.Body)
                    bodyOut @ elseOut @ elseIfOut
                | NonblockingAssignment a -> a.LHS.getNames()
                | SeqBlock a -> List.collect getStatementOutputsRec (List.map Some a)
        let names = 
            statementOpt
            |> getStatementOutputsRec
            |> List.distinct
        let map = 
            names
            |> List.indexed
            |> List.map (fun (a,b) -> (b, uint a))
            |> Map.ofList
        (map, names)


let collectDecs (asts: ASTT list) : ModuleDeclaration list =
    let orderList order lst =
        order
        |> List.map (fun name ->
            lst
            |> List.tryFind (fun (pname, _, _) -> name = pname)
            |> function
            // TODO: None here implies that the port list and port declarations don't match up
            | None -> raise <| NotImplementedException() 
            | Some p -> p)
    let processPortDec (pd: PortDeclarationT) = (pd.name, pd.dir, Util.optRangeTToRange pd.range)
    let processModDec1 (dec: {| ports: IdentifierT List; body: ModuleItemT List |}) =
        dec.body
        |> List.choose 
            (function
            | PortDeclaration pd -> Some <| processPortDec pd
            | _ -> None)
        |> orderList dec.ports
    let processModDec2 (dec: {| ports: PortDeclarationT List; body: NonPortModuleItemT List |}) =
        dec.ports
        |> List.map processPortDec
    let getDec ast : ModuleDeclaration =
        let ports = 
            match ast.info with
            | ModDec1 dec -> processModDec1 dec
            | ModDec2 dec -> processModDec2 dec
        { ModuleDeclaration.name = ast.name; ports = ports}
    List.map getDec asts

let compileAST (modDecs: ModuleDeclaration list) (ast: ASTT) : Result<Netlist,string> =
    let thisModDecRes =
        modDecs
        |> List.tryFind (fun dec -> dec.name = ast.name)
        |> function
        | Some d -> Ok d
        | None -> Error <| sprintf "The module declaration for the module %A was not provided to the compiler." ast.name
    let items = 
        ast.info
        |> function
        | ModDec1 elems -> 
            elems.body
            |> List.choose
                (function
                | PortDeclaration _ -> None
                | NonPortModuleItem elem -> Some elem)
        | ModDec2 elems -> elems.body

    thisModDecRes 
    ?> fun thisModDec ->
        let mutable nodeMap : MutMap<IdentifierT,Node> =
            thisModDec.ports
            |> List.map (fun (portName, portDir, portRange) ->
                let initFunc = 
                    match portDir with
                    | Input -> Node.initInputComp
                    | Output t ->
                        match t with
                        | Wire -> Node.initOutputReg
                        | Reg -> Node.initOutputWire
                (portName, initFunc portRange))
            |> Map.ofList
            |> MutMap

        let getName =
            // TODO: Make "__GENERATED_COMP__<number>" not available as wire/reg name
            let nameBase = "__GENERATED_COMP__"
            let mutable nameNum = 0
            fun () ->
                nameNum <- nameNum + 1
                nameBase + string nameNum

        let integrateComp compFun (elemToStore: 'a) exp =
            exp
            |> Internal.validateExpr nodeMap
            ?> fun _ -> 
                let (inputMap, inputs) = Internal.getExprInputs exp
                let comp = compFun elemToStore inputMap
                inputs
                |> ResList.map (fun name ->
                    name
                    |> Internal.validateDriven nodeMap
                    ?> fun _ -> Ok { range = Range.max(); connections = [|{ myRange = Range.max(); theirName = name; theirPinNum = 0u; theirRange = Range.max() }|] })
                |> ResList.toResArray
                ?> fun ports -> 
                    let name = getName()
                    nodeMap.Add (name, { comp = comp; inputs = ports })
                    Ok name

        let integrateExpression exp =   
            integrateComp (fun exp inp -> Expression (exp, inp)) exp exp

        let integrateEventControl evCon =
            match evCon with
            | Star -> 
                let name = getName()
                nodeMap.Add(name, { comp = EventControl (evCon, Map.empty); inputs = [||] })
                Ok name
            | EventList e ->
                let exp =
                    e
                    |> List.map EventExpressionT.unwrap
                    |> PrimaryT.Concat
                    |> Primary
                integrateComp (fun (evCon: EventControlT) inp -> EventControl (evCon, inp)) evCon exp

        let addConnections pin netLVal name =
            netLVal
            |> Internal.validateDrivenNetLVal nodeMap
            ?> Internal.getRangedList nodeMap
            ?> ResList.map (fun (myName, myRange, theirRange) -> 
                // No need to check if myName is in map as this is checked in validateDriven
                nodeMap.[myName].inputs.[0].addConnection
                    myName
                    myRange
                    name
                    pin
                    theirRange)
            ?> ResList.ignore

        let processModuleItemDeclaration =
            function
            | ModuleItemDeclaration a ->
                a.names
                |> ResList.map (fun name ->
                    if nodeMap.ContainsKey name
                    then Error <| sprintf "The identifier %A is used multiple times for an input/output or non port reg/wire." name
                    else 
                        let range = Util.optRangeTToRange a.range
                        let node = 
                            match a.decType with
                            | Wire -> Node.initWireComp range
                            | Reg -> Node.initRegComp range
                        nodeMap.Add (name, node)
                        Ok ())
                ?> ResList.ignore
            | _ -> Ok ()

        let processContinuousAssign =
            function
            | ContinuousAssign a ->
                a
                |> ResList.map (fun netAssign ->
                    netAssign.RHS
                    |> integrateExpression 
                    ?> addConnections 0u netAssign.LHS)
                ?> ResList.ignore
            | _ -> Ok ()

        let processInitialConstruct =
            function
            | InitialConstruct a -> 
                a
                |> ResList.map (fun blockingAssignment ->
                    let value = ConstExprEval.evalConstExpr blockingAssignment.RHS
                    blockingAssignment.LHS
                    |> Internal.getRangedList nodeMap
                    ?> fun lst -> 
                        lst
                        |> ResList.map (fun (myName, myRange, theirRange) ->
                            if nodeMap.ContainsKey myName
                            then
                                match nodeMap.[myName].comp with
                                | RegComp rc
                                | OutputReg rc ->
                                    let maskedValue = VNum.(<<<) (value.selectRange theirRange, VNum myRange.lower)  
                                    let maskedInit = rc.initVal.mask(Down, myRange.toMask()).maskUnknown(myRange.toMask())
                                    rc.initVal <- maskedValue ||| maskedInit
                                    Ok()
                                | InputComp _
                                | OutputWire _
                                | WireComp _ -> Error <| sprintf "Cannot assign initial value to wire type %A. Only reg types can have initial values." myName
                                | _ -> Error <| sprintf "Cannot assign initial value to %A, as it is not a reg type." myName
                            else Error <| sprintf "Cannot assign initial value to %A, as it has not been declared as an input/output/wire/reg." myName))
                ?> ResList.ignore
            | _ -> Ok ()

        let processModuleInstantiation =
            function
            | ModuleInstantiation a -> 
                modDecs
                |> List.tryFind (fun md -> md.name = a.Name)
                |> function
                | None -> Error <| sprintf "Could not find module declaration for %A." a.Name
                | Some md ->
                    let (inputMap, outputMap) = Internal.getModulePortMaps md
                    a.Module.PortConnections
                    |> Internal.validateModuleConnections nodeMap md
                    ?> function
                    | Unnamed exps -> 
                        let indexedExps = List.indexed exps
                        let getList f =
                            indexedExps
                            |> List.choose(fun (i,exp) -> 
                                let (pName,pType,pRange) = md.ports.[i]
                                f pName pType pRange exp)
                        let inputs pName pType pRange exp =
                            match pType with
                            | Input _ -> Some (inputMap.[pName], pRange, exp)
                            | _ -> None
                        let outputs pName pType pRange exp =
                            match pType with
                            | Output _ -> Some (outputMap.[pName], pRange, exp)
                            | _ -> None
                        Ok (getList inputs, getList outputs)
                    | Named nExps -> 
                        let unknownConst = Primary (Number (VNum.unknown 1u))
                        let allPorts = 
                            nExps
                            |> List.map (fun n ->
                                match n.Value with
                                | Some exp -> (n.Name, exp)
                                | None -> (n.Name, unknownConst))
                        (([], []), allPorts)
                        ||> List.fold (fun (inputs, outputs) (pName, pExp) ->
                            // this does not need to be a try find as it has already been verified
                            let (_, modPType, modPRange) = 
                                md.ports
                                |> List.find (fun (modPName, _, _) -> modPName = pName)
                            match modPType with
                            | Input -> ((inputMap.[pName], modPRange, pExp)::inputs, outputs)
                            | Output _ -> (inputs, (outputMap.[pName], modPRange, pExp)::outputs))
                        |> Ok
                    ?> fun (inputExprs, outputExprs) ->
                        inputExprs
                        |> ResList.map (fun (i, range, exp) ->
                            exp
                            |> integrateExpression
                            ?>> fun expName ->
                                let connection =
                                    { myRange = range
                                      theirName = expName
                                      theirPinNum = i
                                      theirRange = Range.max() }
                                { range = range
                                  connections = [| connection |] })
                        ?>> Array.ofList
                        ?>> fun ports ->
                            let content =
                                { moduleName = a.Name
                                  inputMap = inputMap
                                  outputMap = outputMap }
                            let node =
                                { comp = ModuleInst content
                                  inputs = ports }
                            nodeMap.Add(a.Module.Name, node)
                        ?> fun _ ->
                            outputExprs
                            |> ResList.map (fun (i, range, exp) ->
                                exp
                                |> Internal.expToNetLVal nodeMap
                                ?> fun netLVal -> addConnections i netLVal a.Module.Name)
                            ?> ResList.ignore
            | _ -> Ok ()

        // TODO: Separate timing statement and implement - should be input port 0
        // TODO: Get inputs of statement, build inputMap (starting with port 1), and record connections
        // TODO: Get outputs (NetLValueT) of statement, build outputMap, and record connections
        let processAlwaysConstruct =
            function
            | AlwaysConstruct a -> 
                a.Control 
                |> integrateEventControl
                ?> fun controlName ->
                    let controlPort =
                        let connection =
                            { myRange = Range.max()
                              theirName = controlName
                              theirPinNum = 0u
                              theirRange = Range.max() } 
                        { range = Range.max(); connections = [| connection |] }
                    let (inputMap, inputNames) = Internal.getStatementInputs a.Statement
                    let (outputMap, outputNames) = Internal.getStatementOutputs a.Statement
                    inputNames
                    |> ResList.map (Internal.validateDriver nodeMap)
                    ?> fun _ ->
                        outputNames
                        |> ResList.map (Internal.validateDriven nodeMap)
                    ?>> fun _ ->
                        let name = getName()
                        let inputPorts =
                            [controlPort] @
                                (inputNames
                                |> List.map (fun inName -> 
                                    let connection =
                                        { myRange = Range.max()
                                          theirName = inName
                                          theirPinNum = 0u
                                          theirRange = Range.max() } 
                                    { range = Range.max(); connections = [| connection |] }))
                            |> Array.ofList
                        nodeMap.Add(name, { comp = Always { body = a.Statement; inputMap = inputMap; outputMap = outputMap }; inputs = inputPorts })
                        // outputNames
                        // |> List.map (fun outName -> 
                        //     nodeMap.[outName].inputs.[0].connections.)
                        // TODO: looks like outputs do need the ranges <upside down smiley face>
            | _ -> Ok ()

        let processItems items funcs =
            funcs
            |> ResList.map (fun func -> ResList.map func items)

        processItems items 
            [ processModuleItemDeclaration
              processContinuousAssign
              processInitialConstruct
              processModuleInstantiation
              processAlwaysConstruct]
        ?> fun _ -> Ok { modDec = thisModDec; nodes = nodeMap.Map }
