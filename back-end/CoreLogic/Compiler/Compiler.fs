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
                | PrimaryT.Ranged r -> [ r.Name, r.Range ]
                | PrimaryT.Concat c -> List.collect getExprInputsRec c
                | Brackets b -> getExprInputsRec b
            | UniExpression u -> getExprInputsRec u.Expression
            | BinaryExpression b -> (getExprInputsRec b.LHS) @ (getExprInputsRec b.RHS)
            | CondExpression c -> (getExprInputsRec c.Condition) @ (getExprInputsRec c.TrueVal) @ (getExprInputsRec c.FalseVal)
        let connections = 
            exp
            |> getExprInputsRec 
        let names = 
            connections
            |> List.map fst
            |> List.distinct
        let map = 
            names
            |> List.indexed
            |> List.map (fun (a,b) -> (b, uint a))
            |> Map.ofList
        (map, names, connections)

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
        |> Array.ofList
    let processModDec2 (dec: {| ports: PortDeclarationT List; body: NonPortModuleItemT List |}) =
        dec.ports
        |> List.map processPortDec
        |> Array.ofList
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

    match thisModDecRes with
    | Error e -> Error e
    | Ok thisModDec ->
        let mutable nodeMap : Map<IdentifierT,Node> =
            thisModDec.ports
            |> Array.map (fun (portName, portDir, portRange) ->
                let initFunc = 
                    match portDir with
                    | Input -> Node.initInputComp
                    | Output t ->
                        match t with
                        | Wire -> Node.initOutputReg
                        | Reg -> Node.initOutputWire
                (portName, initFunc portRange))
            |> Map.ofArray

        let getRangedList = // [ (myName, myRange, theirRange) ]
            let rec getRangedListRec offset =
                function
                | NetLValueT.Ranged r ->
                    match Util.optRangeTToRangeWithNodes nodeMap r.Name r.Range with
                    | Ok range ->
                        Ok ([ (r.Name, range, range.ground().offset offset) ], (offset + range.size))
                    | Error e -> Error e
                | NetLValueT.Concat c ->
                    (c, Ok ([], offset))
                    ||> List.foldBack (fun netLVal state ->
                        match state with
                        | Ok (lst, off) -> 
                            match getRangedListRec off netLVal with
                            | Ok (curr, currOff) -> Ok (curr @ lst, currOff)
                            | Error e -> Error e
                        | Error e -> Error e)
            getRangedListRec 0u
            >> function
            | Ok (lst, _) -> Ok lst
            | Error e -> Error e

        let integrateExpression =
            let getName =
                // TODO: Make "__EXP__<number>" not available as wire/reg name
                let nameBase = "__EXP__"
                let mutable nameNum = 0
                fun () ->
                    nameNum <- nameNum + 1
                    nameBase + string nameNum
            fun exp ->
                let (inputMap, inputs, connections) = Internal.getExprInputs exp
                let comp = Expression (exp, inputMap)
                let getConnections name =
                    connections
                    |> ResList.choose (fun (n, r) ->
                        if n = name
                        then 
                            match Util.optRangeTToRangeWithNodes nodeMap name r with
                            | Ok r ->
                                Ok <| Some 
                                        { myRange = Range.max()
                                          theirName = name
                                          theirPinNum = 0u    // as coming from input/output/wire/reg -> single output
                                          theirRange = r }
                            | Error e -> Error e
                        else Ok None)
                    |> function
                    | Ok a -> Ok <| List.toArray a
                    | Error e -> Error e
                // [| for name in inputs -> { range = Range.max(); connections = getConnections name } |]
                inputs
                |> ResList.map (fun name ->
                    match getConnections name with
                    | Ok conns -> Ok { range = Range.max(); connections = conns }
                    | Error e -> Error e)
                |> ResList.toResArray
                |-> fun ports -> 
                    let name = getName()
                    nodeMap <- nodeMap.Add (name, { comp = comp; inputs = ports })
                    Ok name

        let processModuleItemDeclaration =
            function
            | ModuleItemDeclaration a ->
                a.names
                |> ResList.map (fun name ->
                    if Map.containsKey name nodeMap
                    // TODO: improve error
                    then Error <| sprintf "The identifier %A is used multiple times for an input/output or non port reg/wire." name
                    else 
                        nodeMap <- 
                            let range = Util.optRangeTToRange a.range
                            let node = 
                                match a.decType with
                                | Wire -> Node.initWireComp range
                                | Reg -> Node.initRegComp range
                            nodeMap.Add (name, node)
                        Ok ())
                |-> fun _ -> Ok()
            | _ -> Ok ()

        let processContinuousAssign =
            function
            | ContinuousAssign a ->
                a
                |> ResList.map (fun netAssign ->
                    netAssign.RHS
                    |> integrateExpression 
                    |-> fun expName ->
                        netAssign.LHS
                        |> getRangedList
                        |-> fun lst -> 
                            lst
                            |> ResList.map (fun (myName, myRange, theirRange) -> 
                                if nodeMap.ContainsKey myName 
                                then nodeMap.[myName].inputs.[0].addConnection
                                        myName
                                        myRange
                                        expName
                                        0u
                                        theirRange
                                else Error <| sprintf "%A is not a registered input/output/wire/reg." myName))
                |-> fun _ -> Ok ()
            | _ -> Ok ()

        let processInitialConstruct =
            function
            | InitialConstruct a -> 
                a
                |> ResList.map (fun blockingAssignment ->
                    let value = ConstExprEval.evalConstExpr blockingAssignment.RHS
                    blockingAssignment.LHS
                    |> getRangedList
                    |-> fun lst -> 
                        lst
                        |> ResList.map (fun (myName, myRange, theirRange) ->
                            if nodeMap.ContainsKey myName
                            then
                                match nodeMap.[myName].comp with
                                | RegComp rc
                                | OutputReg rc ->
                                    let maskedValue = VNum.(<<<) (value.selectRange theirRange, VNum myRange.lower)  
                                    let maskedInit = rc.initVal.mask(Down, myRange.toMask()).maskUnknown(myRange.toMask())
                                    printfn "************\n name: %A\n myInit: %A\n myRange: %A\n theirRange: %A\n value: %A\n maskedValue: %A\n maskedInit: %A\n added values: %A"
                                        myName
                                        rc.initVal
                                        myRange
                                        theirRange
                                        value
                                        maskedValue
                                        maskedInit
                                        (maskedValue ||| maskedInit)
                                    rc.initVal <- maskedValue ||| maskedInit
                                    Ok()
                                | InputComp _
                                | OutputWire _
                                | WireComp _ -> Error <| sprintf "Cannot assign initial value to wire type %A. Only reg types can have initial values." myName
                                | _ -> Error <| sprintf "Cannot assign initial value to %A, as it is not a reg type." myName
                            else Error <| sprintf "Cannot assign initial value to %A, as it has not been declared as an input/output/wire/reg." myName))
                |-> fun _ -> Ok ()
            | _ -> Ok ()

        let processModuleInstantiation =
            function
            | ModuleInstantiation a -> 
                raise <| NotImplementedException()
            | _ -> Ok ()

        let processAlwaysConstruct =
            function
            | AlwaysConstruct a -> 
                raise <| NotImplementedException()
            | _ -> Ok ()

        let processItems items funcs =
            funcs
            |> ResList.map (fun func -> ResList.map func items)

        let result = processItems items [
            processModuleItemDeclaration
            processContinuousAssign
            processInitialConstruct
            // processModuleInstantiation
            // processAlwaysConstruct
        ]

        result |-> fun _ -> Ok { modDec = thisModDec; nodes = nodeMap }
