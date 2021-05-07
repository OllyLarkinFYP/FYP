module Compiler

open System
open AST
open Netlist
open CommonHelpers
open CommonTypes

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
    let getDec ast =
        let ports = 
            match ast.info with
            | ModDec1 dec -> processModDec1 dec
            | ModDec2 dec -> processModDec2 dec
        { name = ast.name; ports = ports}
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
                    |> List.choose (fun (n, r) ->
                        if n = name
                        then 
                            let range =
                                match Util.optRangeTToRangeWithNodes nodeMap name r with
                                | Ok r -> r
                                // TODO: handle this
                                | Error e -> raise <| Exception e
                            Some 
                                { myRange = Range.max()
                                  theirName = name
                                  theirPinNum = 0u    // as coming from input/output/wire/reg -> single output
                                  theirRange = range }
                        else None)
                    |> List.toArray
                let ports = [| for name in inputs -> { range = Range.max(); connections = getConnections name } |]
                let name = getName()
                nodeMap <- nodeMap.Add (name, { comp = comp; inputs = ports })
                name

        let processModuleItemDeclaration =
            function
            | ModuleItemDeclaration a ->
                a.names
                |> Util.resListMap (fun name ->
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
                |> function
                | Ok _ -> Ok ()
                | Error e -> Error e
            | _ -> Ok ()

        let processContinuousAssign =
            let getRangedList = // [ (myName, myRange, theirRange) ]
                let rec getRangedListRec offset =
                    function
                    | NetLValueT.Ranged r ->
                        let range =
                            match Util.optRangeTToRangeWithNodes nodeMap r.Name r.Range with
                            | Ok range -> range
                            | Error e -> raise <| NotImplementedException()
                        [ (r.Name, range, range.offset offset) ], (offset + range.size())
                    | NetLValueT.Concat c ->
                        (c, ([], offset))
                        ||> List.foldBack (fun netLVal (lst, off) ->
                            let curr, currOff = getRangedListRec off netLVal
                            (curr @ lst, currOff))
                getRangedListRec 0u
                >> fst
                >> fun a -> printfn "*******************\n Ranged List: \n%A" a; a
            function
            | ContinuousAssign a ->
                a
                |> Util.resListMap (fun netAssign ->
                    let expName = integrateExpression netAssign.RHS
                    getRangedList netAssign.LHS
                    |> Util.resListMap (fun (myName, myRange, theirRange) -> 
                        nodeMap.[myName].inputs.[0].addConnection
                            myName
                            myRange
                            expName
                            0u
                            theirRange))
                |> function
                | Ok _ -> Ok()
                | Error e -> Error e
            | _ -> Ok ()

        let processModuleInstantiation =
            function
            | ModuleInstantiation a -> 
                raise <| NotImplementedException()
            | _ -> Ok ()

        let processInitialConstruct =
            function
            | InitialConstruct a -> 
                raise <| NotImplementedException()
            | _ -> Ok ()

        let processAlwaysConstruct =
            function
            | AlwaysConstruct a -> 
                raise <| NotImplementedException()
            | _ -> Ok ()

        let processItems items funcs =
            funcs
            |> Util.resListMap (fun func -> Util.resListMap func items)

        let result = processItems items [
            processModuleItemDeclaration
            processContinuousAssign
            // processModuleInstantiation
            // processInitialConstruct
            // processAlwaysConstruct
        ]

        match result with
        | Error e -> Error e
        | Ok _ -> Ok { modDec = thisModDec; nodes = nodeMap }
