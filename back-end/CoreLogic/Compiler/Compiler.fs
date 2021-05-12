module Compiler

open System
open AST
open CommonTypes
open Netlist
open CommonHelpers
open CommonHelpers.Operators

module private Helpers =
    let safeMapAdd (map: Map<'a,'b>) (key: 'a, value: 'b) =
        if map.ContainsKey key
        then Error <| sprintf "%A is already a registered component. It cannot be declared again." key
        else Ok <| map.Add(key,value)

    let netLValToRangeList (netlist: Netlist) (netLVal: NetLValueT) =
        let rec toRangeListRec (offset, currLst) =
            function
            | NetLValueT.Ranged rangedNLV ->
                if netlist.variables.ContainsKey rangedNLV.Name
                then
                    let range = Util.optRangeTToRangeDefault netlist.variables.[rangedNLV.Name].range rangedNLV.Range
                    let entry = (rangedNLV.Name, range, range.offset offset)
                    Ok (offset + range.size, entry::currLst)
                else Error <| sprintf "Cannot find the component %A." rangedNLV.Name
            | NetLValueT.Concat c ->
                ((offset, currLst), List.rev c)
                ||> ResList.fold toRangeListRec
        // Returns result of list of (name, idenRange, valueRange)
        toRangeListRec (0u,[]) netLVal ?>> snd

    let rec getExprVars exp =
        let rec getPrimaryVars primary =
            match primary with
            | PrimaryT.Ranged r -> [r.Name]
            | PrimaryT.Concat c -> List.collect getExprVars c
            | PrimaryT.Brackets b -> getExprVars b
            | _ -> []
        match exp with
        | Primary p -> getPrimaryVars p
        | UniExpression u -> getExprVars u.Expression
        | BinaryExpression b -> getExprVars b.LHS @ getExprVars b.RHS
        | CondExpression c -> getExprVars c.Condition @ getExprVars c.TrueVal @ getExprVars c.FalseVal
        |> List.distinct


module private Internal =
    let processInputOutput md =
        md.ports
        |> List.map (fun (name, pType, range) ->
            match pType with
            | Input -> (name, InputComp range)
            | Output Wire -> (name, WireComp { range = range; drivers = [] })
            | Output Reg -> (name, RegComp { range = range; initVal = VNum.unknown range.size; drivers = [] }))
        |> Map.ofList

    let processVariables netlist =
        function
        | ModuleItemDeclaration mid ->
            let newEntries =
                mid.names
                |> List.map (fun name ->
                    let range = Util.optRangeTToRange mid.range
                    let node =
                        match mid.decType with
                        | Wire -> WireComp { range = range; drivers = [] }
                        | Reg -> RegComp { range = range; initVal = VNum.unknown range.size; drivers = [] }
                    (name, node))
            (netlist.variables, newEntries)
            ||> ResList.fold Helpers.safeMapAdd
            ?>> fun newVars -> { netlist with variables = newVars }
        | _ -> Ok netlist

    let processInitialBlock netlist =
        function
        | InitialConstruct ic ->
            ic
            |> ResList.map (fun assignment ->
                let rhsValue = ConstExprEval.evalConstExpr assignment.RHS
                assignment.LHS
                |> Helpers.netLValToRangeList netlist 
                ?> ResList.map (fun (name, idenRange, valueRange) ->
                    match netlist.variables.[name] with
                    | RegComp rc -> 
                        rc.initVal <- rc.initVal.setRange idenRange (rhsValue.getRange valueRange)
                        Ok()
                    | _ -> Error <| sprintf "Can only assign initial values to 'reg' types. %A is not a 'reg'." name))
            ?>> fun _ -> netlist
        | _ -> Ok netlist

    let processContinuousAssigns netlist =
        function
        | ContinuousAssign ca ->
            ca
            |> ResList.map (fun netAssign ->
                netAssign.LHS
                |> Helpers.netLValToRangeList netlist
                ?> ResList.map (fun (name, idenRange, valueRange) ->
                    let exprOutCont = 
                        { expression = netAssign.RHS
                          vars = Helpers.getExprVars netAssign.RHS
                          range = valueRange }
                    match netlist.variables.[name] with
                    | RegComp rc -> 
                        let driver = RegExpressionOutput exprOutCont
                        rc.drivers <- (idenRange,driver)::rc.drivers
                        Ok()
                    | WireComp wc -> 
                        let driver = WireExpressionOutput exprOutCont
                        wc.drivers <- (idenRange,driver)::wc.drivers
                        Ok()
                    | _ -> Error <| sprintf "Cannot drive %A as it is not a reg/wire. Only reg/wires can be driven." name))
                ?>> fun _ -> netlist
        | _ -> Ok netlist

    let processModuleInstances mds netlist =
        function
        | ModuleInstantiation mi -> Ok netlist
        | _ -> Ok netlist

    let processAlwaysBlocks netlist =
        function
        | AlwaysConstruct ac -> Ok netlist
        | _ -> Ok netlist

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
        List.map processPortDec dec.ports
    let getDec ast : ModuleDeclaration =
        let ports = 
            match ast.info with
            | ModDec1 dec -> processModDec1 dec
            | ModDec2 dec -> processModDec2 dec
        { ModuleDeclaration.name = ast.name; ports = ports}
    List.map getDec asts

let compileAST (modDecs: ModuleDeclaration list) (ast: ASTT) =
    modDecs
    |> List.tryFind (fun md -> md.name = ast.name)
    |> function
    | None -> 
        // This should be thrown if the provided module declarations do not include the ast to be compiled
        raise <| ArgumentException()
    | Some md ->
        let init =
            { moduleDeclaration = md
              variables = Internal.processInputOutput md
              moduleInstances = Map.empty
              alwaysBlocks = Map.empty }
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

        (init, items)
        |> ResList.tupleFold Internal.processVariables
        ?> ResList.tupleFold Internal.processInitialBlock
        ?> ResList.tupleFold Internal.processContinuousAssigns
        ?> ResList.tupleFold (Internal.processModuleInstances modDecs)
        ?> ResList.tupleFold Internal.processAlwaysBlocks
        ?>> fst
