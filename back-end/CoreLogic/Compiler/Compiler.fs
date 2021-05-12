module Compiler

open System
open AST
open CommonTypes
open Netlist
open CommonHelpers
open CommonHelpers.Operators

module private Internal =
    let safeMapAdd (map: Map<'a,'b>) (key: 'a, value: 'b) =
        if map.ContainsKey key
        then Error <| sprintf "%A is already a registered component. It cannot be declared again." key
        else Ok <| map.Add(key,value)

    let processInputOutput md =
        md.ports
        |> List.map (fun (name, pType, range) ->
            match pType with
            | Input -> (name, InputComp range)
            | Output Wire -> (name, WireComp { range = range; drivers = [] })
            | Output Reg -> (name, RegComp { range = range; initVal = VNum.unknown range.size; drivers = [] }))
        |> Map.ofList

    let processVariables netlist item =
        match item with
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
            ||> ResList.fold safeMapAdd
            ?>> fun newVars -> { netlist with variables = newVars }
        | _ -> Ok netlist

    let processInitialBlock netlist item = Ok netlist

    let processContinuousAssigns netlist item = Ok netlist

    let processModuleInstances mds netlist item = Ok netlist

    let processAlwaysBlocks netlist item = Ok netlist

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
