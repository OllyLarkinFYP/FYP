module Compiler

open System
open AST
open Netlist
open CommonHelpers
open CommonTypes

let collectDecs (asts: ASTT list) : ModuleDeclaration list =
    let orderList order lst =
        order
        |> List.map (fun name ->
            lst
            |> List.tryFind (fun (pname, dir, range) -> name = pname)
            |> function
            // TODO: None here implies that the port list and port declarations don't match up
            | None -> raise <| NotImplementedException() 
            | Some p -> p)
    let processPortDec (portDec: PortDeclarationT) =
        match portDec.range with
        | None -> (portDec.name, portDec.dir, Single)
        | Some r -> 
            let lsb = (ConstExprEval.evalConstExpr r.LSB).toInt() |> uint
            let msb = (ConstExprEval.evalConstExpr r.MSB).toInt() |> uint
            (portDec.name, portDec.dir, Ranged (msb,lsb))
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
        let nodeMap : Map<IdentifierT,Node> =
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

        let processModuleItemDeclaration =
            function
            | ModuleItemDeclaration a ->
                raise <| NotImplementedException()
            | _ -> Ok ()

        let processContinuousAssign =
            function
            | ContinuousAssign a ->
                raise <| NotImplementedException()
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
            processModuleInstantiation
            processInitialConstruct
            processAlwaysConstruct
        ]

        match result with
        | Error e -> Error e
        | Ok _ -> Ok { modDec = thisModDec; nodes = nodeMap }
