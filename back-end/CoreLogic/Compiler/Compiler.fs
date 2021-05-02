module rec Compiler

open System
open AST
open Netlist
open CommonHelpers
open Result

// let getPorts (modDec: ModuleDeclarationT) =
//     let getPortsSpec (input: bool) (modDec: ModuleDeclarationT) =
//         let getInfo (p: PortDeclarationT) =
//             let range =
//                 match p.range with
//                 | None -> Single
//                 | Some r ->
//                     let msb = ConstExprEval.evalConstExpr r.MSB
//                     let lsb = ConstExprEval.evalConstExpr r.MSB
//                     Ranged (uint32 msb.value, uint32 lsb.value)
//             let pType =
//                 match p.dir with
//                 | Input a ->
//                     match a with
//                     | InputPortDecType.Wire -> PortType.Wire
//                     | InputPortDecType.Logic -> PortType.Logic
//                 | Output a -> 
//                     match a with
//                     | OutputPortDecType.Wire -> PortType.Wire
//                     | OutputPortDecType.Logic -> PortType.Logic
//                     | OutputPortDecType.Reg -> PortType.Reg
//             (p.name, pType, { range = range; connections = [||] })

//         // TODO: this function also needs to check that every input is instantiated
//         let reorderPorts names (portDecs: PortDeclarationT list) =
//             names
//             |> List.map (fun name ->
//                     portDecs
//                     |> List.tryFind (fun elem -> elem.name = name)
//                     |> function
//                     | Some p -> p
//                     | None -> failwithf "Unhandled error: port list does not line up with instantiations" )

//         match modDec.info with
//         | ModDec1 m ->      // TODO: check that the internal declarations match the external ones
//             let portNames =
//                 m.ports
//                 |> List.map (fun a -> a.name)
//             m.body          
//             |> List.choose (
//                 function
//                 | PortDeclaration pd -> Some pd
//                 | _ -> None )
//             |> reorderPorts portNames
//         | ModDec2 m -> m.ports
//         |> List.filter (fun p -> 
//             match p.dir with
//             | Input _ -> input
//             | Output _ -> not input)
//         |> List.map getInfo
//         |> List.toArray
    
//     (getPortsSpec true modDec, getPortsSpec false modDec)

// let compileAST (ast: ASTT) : Result<Netlist, string> =
//     compileModDecBody ast.modDec
//     |> bind ( fun nodes -> 
//         let inputs, outputs = getPorts ast.modDec
//         { Netlist.name = ast.modDec.name
//           inputs = inputs
//           outputs = outputs
//           nodes = nodes } |> Ok )

// let private compileModDecBody (modDec: ModuleDeclarationT) : Result<Map<string,Node>,string> = 
//     let nodeMap: Map<string,Node> = Map.empty
//     let items =
//         match modDec.info with
//         | ModDec2 info -> info.body
//         | ModDec1 info ->
//             info.body
//             |> List.choose (
//                 function
//                 | PortDeclaration _ -> None
//                 | NonPortModuleItem i -> Some i )

//     raise <| NotImplementedException()

let gatherDecs (asts: ASTT list) : ModuleDeclaration list =
    raise <| NotImplementedException ()
