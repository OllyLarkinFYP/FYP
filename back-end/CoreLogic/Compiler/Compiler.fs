namespace Compiler

open System
open AST
open Netlist
open Helpers
open Result

module rec Compiler =

    let compileAST (ast: ASTT) : Result<Netlist, string> =
        compileModDecBody ast.modDec
        |> bind ( fun nodes -> 
            let inputs, outputs = getPorts ast.modDec
            { Netlist.name = ast.modDec.name
              inputs = inputs
              outputs = outputs
              nodes = nodes } |> Ok )

    let private compileModDecBody (modDec: ModuleDeclarationT) : Result<Map<string,Node>,string> = 
        let nodeMap: Map<string,Node> = Map.empty
        let items =
            match modDec.info with
            | ModDec2 info -> info.body
            | ModDec1 info ->
                info.body
                |> List.choose (
                    function
                    | PortDeclaration _ -> None
                    | NonPortModuleItem i -> Some i )

        raise <| NotImplementedException()
