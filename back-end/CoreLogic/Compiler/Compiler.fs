namespace Compiler

open System
open AST
open Netlist
open Helpers

module rec Compiler =

    let compileAST (ast: ASTT) : Result<Netlist, string> =
        compileModDecBody ast.modDec
        |> function
        | Error e -> Error e
        | Ok nodes -> 
            let inputs, outputs = getPorts ast.modDec
            { Netlist.name = getModName ast.modDec
              inputs = inputs
              outputs = outputs
              nodes = nodes } |> Ok

    let compileModDecBody (modDec: ModuleDeclarationT) : Result<Map<string,Node>,string> = 
        raise <| NotImplementedException() // TODO: implement
