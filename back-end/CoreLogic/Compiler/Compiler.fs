namespace Compiler

open System
open AST
open Netlist
open Helpers

module rec Compiler =

    let compileAST (ast: ASTT) : Result<Netlist, string> =
        match ast with
        | Empty -> Error "Empty AST"
        | ModuleDeclaration m -> 
            compileModDecBody m.ModDec
            |> function
            | Error e -> Error e
            | Ok nodes -> 
                { Netlist.name = getModName m.ModDec
                  inputs = getModInputs m.ModDec
                  outputs = getModOutputs m.ModDec
                  nodes = nodes } |> Ok

    let compileModDecBody (modDec: ModuleDeclarationT) : Result<Map<string,Node>,string> = 
        raise <| NotImplementedException()
