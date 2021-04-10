namespace Compiler

open System
open AST
open Netlist
open CommonTypes
open CommonHelpers

module Helpers =

    let getModName (modDec: ModuleDeclarationT) =
        match modDec with
        | ModDec1 m -> m.Name
        | ModDec2 m -> m.Name

    let getPorts (modDec: ModuleDeclarationT) =
        let getPortsSpec (input: bool) (modDec: ModuleDeclarationT) =
            let getInfo (port: PortDeclarationT) =
                let p = 
                    match port with
                    | Input i ->
                        match i with
                        | InputDeclarationT.WireDec a -> a
                        | InputDeclarationT.LogicDec a -> a
                    | Output i ->
                        match i with
                        | WireDec a -> a
                        | RegDec a -> a
                        | LogicDec a -> a
                let name = p.Name
                let range =
                    match p.Range with
                    | None -> Single
                    | Some r ->
                        let msb = ConstExprEval.eval r.MSB
                        let lsb = ConstExprEval.eval r.MSB
                        Ranged (uint32 msb.value, uint32 lsb.value)
                (name, range) 
            match modDec with
            | ModDec1 m -> raise <| NotImplementedException() // TODO: implement this
            | ModDec2 m -> 
                m.Ports
                |> List.filter ( 
                    function
                    | Input _ -> input
                    | Output _ -> not input )
                |> List.map getInfo
                |> List.toArray
        
        (getPortsSpec true modDec, getPortsSpec false modDec)
