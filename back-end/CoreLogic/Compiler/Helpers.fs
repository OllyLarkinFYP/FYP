namespace Compiler

open System
open AST
open Netlist
open CommonTypes

module Helpers =

    module rec EvalConstExpr =

        type Value = { value: NumT; size: SizeT }

        let evalConstExpr (expr: ConstantExpressionT) : Value =
            // match expr with
            // | ConstantExpressionT.Primary ex -> evalConstPrimary ex
            // | ConstantExpressionT.UniExpression ex -> evalConstUni ex.Operator ex.Expression
            // | ConstantExpressionT.BinaryExpression ex -> evalConstBin ex.BinOperator ex.LHS ex.RHS
            // | ConstantExpressionT.CondExpression ex -> evalConstCond ex.Condition ex.TrueVal ex.FalseVal
            raise <| NotImplementedException()

        let evalConstPrimary (expr: ConstantPrimaryT) : Value =
            match expr with
            | ConstantPrimaryT.Number n -> { value = n.Value; size = Option.defaultValue 32u n.Size }
            | ConstantPrimaryT.Concat c -> raise <| NotImplementedException()
            | ConstantPrimaryT.Brackets ex -> evalConstExpr ex

    let getModName (modDec: ModuleDeclarationT) =
        match modDec with
        | ModDec1 m -> m.Name
        | ModDec2 m -> m.Name

    let getModInputs (modDec: ModuleDeclarationT) =
        match modDec with
        | ModDec1 m -> raise <| NotImplementedException()
        | ModDec2 m -> 
            m.Ports
            |> List.filter ( 
                function
                | Output _ -> false
                | Input _ -> true )
            |> List.map ( 
                function
                | Output _ -> failwith "Unreachable code reached"
                | Input i ->
                    let getInfo (dec: {| Range: RangeT option; Signed: bool; Name: IdentifierT |}) =
                        let name = dec.Name
                        let range =
                            match dec.Range with
                            | None -> Single
                            | Some r ->
                                let msb = EvalConstExpr.evalConstExpr r.MSB
                                let lsb = EvalConstExpr.evalConstExpr r.MSB
                                Ranged (msb.value |> SizeT.CastUInt64, lsb.value |> SizeT.CastUInt64)
                        (name, range)
                    match i with
                    | InputDeclarationT.WireDec d -> getInfo d
                    | InputDeclarationT.LogicDec d -> getInfo d )
            |> List.toArray

    let getModOutputs (modDec: ModuleDeclarationT) =
        match modDec with
        | ModDec1 m -> raise <| NotImplementedException()
        | ModDec2 m -> 
            m.Ports
            |> List.filter ( 
                function
                | Input _ -> false
                | Output _ -> true )
            |> List.map ( 
                function
                | Input _ -> failwith "Unreachable code reached"
                | Output i ->
                    let getInfo (dec: {| Range: RangeT option; Signed: bool; Name: IdentifierT |}) =
                        let name = dec.Name
                        let range =
                            match dec.Range with
                            | None -> Single
                            | Some r ->
                                let msb = EvalConstExpr.evalConstExpr r.MSB
                                let lsb = EvalConstExpr.evalConstExpr r.MSB
                                Ranged (msb.value |> SizeT.CastUInt64, lsb.value |> SizeT.CastUInt64)
                        (name, range)
                    match i with
                    | OutputDeclarationT.WireDec d -> getInfo d
                    | OutputDeclarationT.LogicDec d -> getInfo d
                    | OutputDeclarationT.RegDec d -> getInfo d )
            |> List.toArray

    // TODO: Combine getmodinputs and getmodoutputs
