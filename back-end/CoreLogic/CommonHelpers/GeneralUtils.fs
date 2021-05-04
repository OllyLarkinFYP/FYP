namespace CommonHelpers

open System
open AST
open Netlist

module Util =
    let rec resListMap f =
        function
        | [] -> Ok []
        | hd::tl ->
            match f hd with
            | Error e -> Error e
            | Ok res ->
                match resListMap f tl with
                | Error e -> Error e
                | Ok processedTl -> Ok (res :: processedTl)

    let rangeTToRange (r: RangeT) : Range =
        let lsb = (ConstExprEval.evalConstExpr r.LSB).toInt() |> uint
        let msb = (ConstExprEval.evalConstExpr r.MSB).toInt() |> uint
        if lsb = msb
        then Single
        else Ranged (msb,lsb)

    let optRangeTToRange (r: RangeT option) : Range =
        match r with
        | Some range -> rangeTToRange range
        | None -> Single
