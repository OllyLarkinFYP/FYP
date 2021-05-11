namespace CommonHelpers

open System
open AST
open Netlist
open CommonTypes

module Operators =
    let (?>) r f = Result.bind f r
    let (?>>) r f =
        match r with
        | Ok a -> Ok (f a)
        | Error e -> Error e 

module ResList =
    let rec map f =
        function
        | [] -> Ok []
        | hd::tl ->
            match f hd with
            | Error e -> Error e
            | Ok res ->
                match map f tl with
                | Error e -> Error e
                | Ok processedTl -> Ok (res :: processedTl)

    let rec choose f =
        function
        | [] -> Ok []
        | hd::tl ->
            match f hd with
            | Error e -> Error e
            | Ok res ->
                match choose f tl with
                | Error e -> Error e
                | Ok processedTl ->
                    match res with
                    | Some a -> Ok (a::processedTl)
                    | None -> Ok processedTl
                    
    let rec fold folder state =
        function
        | [] -> Ok state
        | hd::tl ->
            match folder state hd with
            | Error e -> Error e
            | Ok acc -> fold folder acc tl

    let toResArray =
        function
        | Ok lst -> Ok <| List.toArray lst
        | Error e -> Error e

    let ignore a = Ok()

module Util =
    let rangeTToRange (r: RangeT) : Range =
        let lsb = (ConstExprEval.evalConstExpr r.LSB).toInt() |> uint
        let msb = (ConstExprEval.evalConstExpr r.MSB).toInt() |> uint
        if lsb = msb
        then Single lsb
        else Ranged (msb,lsb)

    let optRangeTToRange (r: RangeT option) : Range =
        match r with
        | Some range -> rangeTToRange range
        | None -> Single 0u

    let optRangeTToRangeWithNodes (nodes: MutMap<IdentifierT,Node>) (name: string) (r: RangeT option) =
        match r with
        | Some range -> Ok <| rangeTToRange range
        | None -> 
            match nodes.TryFind name with
            | None -> Error <| sprintf "Could not find range of %A. Cannot be found in node map." name
            | Some node -> 
                match node.comp with
                | InputComp r -> Ok r
                | OutputReg c -> Ok c.range
                | OutputWire r -> Ok r
                | RegComp c -> Ok c.range
                | WireComp r -> Ok r
                | _ -> Error <| sprintf "Cannot find range of non-input/output/reg/wire. Unable to get range for %A" name

    let optRangeTToRangeDefault (defaultVal: Range) (r: RangeT option) =
        match r with
        | Some range -> rangeTToRange range
        | None -> defaultVal

    let duplicates comp lst =
        (lst,lst)
        ||> List.allPairs
        |> List.choose (fun (a,b) -> 
            if comp a b
            then Some a
            else None)
