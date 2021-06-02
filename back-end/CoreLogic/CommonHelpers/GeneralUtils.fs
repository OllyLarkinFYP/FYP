namespace CommonHelpers

open System
open AST
open CommonTypes

module private Operators =
    let (?>) r f = Result.bind f r
    let (?>>) r f =
        match r with
        | Ok a -> Ok (f a)
        | Error e -> Error e 

module List =
    let chooseFold f (acc: 'State) (lst: List<'T>) : 'State * List<'Result> =
        ((acc, []), lst)
        ||> List.fold (fun (acc', selected) item ->
            match f acc' item with
            | acc'', Some e -> acc'', e::selected
            | acc'', None -> acc'', selected)

    let rec existsFold f (acc: 'State) (lst: List<'T>) : 'State * bool =
        match lst with
        | [] -> acc, false
        | hd::tl ->
            match f acc hd with
            | acc', true -> acc', true
            | acc', false -> existsFold f acc' tl

    let rec foldUntil folder (state: 'State) (lst: List<'T>) : 'State option =
        match lst with
        | [] -> None
        | hd::tl ->
            match folder hd with
            | Some state' -> Some state'
            | None -> foldUntil folder state tl


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

    let tuple a b = a,b

    let unreachableCode () =
        failwithf "Unreachable code reached."

    let printAndContinue a = 
        printfn "\n%A\n" a
        a
