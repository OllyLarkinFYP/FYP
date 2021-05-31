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

    let expToConstExpr (inputMap: Map<IdentifierT, VNum>) exp =
        let rec toConstExprRec exp =
            let toConstPrimary = 
                function
                | PrimaryT.Number v -> ConstantPrimaryT.Number v
                | PrimaryT.Ranged r ->
                    let range = optRangeTToRange r.range
                    let value = inputMap.[r.name].getRange range
                    ConstantPrimaryT.Number value
                | PrimaryT.Concat c -> ConstantPrimaryT.Concat <| List.map toConstExprRec c
                | PrimaryT.Brackets b -> ConstantPrimaryT.Brackets <| toConstExprRec b
            match exp with
            | Primary p -> ConstantExpressionT.Primary <| toConstPrimary p
            | UniExpression u ->
                ConstantExpressionT.UniExpression
                    {| Operator = u.Operator
                       Expression = toConstExprRec u.Expression |}
            | BinaryExpression b -> 
                ConstantExpressionT.BinaryExpression
                    {| LHS = toConstExprRec b.LHS
                       BinOperator = b.BinOperator
                       RHS = toConstExprRec b.RHS |}
            | CondExpression c ->
                ConstantExpressionT.CondExpression
                    {| Condition = toConstExprRec c.Condition
                       TrueVal = toConstExprRec c.TrueVal
                       FalseVal = toConstExprRec c.FalseVal |}
        toConstExprRec exp
