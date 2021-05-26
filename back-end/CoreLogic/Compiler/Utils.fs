namespace Compiler

open Compiler.CompResult

module Utils =

    /// A bind operator for the CompRes type
    let (?>) (r: CompRes<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> f v
        | Warn (v, w) ->
            match f v with
            | Fail e -> Fail e
            | Succ v' -> Warn (v', w)
            | Warn (v', w') -> Warn (v', w @ w')

    /// Similar to bind except 'f' does not need to return a CompRes, just a value.
    /// Cannot generate errors or warnings
    let (?>>) (r: CompRes<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> Succ (f v)
        | Warn (v, w) -> Warn (f v, w)

    type List<'T> with
        static member compResMap mapper list =
            let rec mapRec =
                function
                | [] -> Succ []
                | hd::tl ->
                    match mapper hd with
                    | Fail e ->
                        match mapRec tl with
                        | Fail e' -> Fail (e @ e')
                        | _ -> Fail e
                    | _ as r -> 
                        r ?> fun pHd ->
                            mapRec tl ?>> fun pTl -> pHd::pTl
            mapRec list 

        static member compResFold folder list (state: 'State) =
            let rec foldRec lst s =
                match lst with
                | [] -> Succ s
                | hd::tl ->
                    match folder s hd with
                    | Fail e ->
                        match foldRec tl s with
                        | Fail e' -> Fail (e @ e')
                        | _ -> Fail e
                    | _ as r -> r ?> foldRec tl
            foldRec list state
