namespace Compiler

open Compiler.CompResult

module Utils =

    /// A kind of bind function for the above type
    let (?>) (r: CompRes<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> f v
        | Warn (v, w) ->
            match f v with
            | Fail e -> Fail e
            | Succ v' -> Warn (v', w)
            | Warn (v', w') -> Warn (v', w @ w')

    /// Similar to bind except 'f' does not need to return a CompRes, just a value
    let (?>>) (r: CompRes<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> Succ (f v)
        | Warn (v, w) -> Warn (f v, w)

    type List<'T> with
        static member compRetMap mapper list =
            let rec compRetMapRec =
                function
                | [] -> Succ []
                | hd::tl ->
                    mapper hd
                    ?> fun v ->
                        compRetMapRec tl
                        ?>> fun pTl -> v::pTl
            compRetMapRec list 

