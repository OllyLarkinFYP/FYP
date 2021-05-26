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
        static member compRetMap mapper list =
            let rec mapRec =
                function
                | [] -> Succ []
                | hd::tl ->
                    mapper hd
                    ?> fun pHd ->
                        mapRec tl
                        ?>> fun pTl -> pHd::pTl
            mapRec list 

        static member compRetFold folder list (state: 'State) =
            let rec foldRec lst s =
                match lst with
                | [] -> Succ s
                | hd::tl ->
                    folder s hd
                    ?> foldRec tl
            foldRec list state
