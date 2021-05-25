namespace Compiler

open Compiler.CompResult

module Utils =

    /// A kind of bind function for the above type
    let (?>) (r: CompRes<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> f v
        | SuccW (v, w) ->
            match f v with
            | Fail e -> Fail e
            | Succ v' -> SuccW (v', w)
            | SuccW (v', w') -> SuccW (v', w @ w')

    /// Similar to bind except 'f' does not need to return a CompRes, just a value
    let (?>>) (r: CompRes<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> Succ (f v)
        | SuccW (v, w) -> SuccW (f v, w)

    type List<'T> with
        static member compRetMap mapper list =
            raise <| System.NotImplementedException()
