namespace Compiler

module CompResult =

    type CompErrors = string list

    type CompWarnings = string list

    type CompRet<'Result> =
        | Succ of 'Result
        | SuccW of 'Result * CompWarnings
        | Fail of CompErrors

    /// A kind of bind function for the above type
    let (?>) (r: CompRet<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> f v
        | SuccW (v, w) ->
            match f v with
            | Fail e -> Fail e
            | Succ v' -> SuccW (v', w)
            | SuccW (v', w') -> SuccW (v', w @ w')

    /// Similar to bind except 'f' does not need to return a CompRet, just a value
    let (?>>) (r: CompRet<_>) f =
        match r with
        | Fail e -> Fail e
        | Succ v -> Succ (f v)
        | SuccW (v, w) -> SuccW (f v, w)
