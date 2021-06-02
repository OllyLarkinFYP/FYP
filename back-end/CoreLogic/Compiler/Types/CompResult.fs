namespace Compiler

module CompResult =

    type CompErrors = string list

    type CompWarnings = string list

    type CompRes<'Result> =
        | Succ of 'Result
        | Warn of 'Result * CompWarnings
        | Fail of CompErrors
