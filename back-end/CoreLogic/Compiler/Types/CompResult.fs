namespace Compiler

open CommonTypes

module CompResult =

    type CompErrors = WithPos<string> list

    type CompWarnings = WithPos<string> list

    type CompRes<'Result> =
        | Succ of 'Result
        | Warn of 'Result * CompWarnings
        | Fail of CompErrors
