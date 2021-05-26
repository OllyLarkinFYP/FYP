namespace Compiler

open Compiler.CompResult

module Errors =
    module CompilerAPI =
        let astNotProvided name = Fail [ sprintf "The AST for %A was not provided to the compiler." name ]

    module ProcessPorts =
        let portsDontMatchPortDecs ports decs = Fail [ sprintf "The ports declared in the module definition do not match the ports declared inside the module.\nThe module definition has ports %A, but the the ports declared inside the module are %A" ports decs ]

    module ProcessVariables =
        let duplicateDefinition name = Fail [ sprintf "The component %A has already been declared. It cannot be re-declared." name ]