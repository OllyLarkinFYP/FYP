namespace Compiler

open Compiler.CompResult

module Errors =
    module CompilerAPI =
        let astNotProvided name = Fail [ sprintf "The AST for %A was not provided to the compiler." name ]

    module ProcessPorts =
        let portsDontMatchPortDecs ports decs = Fail [ sprintf "The ports declared in the module definition do not match the ports declared inside the module.\nThe module definition has ports %A, but the the ports declared inside the module are %A" ports decs ]
        let duplicatePorts name = Fail [ sprintf "The port %A is declared multiple times. It can only be declared once." name ]

    module UniqueNames =
        let duplicateVarDefinition name = Fail [ sprintf "The input/reg/wire %A has already been declared. The name cannot be re-used for another component." name ]
        let duplicateModInstDefinition name = Fail [ sprintf "The module instance %A has already been declared. The name cannot be re-used for another component." name ]

    module ProcessInitial =
        let regDoesNotExist name = Fail [ sprintf "The reg %A has not been declared and, therefore, cannot be initialised." name ]
        let shouldBeReg name = Fail [ sprintf "%A is not a reg. Only regs can be initialised." name ]
