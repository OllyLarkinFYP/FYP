namespace Compiler

open Compiler.CompResult

module Errors =

    module General =
        let varDoesNotExist name = Fail [ sprintf "The wire/reg %A has not been declared." name ]
    
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

    module NetLValueE =
        let shouldBeWire name = Fail [ sprintf "%A is not a wire. The left hand side of an assignment must be a wire." name ]
        let doesNotExist name = Fail [ sprintf "The wire %A has not been declared and, therefore, cannot be assigned to." name ]

    module ProcessContAssign =
        let canOnlyDriveWire name = Fail [ sprintf "Cannot drive input/reg. %A is not a wire and, therefore, cannot be driver" name ]
        let multiDrivenRanges name r1 r2 = Fail [ sprintf "There is an overlap of the ranges being driven in %A. Ranges %s and %s cannot be driven by different components as they overlap." name (r1.ToString()) (r2.ToString()) ]

    module ProcessModuleInstances =
        let notUniqueModule name = Fail [ sprintf "Cannot instantiate %A as it is already instantiated." name ]
        let moduleDoesNotExist name = Fail [ sprintf "Cannot find module %A. Is it included in the project?" name ]
