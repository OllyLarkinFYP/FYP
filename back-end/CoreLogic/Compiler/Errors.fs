namespace Compiler

open Compiler.CompResult
open CommonTypes

module Errors =

    module General =
        let varDoesNotExist (name: string) = Fail [ sprintf "The wire/reg %A has not been declared." name ]
    
    module CompilerAPI =
        let astNotProvided (name: string) = Fail [ sprintf "The AST for %A was not provided to the compiler." name ]

    module ProcessPorts =
        let portsDontMatchPortDecs ports decs = Fail [ sprintf "The ports declared in the module definition do not match the ports declared inside the module.\nThe module definition has ports %A, but the the ports declared inside the module are %A" ports decs ]
        let duplicatePorts (name: string) = Fail [ sprintf "The port %A is declared multiple times. It can only be declared once." name ]

    module UniqueNames =
        let duplicateVarDefinition (name: string) = Fail [ sprintf "The input/reg/wire %A has already been declared. The name cannot be re-used for another component." name ]
        let duplicateModInstDefinition (name: string) = Fail [ sprintf "The module instance %A has already been declared. The name cannot be re-used for another component." name ]

    module ProcessInitial =
        let regDoesNotExist (name: string) = Fail [ sprintf "The reg %A has not been declared and, therefore, cannot be initialised." name ]
        let shouldBeReg (name: string) = Fail [ sprintf "%A is not a reg. Only regs can be initialised." name ]

    module NetLValueE =
        let shouldBeWire (name: string) = Fail [ sprintf "%A is not a wire. The left hand side of an assignment must be a wire." name ]
        let doesNotExist (name: string) = Fail [ sprintf "The wire %A has not been declared and, therefore, cannot be assigned to." name ]

    module VarLValueE =
        let shouldBeReg (name: string) = Fail [ sprintf "%A is not a reg. The left hand side of a variable assignment must be a reg." name ]
        let doesNotExist (name: string) = Fail [ sprintf "The reg %A has not been declared and, therefore, cannot be assigned to." name ]
    
    module ProcessContAssign =
        let canOnlyDriveWire (name: string) = Fail [ sprintf "Cannot drive input/reg. %A is not a wire and, therefore, cannot be driver" name ]
        let multiDrivenRanges (name: string) (r1: Range) (r2: Range) = Fail [ sprintf "There is an overlap of the ranges being driven in %A. Ranges %s and %s cannot be driven by different components as they overlap." name (r1.ToString()) (r2.ToString()) ]

    module ProcessModuleInstances =
        let notUniqueModule (name: string) = Fail [ sprintf "Cannot instantiate %A as it is already instantiated." name ]
        let moduleDoesNotExist (name: string) = Fail [ sprintf "Cannot find module %A. Is it included in the project?" name ]
        let cannotDriveExpression (port: string) (modInst: string) = Fail [ sprintf "The expression given to output port %A of module instance %A cannot be driven. Only a reg on concatenation of regs can be driven." port modInst ]
        let portsDoNotMatch (modInstName: string) numPorts numExps = Fail [ sprintf "The module instance %A has not been provided the correct number of ports. It has %i ports but only %i were provided." modInstName numPorts numExps ]  
        let namedPortsDoNotMatch (modInstName: string) (port: string) = Fail [ sprintf "The module instance %A was provided with an input to port %A, but the module does not have a port by this name." modInstName port ]
