namespace Compiler

open Compiler.CompResult
open CommonTypes

module Errors =

    module General =
        let varDoesNotExist (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "The wire/reg %A has not been declared." name.value }]
    
    module CompilerAPI =
        let astNotProvided (name: string) =
            Fail
                [ WithPos<string>.empty <| sprintf "The AST for %A was not provided to the compiler." name ]

    module ProcessPorts =
        let portNotDeclaredInBody (name: WithPos<string>) = 
            Fail
                [{ name 
                    with value = sprintf "The port %A is in the module declaration but is never declared in the module body." name.value }]

        let portNotDeclaredInModDec (name: WithPos<string>) = 
            Fail
                [{ name 
                    with value = sprintf "The port %A is declared in the module body but is not present in the module declaration." name.value }]      

        let duplicatePorts (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "The port %A is declared multiple times. It can only be declared once." name.value }]

    module UniqueNames =
        let duplicateVarDefinition (name: WithPos<string>) = 
            Fail
                [{ name 
                    with value = sprintf "The input/reg/wire %A has already been declared. The name cannot be re-used for another component." name.value }]

        let duplicateModInstDefinition (name: WithPos<string>) = 
            Fail
                [{ name 
                    with value = sprintf "The module instance %A has already been declared. The name cannot be re-used for another component." name.value }]

    module ProcessInitial =
        let regDoesNotExist (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "The reg %A has not been declared and, therefore, cannot be initialised." name.value }]

        let shouldBeReg (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "%A is not a reg. Only regs can be initialised." name.value }]

    module NetLValueE =
        let shouldBeWire (name: WithPos<string>) = 
            Fail
                [{ name 
                    with value = sprintf "%A is not a wire. The left hand side of an assignment must be a wire." name.value }]

        let doesNotExist (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "The wire %A has not been declared and, therefore, cannot be assigned to." name.value }]

    module VarLValueE =
        let shouldBeReg (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "%A is not a reg. The left hand side of a variable assignment must be a reg." name.value }]

        let doesNotExist (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "The reg %A has not been declared and, therefore, cannot be assigned to." name.value }]
    
    module ProcessContAssign =
        let canOnlyDriveWire (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "Cannot drive input/reg. %A is not a wire and, therefore, cannot be driver" name.value }]

        let multiDrivenRanges (name: WithPos<string>) (r1: Range) (r2: Range) = 
            Fail
                [{ name 
                    with value = sprintf "There is an overlap of the ranges being driven in %A. Ranges %s and %s cannot be driven by different components as they overlap." name.value (r1.ToString()) (r2.ToString()) }]

    module ProcessModuleInstances =
        let notUniqueModule (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "Cannot instantiate %A as it is already instantiated." name.value }]

        let moduleDoesNotExist (name: WithPos<string>) =
            Fail
                [{ name 
                    with value = sprintf "Cannot find module %A. Is it included in the project?" name.value }]

        let cannotDriveExpression (loc: WithPos<'a>) (port: string) (modInst: string) = 
            Fail
                [{ file = loc.file
                   start = loc.start
                   finish = loc.finish 
                   value = sprintf "The expression given to output port %A of module instance %A cannot be driven. Only a reg or concatenation of regs can be driven." port modInst }]

        let portsDoNotMatch (loc: WithPos<'a>) (modInstName: string) numPorts numExps =
            Fail
                [{ file = loc.file
                   start = loc.start
                   finish = loc.finish 
                   value = sprintf "The module instance %A has not been provided the correct number of ports. It has %i ports but only %i were provided." modInstName numPorts numExps }]

        let namedPortsDoNotMatch (modInstName: string) (port: WithPos<string>) =
            Fail
                [{ port 
                    with value = sprintf "The module instance %A was provided with an input to port %A, but the module does not have a port by this name." modInstName port.value }]
