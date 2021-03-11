namespace Parser

open System
open FParsec
open AST
open Token
open Expression
open ConstantExpression

module LangConstructs =

    // This acts as a forward declaration to allow the recursive grammar
    // Implementation can be found at the bottom of the page
    let pStatement, pStatementImpl : Parser<StatementT,unit> * Parser<StatementT,unit> ref = createParserForwardedToRef()

    let pStatementOrNull: Parser<StatementOrNullT,unit> =
        choice [
            pStatement |>> Some
            Symbol.pSemiColon >>% None
        ]

    let pElseIf =
        // Else should backtrace so it can be used in the actual else body
        Keyword.pElse >>? Keyword.pIf >>. Symbol.pOpenRBrac >>. pExpression .>> Symbol.pCloseRBrac .>>. pStatementOrNull
        |>> function
        | cond, body -> {| Condition = cond; Body = body |}

    let pConditionalStatement: Parser<ConditionalStatementT,unit> = 
        Keyword.pIf 
        >>. Symbol.pOpenRBrac 
        >>. pExpression 
        .>> Symbol.pCloseRBrac 
        .>>. pStatementOrNull
        .>>. many pElseIf
        .>>. opt (
            Keyword.pElse
            >>. pStatementOrNull
        ) |>> function
        | ((cond, ifBody), elseIfList), elseBody -> 
            { ConditionalStatementT.Condition = cond; Body = ifBody; ElseIf = elseIfList; ElseBody = elseBody }

    let pCaseItem: Parser<CaseItemT,unit> = 
        choice [
            Keyword.pDefault >>. opt Symbol.pColon >>. pStatementOrNull |>> CaseItemT.Default
            sepBy1 pExpression Symbol.pComma .>> Symbol.pColon .>>. pStatementOrNull |>> fun (exps, s) ->
                CaseItemT.Item {| Elems = exps; Body = s |}
        ]

    let pCaseStatement: Parser<CaseStatementT,unit> = 
        Keyword.pCase >>. Symbol.pOpenRBrac >>. pExpression .>> Symbol.pCloseRBrac .>>. many1 pCaseItem .>> Keyword.pEndCase
        |>> function
        | expr, items -> { CaseStatementT.CaseExpr = expr; Items = items }

    let pEventExpression =
        choice [
            Keyword.pPosedge >>. pExpression |>> EventExpressionT.Posedge
            Keyword.pNegedge >>. pExpression |>> EventExpressionT.Negedge
        ]

    let pEventExpressionList =
        sepBy pEventExpression (Symbol.pComma <|> Keyword.pOr)

    let pEventControl = 
        choice [
            Symbol.pAt >>? Symbol.pOpenRBrac >>? Symbol.pStar .>> Symbol.pCloseRBrac >>% EventControlT.Star
            Symbol.pAt >>? Symbol.pStar >>% EventControlT.Star
            Symbol.pAt >>. Symbol.pOpenRBrac >>. pEventExpressionList .>> Symbol.pCloseRBrac |>> EventControlT.EventList
        ]

    let pProceduralTimingControlStatement =
        pEventControl .>>. pStatementOrNull
        |>> function
        | eControl, statement -> { ProceduralTimingControlStatementT.Control = eControl; Statement = statement }

    // This parser is self recursive so needs a forward reference
    // The implementation is defined underneath
    let pVariableLValue, pVariableLValueImpl : Parser<VariableLValueT,unit> * Parser<VariableLValueT,unit> ref = createParserForwardedToRef()

    do pVariableLValueImpl := 
        choice [
            Symbol.pOpenCBrac >>. sepBy1 pVariableLValue Symbol.pComma .>> Symbol.pCloseCBrac |>> VariableLValueT.Concat
            pIdentifier .>>. opt (Symbol.pOpenSBrac >>. pRangeExpression .>> Symbol.pCloseSBrac) |>> fun (iden, range) ->
                VariableLValueT.Ranged {| Name = iden; Range = range |}
        ]

    // This parser is self recursive so needs a forward reference
    // The implementation is defined underneath
    let pNetLValue, pNetLValueImpl : Parser<NetLValueT,unit> * Parser<NetLValueT,unit> ref = createParserForwardedToRef()

    do pNetLValueImpl := 
        choice [
            Symbol.pOpenCBrac >>. sepBy1 pNetLValue Symbol.pComma .>> Symbol.pCloseCBrac |>> NetLValueT.Concat
            pIdentifier .>>. opt (Symbol.pOpenSBrac >>. pConstantRangeExpression .>> Symbol.pCloseSBrac) |>> fun (iden, range) ->
                NetLValueT.Ranged {| Name = iden; Range = range |}
        ]

    let pBlockingAssignment =
        pVariableLValue .>>? Symbol.pAssign .>>. pExpression
        |>> function
        | lval, exp -> { BlockingAssignmentT.LHS = lval; RHS = exp }

    let pNonBlockingAssignment =
        pVariableLValue .>>? Symbol.pNonBlockAssign .>>. pExpression
        |>> function
        | lval, exp -> { NonblockingAssignmentT.LHS = lval; RHS = exp }

    let pSeqBlock: Parser<SeqBlockT,unit> =
        Keyword.pBegin >>. many pStatement .>> Keyword.pEnd

    let pAlwaysConstruct: Parser<AlwaysConstructT,unit> = 
        Keyword.pAlways >>. pStatement

    let pInitialConstruct: Parser<InitialConstructT,unit> = 
        Keyword.pInitial >>. pStatement

    let pNetAssignment =
        pNetLValue .>>? Symbol.pAssign .>>. pExpression
        |>> function
        | lval, exp -> { NetAssignmentT.LHS = lval; RHS = exp }

    let pContinuousAssign =
        Keyword.pAssign >>. sepBy1 pNetAssignment Symbol.pComma .>> Symbol.pSemiColon

    let pRange = 
        Symbol.pOpenSBrac >>. pConstantExpression .>> Symbol.pSemiColon .>>. pConstantExpression .>> Symbol.pCloseSBrac
        |>> function
        | msb, lsb -> { RangeT.MSB = msb; LSB = lsb}

    let pListOfIdentifiers =
        sepBy1 pIdentifier Symbol.pComma

    let pNetDeclaration =
        Keyword.pWire >>. opt Keyword.pSigned .>>. opt pRange .>>. pListOfIdentifiers .>> Symbol.pSemiColon
        |>> function
        | (signed, range), idens -> { NetDeclarationT.Range = range; Signed = Option.isSome signed; Names = idens }

    let pRegDeclaration = 
        Keyword.pReg >>. opt Keyword.pSigned .>>. opt pRange .>>. pListOfIdentifiers .>> Symbol.pSemiColon
        |>> function
        | (signed, range), idens -> { RegDeclarationT.Range = range; Signed = Option.isSome signed; Names = idens }

    let pLogicDeclaration = 
        Keyword.pLogic >>. opt Keyword.pSigned .>>. opt pRange .>>. pListOfIdentifiers .>> Symbol.pSemiColon
        |>> function
        | (signed, range), idens -> { LogicDeclarationT.Range = range; Signed = Option.isSome signed; Names = idens }

    let pModuleOrGenerateItemDeclaration =
        choice [
            pNetDeclaration |>> ModuleOrGenerateItemDeclarationT.NetDeclaration
            pRegDeclaration |>> ModuleOrGenerateItemDeclarationT.RegDeclaration
            pLogicDeclaration |>> ModuleOrGenerateItemDeclarationT.LogicDeclaration
        ]
       
    type private pInputDeclarationIntermediate =
        | Logic
        | Wire

    let pInputDeclaration =
        let logicOrWire =
            choice [
                Keyword.pLogic >>% pInputDeclarationIntermediate.Logic
                opt Keyword.pWire >>% pInputDeclarationIntermediate.Wire
            ]
        Keyword.pInput >>. logicOrWire .>>. opt Keyword.pSigned .>>. opt pRange .>>. pListOfIdentifiers
        |>> function
        | ((pInputDeclarationIntermediate.Logic, signed), range), idens -> 
            InputDeclarationT.LogicDec {| Range = range; Signed = Option.isSome signed; Names = idens |}
        | ((pInputDeclarationIntermediate.Wire, signed), range), idens -> 
            InputDeclarationT.WireDec {| Range = range; Signed = Option.isSome signed; Names = idens |}
       
    type private pOutputDeclarationIntermediate =
        | Logic
        | Reg
        | Wire

    let pOutputDeclaration = 
        let logicOrWire =
            choice [
                Keyword.pLogic >>% pOutputDeclarationIntermediate.Logic
                Keyword.pReg >>% pOutputDeclarationIntermediate.Reg
                opt Keyword.pWire >>% pOutputDeclarationIntermediate.Wire
            ]
        Keyword.pInput >>. logicOrWire .>>. opt Keyword.pSigned .>>. opt pRange .>>. pListOfIdentifiers
        |>> function
        | ((pOutputDeclarationIntermediate.Logic, signed), range), idens -> 
            OutputDeclarationT.LogicDec {| Range = range; Signed = Option.isSome signed; Names = idens |}
        | ((pOutputDeclarationIntermediate.Reg, signed), range), idens -> 
            OutputDeclarationT.RegDec {| Range = range; Signed = Option.isSome signed; Names = idens |}
        | ((pOutputDeclarationIntermediate.Wire, signed), range), idens -> 
            OutputDeclarationT.WireDec {| Range = range; Signed = Option.isSome signed; Names = idens |}

    let pPortDeclaration =
        choice [
            pInputDeclaration |>> PortDeclarationT.Input
            pOutputDeclaration |>> PortDeclarationT.Output
        ]

    let pPort =
        pIdentifier .>>. opt (Symbol.pOpenSBrac >>. pConstantRangeExpression .>> Symbol.pCloseSBrac)
        |>> function
        | (iden, opRange) -> { PortT.Name = iden; Range = opRange }

    let pListOfPortConnections =
        let named =
            Symbol.pPeriod >>. pIdentifier .>>. opt (Symbol.pOpenRBrac >>. opt pExpression .>> Symbol.pCloseRBrac)
            |>> function
            | iden, None
            | iden, Some (None) -> PortConnectionT.Named {| Name = iden; Value = None |}
            | iden, Some (Some exp) -> PortConnectionT.Named {| Name = iden; Value = Some exp |}
        choice [
            sepBy1 named Symbol.pComma
            sepBy pExpression Symbol.pComma |>> List.map PortConnectionT.Unnamed
        ]

    let pModuleInstance =
        pIdentifier .>>? Symbol.pOpenRBrac .>>. pListOfPortConnections .>> Symbol.pCloseRBrac
        |>> function
        | iden, connections -> { ModuleInstanceT.Name = iden; PortConnections = connections }

    let pModuleInstantiation =
        pIdentifier .>>.? pModuleInstance .>> Symbol.pSemiColon
        |>> function
        | iden, modInt -> { ModuleInstantiationT.Name = iden; Module = modInt }

    let pNonPortModuleItem =
        choice [
            pModuleOrGenerateItemDeclaration |>> NonPortModuleItemT.ModuleItemDeclaration
            pContinuousAssign |>> NonPortModuleItemT.ContinuousAssign
            pInitialConstruct |>> NonPortModuleItemT.InitialConstruct
            pAlwaysConstruct |>> NonPortModuleItemT.AlwaysConstruct
            pModuleInstantiation |>> NonPortModuleItemT.ModuleInstantiation
        ]

    let pModuleItem =
        let op1 = pPortDeclaration .>> Symbol.pSemiColon |>> ModuleItemT.PortDeclaration
        let op2 = pNonPortModuleItem |>> ModuleItemT.NonPortModuleItem
        op1 <|> op2

    let pListOfPorts = 
        Symbol.pOpenRBrac >>. sepBy1 pPort Symbol.pComma .>> Symbol.pCloseRBrac

    let pListOfPortDeclarations =
        Symbol.pOpenRBrac >>. sepBy pPortDeclaration Symbol.pComma .>> Symbol.pCloseRBrac

    let pModuleDeclaration =
        // first keyword, identifier must be backtracable to catch second option
        let modDec1 = 
            Keyword.pModule >>? pIdentifier .>>.? pListOfPorts .>> Symbol.pSemiColon .>>. many pModuleItem .>> Keyword.pEndModule
            |>> function
            | ((iden, ports), moduleItems) -> ModDec1 {| Name = iden; Ports = ports; Body = moduleItems |}
        let modDec2 =
            Keyword.pModule >>. pIdentifier .>>. pListOfPortDeclarations .>> Symbol.pSemiColon .>>. many pNonPortModuleItem .>> Keyword.pEndModule
            |>> function
            | ((iden, ports), moduleItems) -> ModDec2 {| Name = iden; Ports = ports; Body = moduleItems |}
        modDec1 <|> modDec2

    let pSourceText isSystemVerilog = 
        spaces >>. opt pModuleDeclaration .>> eof
        |>> function
        | Some m -> ModuleDeclaration {| ModDec = m; IsSystemVerilog = isSystemVerilog |}
        | None -> Empty

    do pStatementImpl := 
        choice [
            pSeqBlock |>> StatementT.SeqBlock
            pProceduralTimingControlStatement |>> StatementT.ProceduralTimingControl
            pConditionalStatement |>> StatementT.Conditional
            pCaseStatement |>> StatementT.Case
            pBlockingAssignment |>> StatementT.BlockingAssignment
            pNonBlockingAssignment |>> StatementT.NonblockingAssignment
        ]
