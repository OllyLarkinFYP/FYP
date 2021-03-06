namespace Parser

open System
open FParsec
open AST
open Token
open Expression
open ConstantExpression
open CommonTypes
open Utils

module LangConstructs =

    // This acts as a forward declaration to allow the recursive grammar
    // Implementation can be found at the bottom of the page
    let pStatement, pStatementImpl : Parser<StatementT,UserState> * Parser<StatementT,UserState> ref = createParserForwardedToRef()

    let pStatementOrNull: Parser<StatementOrNullT,UserState> =
        choice [
            pStatement |>> Some
            Symbol.pSemiColon >>% None
        ]

    let pElseIf =
        // Else should backtrace so it can be used in the actual else body
        Keyword.pElse >>? Keyword.pIf >>. Symbol.pOpenRBrac >>. pExpression .>> Symbol.pCloseRBrac .>>. pStatementOrNull
        |>> function
        | cond, body -> {| Condition = cond; Body = body |}

    let pConditionalStatement: Parser<ConditionalStatementT,UserState> = 
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
        | ((cond, ifBody), elseIfList), (Some elseBody) -> 
            { ConditionalStatementT.Condition = cond; Body = ifBody; ElseIf = elseIfList; ElseBody = elseBody }
        | ((cond, ifBody), elseIfList), None -> 
            { ConditionalStatementT.Condition = cond; Body = ifBody; ElseIf = elseIfList; ElseBody = None }

    let pCaseItem: Parser<CaseItemT,UserState> = 
        choice [
            Keyword.pDefault >>. opt Symbol.pColon >>. pStatementOrNull |>> CaseItemT.Default
            sepBy1 pExpression Symbol.pComma .>>? Symbol.pColon .>>. pStatementOrNull |>> fun (exps, s) ->
                CaseItemT.Item {| Elems = exps; Body = s |}
        ]

    let pCaseStatement: Parser<CaseStatementT,UserState> = 
        Keyword.pCase >>. Symbol.pOpenRBrac >>. pExpression .>> Symbol.pCloseRBrac .>>. many1 pCaseItem .>> Keyword.pEndCase
        |>> function
        | expr, items -> { CaseStatementT.CaseExpr = expr; Items = items }

    let pRangedVar: Parser<RangedVarT,UserState> =
        withPos pIdentifier .>>. opt (Symbol.pOpenSBrac >>. pConstantRangeExpression .>> Symbol.pCloseSBrac) |>> fun (iden, range) ->
            { name = iden; range = range }

    let pEventExpression =
        choice [
            Keyword.pPosedge >>. pRangedVar |>> fun rv -> (Posedge, rv)
            Keyword.pNegedge >>. pRangedVar |>> fun rv -> (Negedge, rv)
            pRangedVar |>> fun rv -> (Neither, rv)
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
    let pNetLValue, pNetLValueImpl : Parser<NetLValueT,UserState> * Parser<NetLValueT,UserState> ref = createParserForwardedToRef()

    do pNetLValueImpl := 
        choice [
            Symbol.pOpenCBrac >>. sepBy1 pNetLValue Symbol.pComma .>> Symbol.pCloseCBrac |>> NetLValueT.Concat
            pRangedVar |>> NetLValueT.Ranged
        ]

    // This parser is self recursive so needs a forward reference
    // The implementation is defined underneath
    let pVarLValue, pVarLValueImpl : Parser<VarLValueT,UserState> * Parser<VarLValueT,UserState> ref = createParserForwardedToRef()

    do pVarLValueImpl := 
        choice [
            Symbol.pOpenCBrac >>. sepBy1 pVarLValue Symbol.pComma .>> Symbol.pCloseCBrac |>> VarLValueT.Concat
            pRangedVar |>> VarLValueT.Ranged
        ]

    let pBlockingAssignment =
        pVarLValue .>>? Symbol.pAssign .>>. pExpression
        |>> function
        | lval, exp -> { BlockingAssignmentT.LHS = lval; RHS = exp }

    let pNonBlockingAssignment =
        pVarLValue .>>? Symbol.pNonBlockAssign .>>. pExpression
        |>> function
        | lval, exp -> { NonblockingAssignmentT.LHS = lval; RHS = exp }

    let pSeqBlock: Parser<SeqBlockT,UserState> =
        Keyword.pBegin >>. many pStatement .>> Keyword.pEnd

    let pAlwaysConstruct: Parser<AlwaysConstructT,UserState> = 
        Keyword.pAlways >>. pProceduralTimingControlStatement

    let pInitialConstruct: Parser<InitialConstructT,UserState> = 
        let pRangedConstAssignment =
            pRangedVar .>>? Symbol.pAssign .>>. pConstantExpression
            |>> function
            | lval, exp -> { RangedConstAssignT.LHS = lval; RHS = exp }
        let initialBody =
            choice [
                pRangedConstAssignment .>> Symbol.pSemiColon |>> fun a -> [a]
                Keyword.pBegin >>. many (pRangedConstAssignment .>> Symbol.pSemiColon) .>> Keyword.pEnd
            ]
        Keyword.pInitial >>. initialBody

    let pNetAssignment =
        pNetLValue .>>? Symbol.pAssign .>>. pExpression
        |>> function
        | lval, exp -> { NetAssignmentT.LHS = lval; RHS = exp }

    let pContinuousAssign =
        Keyword.pAssign >>. sepBy1 pNetAssignment Symbol.pComma .>> Symbol.pSemiColon

    let pRange = 
        Symbol.pOpenSBrac >>. pConstantExpression .>> Symbol.pColon .>>. pConstantExpression .>> Symbol.pCloseSBrac
        |>> function
        | msb, lsb -> { RangeT.MSB = msb; LSB = lsb}

    let pListOfIdentifiers =
        sepBy1 (withPos pIdentifier) (attempt (Symbol.pComma .>> notFollowedBy (Keyword.pInput <|> Keyword.pOutput)))

    let pNetDeclaration =
        Keyword.pWire >>. opt pRange .>>. pListOfIdentifiers .>> Symbol.pSemiColon
        |>> function
        | range, idens -> 
            { names = idens
              range = range
              decType = Wire }

    let pRegDeclaration = 
        Keyword.pReg >>. opt pRange .>>. pListOfIdentifiers .>> Symbol.pSemiColon
        |>> function
        | range, idens -> 
            { names = idens
              range = range
              decType = Reg }

    let pModuleOrGenerateItemDeclaration =
        choice [
            pNetDeclaration
            pRegDeclaration
        ]

    let pInputDeclaration =
        Keyword.pInput >>. opt Keyword.pWire >>. opt pRange .>>. pListOfIdentifiers
        |>> function
        | range, idens -> 
            { names = idens
              range = range
              dir = Input }

    let pOutputDeclaration = 
        let regOrWire =
            choice [
                Keyword.pReg >>% Reg
                opt Keyword.pWire >>% Wire
            ]
        Keyword.pOutput >>. regOrWire .>>. opt pRange .>>. pListOfIdentifiers
        |>> function
        | (portType, range), idens -> 
            { names = idens
              range = range
              dir = Output portType }

    let pPortDeclaration =
        choice [
            pInputDeclaration
            pOutputDeclaration
        ]

    let pListOfPortConnections =
        let named =
            Symbol.pPeriod >>. withPos pIdentifier .>>. opt (Symbol.pOpenRBrac >>. opt (withPos pExpression) .>> Symbol.pCloseRBrac)
            |>> function
            | iden, None
            | iden, Some (None) -> {| Name = iden; Value = None |}
            | iden, Some (Some exp) -> {| Name = iden; Value = Some exp |}
        choice [
            sepBy1 named Symbol.pComma |>> PortConnectionT.Named
            sepBy (withPos pExpression) Symbol.pComma |>> PortConnectionT.Unnamed
        ]

    let pModuleInstance =
        withPos pIdentifier .>>? Symbol.pOpenRBrac .>>. pListOfPortConnections .>> Symbol.pCloseRBrac
        |>> function
        | iden, connections -> { ModuleInstanceT.Name = iden; PortConnections = connections }

    let pModuleInstantiation =
        withPos pIdentifier .>>.? pModuleInstance .>> Symbol.pSemiColon
        |>> function
        | iden, modInt -> { ModuleInstantiationT.Name = iden; Module = modInt }

    let pNonPortModuleItem =
        choice [
            pModuleOrGenerateItemDeclaration |>> NonPortModuleItemT.ModuleItemDeclaration
            pContinuousAssign |>> NonPortModuleItemT.ContinuousAssign
            pInitialConstruct |>> NonPortModuleItemT.InitialConstruct
            pAlwaysConstruct |>> NonPortModuleItemT.AlwaysConstruct
            withPos pModuleInstantiation |>> NonPortModuleItemT.ModuleInstantiation
        ]

    let pModuleItem =
        let op1 = pPortDeclaration .>> Symbol.pSemiColon |>> ModuleItemT.PortDeclaration
        let op2 = pNonPortModuleItem |>> ModuleItemT.NonPortModuleItem
        op1 <|> op2

    let pListOfPorts = 
        Symbol.pOpenRBrac >>. sepBy1 (withPos pIdentifier) Symbol.pComma .>> Symbol.pCloseRBrac

    let pListOfPortDeclarations =
        Symbol.pOpenRBrac >>. sepBy pPortDeclaration Symbol.pComma .>> Symbol.pCloseRBrac

    let pModuleDeclaration : Parser<ASTT,UserState> =
        // first keyword, identifier must be backtracable to catch second option
        let modDec1 = 
            Keyword.pModule >>? pIdentifier .>>.? attempt pListOfPorts .>> Symbol.pSemiColon .>>. many pModuleItem .>> Keyword.pEndModule
            |>> function
            | ((iden, ports), moduleItems) -> 
                { name = iden
                  info = ModDec1 {| ports = ports; body = moduleItems |}}
        let modDec2 =
            Keyword.pModule >>. pIdentifier .>>. pListOfPortDeclarations .>> Symbol.pSemiColon .>>. many pNonPortModuleItem .>> Keyword.pEndModule
            |>> function
            | ((iden, ports), moduleItems) -> 
                { name = iden
                  info = ModDec2 {| ports = ports; body = moduleItems |}}
        modDec1 <|> modDec2

    let pSourceText : Parser<ASTT,UserState> = 
        spaces >>. pModuleDeclaration .>> eof

    do pStatementImpl := 
        choice [
            pSeqBlock |>> StatementT.SeqBlock
            pConditionalStatement |>> StatementT.Conditional
            pCaseStatement |>> StatementT.Case
            pBlockingAssignment .>> Symbol.pSemiColon |>> StatementT.BlockingAssignment
            pNonBlockingAssignment .>> Symbol.pSemiColon |>> StatementT.NonblockingAssignment
        ]
