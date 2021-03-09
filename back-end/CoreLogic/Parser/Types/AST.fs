namespace rec AST


// ######### A.1.2 Verilog Source Text #########

type ASTT =
    | Empty
    | ModuleDeclaration of {| ModDec: ModuleDeclarationT; IsSystemVerilog: bool |}

type ModuleDeclarationT =
    | ModDec1 of {| Name: IdentifierT; Ports: PortT List; Body: ModuleItemT List |}
    | ModDec2 of {| Name: IdentifierT; Ports: PortDeclarationT List; Body: NonPortModuleItemT List |}


// ######### A.1.3 Module Parameters And Ports #########

type PortT = { Name: IdentifierT; Range: ConstantRangeExpressionT Option }

type PortDeclarationT = 
    | Input of InputDeclarationT
    | Output of OutputDeclarationT


// ######### A.1.4 Module Items #########

type ModuleItemT =
    | PortDeclaration of PortDeclarationT
    | NonPortModuleItem of NonPortModuleItemT

type NonPortModuleItemT =
    | ModuleItemDeclaration of ModuleOrGenerateItemDeclarationT
    | ContinuousAssign of ContinuousAssignT
    | ModuleInstantiation of ModuleInstantiationT
    | InitialConstruct of InitialConstructT
    | AlwaysConstruct of AlwaysConstructT

type ModuleOrGenerateItemDeclarationT =
    | NetDeclaration of NetDeclarationT
    | RegDeclaration of RegDeclarationT
    | LogicDeclaration of LogicDeclarationT


// ######### A.2.1.2 Port Declarations #########

type InputDeclarationT = 
    | WireDec of {| Range: RangeT; Names: IdentifierT List |}
    | LogicDec of {| Range: RangeT; Names: IdentifierT List |}

type OutputDeclarationT =
    | WireDec of {| Range: RangeT; Names: IdentifierT List |}
    | RegDec of {| Range: RangeT; Names: IdentifierT List |}
    | LogicDec of {| Range: RangeT; Names: IdentifierT List |}


// ######### A.2.1.3 Type Declaration #########

type NetDeclarationT = { Range: RangeT; Names: IdentifierT List }

type RegDeclarationT = { Range: RangeT; Names: IdentifierT List }

type LogicDeclarationT = { Range: RangeT; Names: IdentifierT List }


// ######### A.2.5 Declaration Ranges #########

type RangeT = { MSB: ConstantExpressionT; LSB: ConstantExpressionT }


// ######### A.4.1 Module Instantiation #########

type ModuleInstantiationT = { Name: IdentifierT; Module: ModuleInstanceT }

type ModuleInstanceT = { Name: IdentifierT; PortConnections: PortConnectionT List }

type PortConnectionT =
    | Unnamed of ExpressionT
    | Named of {| Name: IdentifierT; Value: ExpressionT |}


// ######### A.6.1 Continuous Assignment Statements #########

type ContinuousAssignT = NetAssignmentT List

type NetAssignmentT = { LHS: NetLValueT; RHS: ExpressionT }


// ######### A.6.2 Procedural Blocks and Assignments #########

type InitialConstructT = StatementT

type AlwaysConstructT = StatementT

type BlockingAssignmentT = { LHS: VariableLValueT; RHS: ExpressionT }

type NonblockingAssignmentT = { LHS: VariableLValueT; RHS: ExpressionT }


// ######### A.6.3 Parallel and Sequential Blocks #########

type SeqBlockT = StatementT List


// ######### A.6.4 Statements #########

type StatementT =
    | BlockingAssignment of BlockingAssignmentT
    | Case of CaseStatementT
    | Conditional of ConditionalStatementT
    | NonblockingAssignment of NonblockingAssignmentT
    | ProceduralTimingControl of ProceduralTimingControlStatementT
    | SeqBlock of SeqBlockT

type StatementOrNullT = StatementT Option


// ######### A.6.5 Timing Control Statements #########

type ProceduralTimingControlStatementT = { Control: EventControlT; Statement: StatementOrNullT }

/// Empty list means it is using '*'
type EventControlT = EventExpressionT List

type EventExpressionT = 
    | Nothing of ExpressionT
    | Posedge of ExpressionT
    | Negedge of ExpressionT


// ######### A.6.6 Conditional Statements #########

type ConditionalStatementT =
    | IfElseIf of IfElseIfStatementT
    | IfElse of {| Condition: ExpressionT; Body: StatementOrNullT; ElseBody: StatementOrNullT Option |}

type IfElseIfStatementT = {
    Condition: ExpressionT
    ElseIf: {| Condition: ExpressionT; Body: StatementOrNullT |} List
    ElseBody: StatementOrNullT Option
}


// ######### A.6.7 Case Statements #########

/// Items should always contain at least 1 item
type CaseStatementT = { CaseExpr: ExpressionT; Items: CaseItemT List }

type CaseItemT =
    | Item of {| Elems: ExpressionT List; Body: StatementOrNullT |}
    | Default of StatementOrNullT


// ######### A.8.1 Concatenations #########

type ConcatenationT = ExpressionT List

type ConstantConcatenationT = ConstantExpressionT List


// ######### A.8.3 Expressions #########

type ConstantRangeExpressionT =
    | Expr of ConstantExpressionT
    | ConstRange of {| LHS: ConstantExpressionT; RHS: ConstantExpressionT |}

type ConstantExpressionT =
    | Primary of ConstantPrimaryT
    | UniPrimary of {| Operator: UnaryOperatorT; Primary: ConstantPrimaryT |}
    | BinaryExpression of {| LHS: ConstantExpressionT; BinOperator: BinaryOperatorT; RHS: ConstantExpressionT |}
    | CondExpression of {| Condition: ConstantExpressionT; TrueVal: ConstantExpressionT; FalseVal: ConstantExpressionT |}

type RangeExpressionT =
    | SingleVal of ExpressionT
    | DoubleVal of {| LHS: ExpressionT; RHS: ExpressionT |}

type ConditionalExpressionT = { Condition: ExpressionT; TrueVal: ExpressionT; FalseVal: ExpressionT }

type ExpressionT =
    | Primary of PrimaryT
    | UniPrimary of {| Operator: UnaryOperatorT; Primary: PrimaryT |}
    | BinaryExpression of {| LHS: ExpressionT; BinOperator: BinaryOperatorT; RHS: ExpressionT |}
    | CondExpression of {| Condition: ExpressionT; TrueVal: ExpressionT; FalseVal: ExpressionT |}


// ######### A.8.4 Primaries #########

type ConstantPrimaryT =
    | Number of NumberT
    | Concat of ConstantConcatenationT
    | Brackets of ConstantExpressionT

type PrimaryT =
    | Number of NumberT
    | Ranged of {| Name: IdentifierT; Range: RangeExpressionT |}
    | Concat of ConcatenationT
    | Brackets of ExpressionT


// ######### A.8.5 Expression Left-Side Values #########

type NetLValueT =
    | Ranged of {| Name: IdentifierT; Range: ConstantRangeExpressionT Option |}
    | Concat of NetLValueT List

type VariableLValueT =
    | Ranged of {| Name: IdentifierT; Range: RangeExpressionT Option |}
    | Concat of VariableLValueT


// ######### A.8.6 Operators #########

type UnaryOperatorT =
    | Plus
    | Minus
    | LogicalNegation
    | BitwiseNegation
    | ReductionAnd
    | ReductionNand
    | ReductionOr
    | ReductionNor
    | ReductionXor
    | ReductionXnor

type BinaryOperatorT =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Modulus
    | LogicalEquality
    | LogicalInequality
    | CaseEquality
    | CaseInequality
    | LogicalAnd
    | LogicalOr
    | Power
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | BitwiseEquivalence
    | LogicalRightShift
    | LogicalLeftShift
    | ArithmaticRightShift
    | ArithmaticLeftShift


// ######### A.8.7 Numbers #########

type NumberT = { Size: uint; Value: uint32; UnknownBits: uint List; Signed: bool }


// ######### A.9.3 Identifiers ######

type IdentifierT = string
