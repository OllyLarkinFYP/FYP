namespace rec AST

open CommonTypes


// ######### A.1.2 Verilog Source Text #########

type ModuleDeclarationInfo =
    | ModDec1 of {| ports: IdentifierT List; body: ModuleItemT List |}
    | ModDec2 of {| ports: PortDeclarationT List; body: NonPortModuleItemT List |}

type ASTT =
    { name: IdentifierT
      info: ModuleDeclarationInfo }


// ######### A.1.3 Module Parameters And Ports #########

type PortDeclarationT = 
    { names: IdentifierT list
      range: RangeT option
      dir: PortDirAndType }


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
    { names: IdentifierT list
      range: RangeT option
      decType: PortType }


// ######### A.2.5 Declaration Ranges #########

type RangeT = { MSB: ConstantExpressionT; LSB: ConstantExpressionT }


// ######### A.4.1 Module Instantiation #########

type ModuleInstantiationT = { Name: IdentifierT; Module: ModuleInstanceT }

type ModuleInstanceT = { Name: IdentifierT; PortConnections: PortConnectionT }

type PortConnectionT =
    | Unnamed of ExpressionT list
    | Named of {| Name: IdentifierT; Value: ExpressionT option |} list


// ######### A.6.1 Continuous Assignment Statements #########

type ContinuousAssignT = NetAssignmentT List

type NetAssignmentT = { LHS: NetLValueT; RHS: ExpressionT }


// ######### A.6.2 Procedural Blocks and Assignments #########

type RangedConstAssignT = { LHS: RangedVarT; RHS: ConstantExpressionT }

type InitialConstructT = RangedConstAssignT list

type AlwaysConstructT = ProceduralTimingControlStatementT

type BlockingAssignmentT = { LHS: VarLValueT; RHS: ExpressionT }

type NonblockingAssignmentT = { LHS: VarLValueT; RHS: ExpressionT }


// ######### A.6.3 Parallel and Sequential Blocks #########

type SeqBlockT = StatementT List


// ######### A.6.4 Statements #########

type StatementT =
    | BlockingAssignment of BlockingAssignmentT
    | Case of CaseStatementT
    | Conditional of ConditionalStatementT
    | NonblockingAssignment of NonblockingAssignmentT
    | SeqBlock of SeqBlockT

type StatementOrNullT = StatementT Option


// ######### A.6.5 Timing Control Statements #########

type ProceduralTimingControlStatementT = { Control: EventControlT; Statement: StatementOrNullT }

/// Empty list means it is using '*'
type EventControlT = 
    | EventList of (EventControlType * RangedVarT) List
    | Star

type EventControlType =
    | Posedge
    | Negedge
    | Neither


// ######### A.6.6 Conditional Statements #########

type ConditionalStatementT = {
    Condition: ExpressionT
    Body: StatementOrNullT
    ElseIf: {| Condition: ExpressionT; Body: StatementOrNullT |} List
    ElseBody: StatementOrNullT
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

type ConstantExpressionT =
    | Primary of ConstantPrimaryT
    | UniExpression of {| Operator: UnaryOperatorT; Expression: ConstantExpressionT |}
    | BinaryExpression of {| LHS: ConstantExpressionT; BinOperator: BinaryOperatorT; RHS: ConstantExpressionT |}
    | CondExpression of {| Condition: ConstantExpressionT; TrueVal: ConstantExpressionT; FalseVal: ConstantExpressionT |}

type ExpressionT =
    | Primary of PrimaryT
    | UniExpression of {| Operator: UnaryOperatorT; Expression: ExpressionT |}
    | BinaryExpression of {| LHS: ExpressionT; BinOperator: BinaryOperatorT; RHS: ExpressionT |}
    | CondExpression of {| Condition: ExpressionT; TrueVal: ExpressionT; FalseVal: ExpressionT |}
    

// ######### A.8.4 Primaries #########

type ConstantPrimaryT =
    | Number of VNum
    | Concat of ConstantConcatenationT
    | Brackets of ConstantExpressionT

type PrimaryT =
    | Number of VNum
    | Ranged of RangedVarT
    | Concat of ConcatenationT
    | Brackets of ExpressionT


// ######### A.8.5 Expression Left-Side Values #########

type RangedVarT = 
    { name: IdentifierT
      range: RangeT option }

type VarLValueT =
    | Ranged of RangedVarT
    | Concat of VarLValueT list
    
type NetLValueT =
    | Ranged of RangedVarT
    | Concat of NetLValueT List
    with
        member this.getNames () =
            match this with
            | Ranged a -> [ a.name ]
            | Concat c -> List.collect (fun (a: NetLValueT) -> a.getNames()) c


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
    | BitwiseXnor
    | LogicalRightShift
    | LogicalLeftShift
    | ArithmeticRightShift
    | ArithmeticLeftShift
