(* C17 Abstract Syntax Tree in OCaml *)
(* Based on ISO/IEC Standard 9899:2017 *)
(* Annex A (informative) Section A2  *)
(* Author: Chubak Bidpaa *)
(* Part of: Cholipa C Compiler *)
(* License: MIT *)

type identifier = string
type integer = int
type rational = float

(* A.2.1: Expressions *)
type primary_expression = 
          IdentifierExpr of identifier
        | ConstantExpr of constant
        | StringLiteral of string
        | ExpressionList of expression list
        | GenericSelection of assignment_expression * generic_association list
and postfix_expression =
          PrimaryPostfix of primary_expression
        | ArrayAccess of postfix_expression * expression
        | FunctionCall of postfix_expression * argument_expression list option
        | MemberAccess of postfix_expression * identifier
        | RefMemberAccess of postfix_expression * identifier
        | PostFixIncrement of postfix_expression
        | PostFixSub of postfix_expression
        | TypedInitialize of type_name * initializer_literal list
and unary_expression =
          PostfixExprUnary of postfix_expression
        | PrefixUnary of unary_expression
        | PrefixSub of unary_expression
        | CastExpr of unary_operator * cast_expression
        | SizeofExpr of unary_expression
        | SizeofType of unary_expression * type_name
        | AlignOfType of type_name
and unary_operator =
          AndUnary
        | AsteriskUnary
        | PlusUnary
        | MinusUnary
        | TildeUnary
        | ExcPointUnary
and cast_expression =
          UnaryCast of unary_expression
          TypedCast of type_name * cast_expression
and multiplicative_expression =
          MultiplicativeCast of cast_expression
        | Times of multiplicative_expression * cast_expression
        | Divide of multiplicative_expression * cast_expression
        | Remainder of multiplicative_expression * cast_expression
and additive_expression =
          MultiplicativeAdditive of multiplicative_expression
        | Subtract of additive_expression * multiplicative_expression
        | Add of additive_expression * multiplicative_expression
and shift_expression = 
          AdditiveShift of shift_expression * additive_expression
        | ShiftLeft of shift_expression * additive_expression
and relational_expression =
          ShiftRelational of shift_expression
        | GreaterThan of relational_expression * shift_expression
        | LessThan of relational_expression * shift_expression
        | GreaterThanEqual of relational_expression * shift_expression
        | LessThanEqual of relational_expression * shift_expression
and equality_expression =
          relational_expression
        | EqualityCheck of equality_expression * relational_expression
        | UnequalityCheck of equality_expression * relational_expression
and and_expression =
          AndEqualityExpr of equality_expression
        | AndExpr of and_expression * equality_expression
and xor_expression =
          XorAndExpr of and_expression
        | XORExpr of xor_expression * and_expression
and or_expression =
          OrXorExpr of xor_expression
        | OrExpr of or_expression * xor_expression
and logical_and_exppression =
          OrLogicalAndExpr of or_expression
        | LogicalAndExpr of logical_and_expr * or_expression
and logical_or_exppression =
          LogicalOrLogicalAndExpr of logical_and_expression
        | LogicalOrExpr of logical_or_expr * logical_and_expression


