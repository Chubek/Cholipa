type identifier = string

type binary_operator =
  | Multiply | Divide | Modulo
  | Add | Subtract
  | LeftShift | RightShift
  | BitwiseAnd | BitwiseXor | BitwiseOr
  | Equal | NotEqual
  | LogicalAnd | LogicalOr
  | Assign | MultiplyAssign | DivideAssign | ModuloAssign
  | AddAssign | SubtractAssign | LeftShiftAssign | RightShiftAssign
  | BitwiseAndAssign | BitwiseXorAssign | BitwiseOrAssign

and unary_operator =
  | Increment | Decrement | Positive | Negative 
  | BitwiseNot | LogicalNot | AddressOf

and constant_expression =
  | Identifier of string
  | IntegerConstant of int
  | FloatConstant of float
  | StringConstant of string

and expression =
  | Identifier of string
  | Constant of constant
  | BinaryExpression of expression * binary_operator * expression
  | UnaryExpression of unary_operator * expression
  | CastExpression of type_name option * expression
  | PostfixExpression of expression * postfix_operator
  | FunctionCall of expression * expression list
  | ArrayAccess of expression * expression
  | StructMemberAccess of expression * string
  | TernaryExpression of expression * expression * expression
  | AssignmentExpression of expression * binary_operator option * expression
  | GenericSelection of expression * generic_association list

and generic_association =
  | TypeNameAssociation of type_name * expression
  | DefaultAssociation of expression

and argument_expression_list = expression list

and unary_expression =
  | UnaryExpression of unary_operator * expression
  | PostfixExpression of expression * postfix_operator

and cast_expression =
  | UnaryExpression of unary_expression
  | CastExpression of type_name * expression

and multiplicative_expression =
  | CastExpression of cast_expression
  | Multiply of multiplicative_expression * cast_expression
  | Divide of multiplicative_expression * cast_expression
  | Modulo of multiplicative_expression * cast_expression

and additive_expression =
  | MultiplicativeExpression of multiplicative_expression
  | Add of additive_expression * multiplicative_expression
  | Subtract of additive_expression * multiplicative_expression

and shift_expression =
  | AdditiveExpression of additive_expression
  | LeftShift of shift_expression * additive_expression
  | RightShift of shift_expression * additive_expression

and relational_expression =
  | ShiftExpression of shift_expression
  | Less of relational_expression * shift_expression
  | Greater of relational_expression * shift_expression
  | LessEqual of relational_expression * shift_expression
  | GreaterEqual of relational_expression * shift_expression

and equality_expression =
  | RelationalExpression of relational_expression
  | Equal of equality_expression * relational_expression
  | NotEqual of equality_expression * relational_expression

and logical_and_expression =
  | EqualityExpression of equality_expression
  | LogicalAnd of logical_and_expression * equality_expression

and logical_or_expression =
  | LogicalAndExpression of logical_and_expression
  | LogicalOr of logical_or_expression * logical_and_expression

and conditional_expression =
  | LogicalOrExpression of logical_or_expression
  | Conditional of logical_or_expression * expression * conditional_expression

and constant_expression = conditional_expression

and assignment_expression =
  | ConditionalExpression of conditional_expression
  | Assignment of expression * binary_operator * assignment_expression option

and integer_type =
  | SignedInt
  | UnsignedInt
  | SignedChar
  | UnsignedChar
  | SignedShort
  | UnsignedShort
  | SignedLong
  | UnsignedLong
  | SignedLongLong
  | UnsignedLongLong
  | UnspecifiedInt
  | UnspecifiedChar
  | UnspeciifiedShort
  | UnspecifiedLong
  | UnspecifiedlongLog

and rational_type =
  | Float
  | Double
  | LongDouble

and declaration_specifier =
  | VoidType
  | Bool of bool
  | IntType of integer_type
  | RationalType of rational_type
  | StructType of struct_or_union_specifier
  | UnionType of struct_or_union_specifier
  | EnumType of enum_body

and type_qualifier =
  | Const | Restrict | Volatile | Atomic

and storage_specifier =
  | Typedef
  | Extern
  | Static

and function_specifier =
  | Inline
  | Noreturn

and alignment_specifier =
  | AlignAs of type_name
  | AlignOf of type_name

and specifier_declarator =
  | SpecifierDeclarator of declaration_specifiers * declarator

and struct_declarator_list = struct_declarator list

and struct_declarator = declarator * constant_expression option

type enum_body =
  | EnumBody of enumerator_list

and enumerator_list = enumerator list

and enumerator =
  | EnumConstant of string
  | EnumConstantWithValue of string * constant_expression

and struct_declaration =
  | StructDeclaration of specifier_qualifier_list * struct_declarator_list option

and struct_or_union_specifier =
  | StructSpecifier of string option * struct_or_union_body option
  | UnionSpecifier of string option * struct_or_union_body option

and struct_or_union_body =
  | StructBody of struct_declaration list
  | UnionBody of struct_declaration list

and declaration_specifiers =
  | StorageClassSpecifier of storage_class_specifier * declaration_specifiers option
  | TypeSpecifier of type_specifier * declaration_specifiers option
  | TypeQualifier of type_qualifier * declaration_specifiers option
  | FunctionSpecifier of function_specifier * declaration_specifiers option
  | AlignmentSpecifier of alignment_specifier * declaration_specifiers option
  | CombinedSpecifiers of declaration_specifiers list

and pointer =
  | TypeQualifierList of type_qualifier list option
  | Pointer of type_qualifier list option * pointer

and constant_expression =
  | Identifier of identifier
  | IntegerConstant of int
  | FloatConstant of float
  | StringConstant of string

and designator =
  | ArrayIndex of expression
  | MemberAccess of identifier

and designator_list = designator list

and initializers =
  | AssignmentExpression of expression
  | InitializerList of (designator_list option * initializers) list

and typedef_name = identifier

and abstract_declarator =
  | PointerDeclarator of pointer option * direct_abstract_declarator
  | DirectAbstractDeclarator of direct_abstract_declarator

and direct_abstract_declarator =
  | Parenthesized of direct_abstract_declarator option
  | ArrayDeclarator of type_qualifier list option * expression option
  | FunctionDeclarator of parameter_type_list option

and parameter_type_list = parameter_list option

and parameter_list = parameter_declaration list

and parameter_declaration = declaration_specifiers * declarator

and declaration = declaration_specifiers * init_declarator_list option


and initializers =
  | AssignmentExpression of expression
  | InitializerList of (designator_list option * initializers) list

and direct_abstract_declarator =
  | Parenthesized of direct_abstract_declarator option
  | ArrayDeclarator of type_qualifier list option * expression option
  | FunctionDeclarator of parameter_type_list option

and parameter_type_list = parameter_list option

and parameter_list = parameter_declaration list

and parameter_declaration = declaration_specifiers * declarator

and struct_declaration = specifier_qualifier_list * struct_declarator_list option

and specifier_qualifier_list = type_specifier list * type_qualifier list

and struct_declarator_list = struct_declarator list

and struct_declarator = declarator * constant_expression option

and constant_expression = expression

and declaration = declaration_specifiers * init_declarator_list option

and declaration_list = declaration list

and init_declarator_list = init_declarator list

and init_declarator = declarator * initializers option

and declarator =
  | IdentifierDeclarator of string
  | PointerDeclarator of pointer option * direct_declarator

and statement =
  | LabeledStatement of identifier * statement
  | CaseStatement of constant_expression * statement
  | DefaultStatement of statement
  | CompoundStatement of block_item list
  | ExpressionStatement of expression option
  | IfStatement of expression * statement * statement option
  | SwitchStatement of expression * statement
  | WhileStatement of expression * statement
  | DoWhileStatement of statement * expression
  | ForStatement of expression option * expression option * expression option * statement
  | GotoStatement of identifier
  | ContinueStatement
  | BreakStatement
  | ReturnStatement of expression option

and block_item =
  | DeclarationItem of declaration
  | StatementItem of statement

and compound_statement = block_item list

and translation_unit =
  | ExternalDeclaration of external_declaration
  | TranslationUnit of external_declaration * translation_unit

and external_declaration =
  | FunctionDefinition of declaration_specifiers * declarator * declaration_list option * compound_statement
  | Declaration of declaration

and program = declaration_list

type pp_directive =
  | IncludeDirective of string
  | DefineDirective of string * pp_macro_body
  | UndefDirective of string
  | PragmaDirective of string

and pp_macro_definition =
  | ObjectMacro of string * macro_body
  | FunctionMacro of string * string list * macro_body

and pp_macro_body =
  | SimpleBody of string
  | ParameterizedBody of pp_macro_parameter list * string list * pp_macro_body list

and pp_macro_parameter = string

and pp_conditional =
  | IfDirective of expression
  | IfdefDirective of string
  | IfndefDirective of string
  | ElifDirective of expression
  | ElseDirective
  | EndifDirective

and pp_expression =
  | Identifier of string
  | Constant of constant
  | BinaryExpression of expression * binary_operator * expression
  | UnaryExpression of unary_operator * expression
  | DefinedExpression of string

and pp_binary_operator =
  | Plus | Minus | Multiply | Divide | Modulo
  | Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
  | LogicalAnd | LogicalOr

and pp_unary_operator =
  | Plus | Minus | LogicalNot | BitwiseNot

and pp_constant_expression =
  | PPIntegerConstant of int
  | PPStringLiteral of string
  | PPIdentifier of identifier

and c_preprocessor =
  | PPDirective of pp_directive
  | PPMacroDefinition of pp_macro_definition
  | PPConditional of pp_conditional
  | PPConstant of pp_constant

a
