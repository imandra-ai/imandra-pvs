(* Imandra<->PVS: PVS AST represented in Imandra *)

type typeref = int

type type_actual = {
  type_ : typeref
}

type constant = {
  actuals : type_actual list;
  constant_name : string;
  type_ : typeref
}

type variable = {
  variable_name : string;
  type_ : typeref
}

type integer = {
  integer_value : int
}

type expr =
  | Variable of variable
  | Constant of constant
  | Lambda of lambda
  | Apply of apply
  | Cases of cases
  | If of if_
  | Integer of integer
  | Forall of bindings
  | Exists of bindings

and apply = {
  operator : expr;
  argument : expr list;
}

and lambda = {
  expression : expr;
  bindings : variable list;
}

and if_ = {
  test : expr;
  else_ : expr;
  then_ : expr
}

and cases = {
  selections : selection list;
  expr : expr;
  else_part : expr option;
}

and selection = {
  pattern : pattern;
  expr : expr;
}

and bindings = {
  expression : expr;
  bindings : variable list;
}

and pattern =
  { expr : expr
  ; variables : variable list
  }

type const_decl = {
  name  : string;
  type_ : typeref;
  const_def : expr;
}

type var_decl = {
  declared_type : typeref list;
  id : string;
  type_ : typeref;
}

type proof_info = {
  script : string;
  status : string;
}

type formula_decl = {
  label : string;
  definition : expr list;
  id : string;
  proof : proof_info
}

type declaration =
  | FormulaDecl of formula_decl
  | VarDecl of var_decl
  | ConstDecl of const_decl

type formal_type_decl = { name : string }

type theory = {
  declarations : declaration list;
  id : string;
  formals : formal_type_decl list;
  assuming : unit;
}

type subtype = {
  supertype : typeref;
  predicate : expr;
}

type functiontype = {
  domain : typeref;
  range : typeref;
}

type tupletype = { types : typeref list }

type typename = { id : string }
type dep_binding = { id : string ; type_ : typeref}

type typelist_entry =
  | SubType of subtype
  | FunctionType of functiontype
  | TupleType of tupletype
  | TypeName of typename
  | DepBinding of dep_binding

type typelist = (string, typelist_entry) Hashtbl.t

type module_with_hash = {
  module_ : theory list;
  type_hash : typelist;
}
