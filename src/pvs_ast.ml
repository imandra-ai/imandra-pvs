(* Imandra<->PVS: PVS AST represented in Imandra *)

type typeref = int

type type_actual = {
  type_ : typeref
}

type variable = {
  variable_name : string;
  type_ : typeref
}

type integer = {
  integer_value : int
}

type formal_constant = {
  constant_name : string
}

type expr =
  | Variable of variable
  | Constant of constant
  | FormalConstant of formal_constant
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

and constant = {
  actuals : actual list;
  constant_name : string;
  type_ : typeref
}

and actual =
  | TypeActual of type_actual
  | ConstActual of const_actual

and const_actual = {
  expr : expr
}
  
type const_decl = {
  name  : string;
  type_ : typeref;
  const_def : expr option;
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

type type_eq_decl = {
  name: string;
  type_: typeref
}

type declaration =
  | FormulaDecl of formula_decl
  | VarDecl of var_decl
  | ConstDecl of const_decl
  | TypeEqDecl of type_eq_decl

type formal_type_decl = { name : string }

type theory = {
  id : string;
  formals : formal_type_decl list;
  declarations : declaration list;
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

module F = CCFormat

let pp_var fmt v =
  F.string fmt v.variable_name

let rec pp_expr fmt e =
  match e with
  | Variable v -> pp_var fmt v
  | Constant c -> F.string fmt c.constant_name
  | FormalConstant c -> F.string fmt c.constant_name
  | Lambda l ->
    let bs = l.bindings in
    let e = l.expression in
    F.fprintf fmt "@[@[λ%a@].@[%a@]@]"
      F.(list pp_var) bs
      pp_expr e
  | Apply {operator; argument} ->
    F.fprintf fmt "@[%a@[(@[%a@])@]@]"
      pp_expr operator F.(list pp_expr) argument
  | Cases c ->
    F.fprintf fmt "(@[match @[%a@] with@\n@[| %a@]@])"
      pp_expr c.expr
      F.(list ~sep:(return "@\n| ") pp_selection) c.selections
  | Integer n -> F.fprintf fmt "%d" n.integer_value
  | If {test; then_; else_} ->
    F.fprintf fmt "(if %a then %a else %a)"
      pp_expr test pp_expr then_ pp_expr else_
  | Forall {bindings; expression} ->
    F.fprintf fmt "@[@[∀%a@](@[%a@])@]"
      F.(list pp_var) bindings pp_expr expression
  | Exists {bindings; expression} ->
    F.fprintf fmt "@[@[∃%a@](@[%a@])@]"
      F.(list pp_var) bindings pp_expr expression

and pp_selection fmt {pattern; expr} =
  F.fprintf fmt "@[@[%a@] ->@ @[%a@]@]"
    pp_pattern pattern
    pp_expr expr

and pp_pattern fmt {expr; variables} =
  F.fprintf fmt "@[%a(%a)@]"
    pp_expr expr
    F.(list pp_var) variables

let pp_formula_decl fmt (d:formula_decl) =
  F.fprintf fmt "@[Formula %s (%s) =@\n@ @[%a@]@]"
    d.id
    d.label
    F.(list pp_expr) d.definition

let pp_const_decl fmt (d:const_decl) =
  F.fprintf fmt "@[Const %s : %d@ =@\n@ @[%a@]@]"
    d.name
    d.type_
    (F.opt pp_expr) d.const_def 

let pp_var_decl fmt (d:var_decl) =
  F.fprintf fmt "@[Var %s : %d@]"
    d.id d.type_

let pp_type_eq_decl fmt (d:type_eq_decl) =
  F.fprintf fmt "@[TypeEq %s : %d@]"
    d.name d.type_

let pp_decl fmt d =
  match d with
  | FormulaDecl d ->
    pp_formula_decl fmt d
  | VarDecl d ->
    pp_var_decl fmt d
  | ConstDecl d ->
    pp_const_decl fmt d
  | TypeEqDecl d ->
    pp_type_eq_decl fmt d
  

let pp_theory fmt (t:theory) =
  F.fprintf fmt "@[@{<Green>Theory@} %s@\n@ @[%a@]@]"
    t.id
    F.(list ~sep:(return "@\n") pp_decl) t.declarations

let pp_module fmt m =
  F.fprintf fmt "@[@{<Blue>Module@}@[@ %a @]@]@."
    F.(list ~sep:(return "@\n") pp_theory) m

let pp fmt m =
  pp_module fmt m.module_

let () = CCFormat.set_color_default true
