(* Imandra<->PVS: PVS AST represented in Imandra *)

type typeref = string

type type_actual = {
  type_: typeref;
}

type variable = {
  variable_name: string;
  type_: typeref;
}

type integer = {
  integer_value: int;
}

type string_const = {
  string_value: string;
}

type formal_constant = {
  constant_name: string;
  theory: string;
}

type expr =
  | Variable of variable
  | Constant of constant
  | FormalConstant of formal_constant
  | Lambda of lambda
  | Let of let_
  | Apply of apply
  | Cases of cases
  | If of if_
  | Integer of integer
  | String of string_const
  | Update of update
  | Forall of bindings
  | Exists of bindings
  | Project of project
  | Tuple of tuple
  | Getfield of getfield

and getfield = {
  argument: expr;
  field: string;
}

and tuple = {
  exprs: expr list;
}

and project = {
  argument: expr;
  index: int;
}

and let_ = {
  bindings: let_binding list;
  body: expr;
}

and let_binding = {
  id: string;
  expr: expr;
}

and apply = {
  operator: expr;
  argument: expr list;
}

and lambda = {
  expression: expr;
  bindings: variable list;
}

and if_ = {
  test: expr;
  else_: expr;
  then_: expr;
}

and cases = {
  selections: selection list;
  expr: expr;
  else_part: expr option;
}

and selection = {
  pattern: pattern;
  expr: expr;
}

and bindings = {
  expression: expr;
  bindings: variable list;
}

and pattern =
  { expr: expr;
    variables: variable list;
  }

and constant = {
  actuals : actual list;
  id : string;
  theory: string;
  type_ : typeref
}

and actual =
  | TypeActual of type_actual
  | ConstActual of const_actual

and const_actual = {
  expr : expr
}

and update = {
  expr: expr;
  assignments: assignment list;
}

and assignment = {
  arguments: expr list;
  expr: expr;
}

type const_decl = {
  id  : string;
  type_ : typeref;
  const_def : expr option;
  theory : string;
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
  proof : proof_info option
}

type type_eq_decl = {
  name: string;
  type_: typeref
}

type type_decl = {
  name : string
}

type application_judgement = {
  id: string;
  declared_type: typeref list;
  type_: typeref list;
  name: expr;
  formals: expr list list;
  judgement_type: typeref list;
}

type subtype_judgement = {
  id: string;
  declared_type: typeref;
  type_: typeref;
  declared_subtype: typeref;
  subtype: typeref;
}

type conversion_decl = {
  id: string;
  expr: expr;
}

type declaration =
  | FormulaDecl of formula_decl
  | VarDecl of var_decl
  | ConstDecl of const_decl
  | TypeDecl of type_decl
  | TypeEqDecl of type_eq_decl
  | ConversionDecl of conversion_decl
  | ApplicationJudgement of application_judgement
  | SubtypeJudgement of subtype_judgement

type formal_type_decl = { id : string; theory : string }

type theory = {
  id: string;
  formals: formal_type_decl list;
  declarations: declaration list;
  assuming: unit;
}

type accessor = {
  id: string;
  declared_type : typeref list;
  type_: typeref list;
}

type constructor = {
  id: string;
  accessors: accessor list;
  recognizer: string;
  assuming: unit;
}

type datatype = {
  id: string;
  formals: (formal_type_decl list) option;
  constructors: constructor list;
  assuming: unit;
}

type subtype = {
  supertype: typeref option;
  predicate: expr option;
}

type functiontype = {
  domain: typeref;
  range: typeref;
}

type tupletype = {
  types: typeref list;
}

type typename = {
  id: string;
}

type dep_binding = {
  id: string;
  type_: typeref;
}

type record_type = {
  fields: field list;
}

and field = {
  id: string;
  type_: typeref;
}

type type_db_entry =
  | SubType of subtype
  | FunctionType of functiontype
  | TupleType of tupletype
  | TypeName of typename
  | DepBinding of dep_binding
  | RecordType of record_type

type type_db = (string, type_db_entry) Hashtbl.t

type module_entry =
  | Theory of theory
  | DataType of datatype

type module_with_hash = {
  module_: module_entry list;
  type_hash: type_db;
}

module F = CCFormat

let pp_var fmt v =
  F.string fmt v.variable_name

let rec pp_expr fmt e =
  match e with
  | Variable v -> pp_var fmt v
  | Constant c -> pp_const fmt c
  | FormalConstant c -> F.string fmt c.constant_name
  | Lambda l ->
    let bs = l.bindings in
    let e = l.expression in
    F.fprintf fmt "@[@[λ%a@].@[%a@]@]"
      F.(list pp_var) bs
      pp_expr e
  | Let {bindings; body} ->
    F.fprintf fmt "@[let (%a) in %a@]"
      (F.list pp_let_binding) bindings
      pp_expr body
  | Apply {operator; argument=[x;y]} when op_is_eq operator ->
    F.fprintf fmt "@[(%a@ =@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_or operator ->
    F.fprintf fmt "@[(%a@ ∨@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_and operator ->
    F.fprintf fmt "@[(%a@ ∧@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_implies operator ->
    F.fprintf fmt "@[(%a@ =>@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_iff operator ->
    F.fprintf fmt "@[(%a@ <=>@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_plus operator ->
    F.fprintf fmt "@[(%a@ +@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_times operator ->
    F.fprintf fmt "@[(%a@ *@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_minus operator ->
    F.fprintf fmt "@[(%a@ -@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_lt operator ->
    F.fprintf fmt "@[(%a@ <@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_gt operator ->
    F.fprintf fmt "@[(%a@ >@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_leq operator ->
    F.fprintf fmt "@[(%a@ <=@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_geq operator ->
    F.fprintf fmt "@[(%a@ >=@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument} ->
    F.fprintf fmt "@[%a@[(@[%a@])@]@]"
      pp_expr operator F.(list pp_expr) argument
  | Cases c ->
    F.fprintf fmt "(@[match @[%a@] with@\n@[| %a@]@])"
      pp_expr c.expr
      F.(list ~sep:(return "@\n| ") pp_selection) c.selections
  | Integer n -> F.fprintf fmt "%d" n.integer_value
  | String s -> F.fprintf fmt "%S" s.string_value
  | If {test; then_; else_} ->
    F.fprintf fmt "(if %a then %a else %a)"
      pp_expr test pp_expr then_ pp_expr else_
  | Update u ->
    F.fprintf fmt "@[%a WITH [%a]@]"
      pp_expr u.expr F.(list pp_assignment) u.assignments
  | Forall {bindings; expression} ->
    F.fprintf fmt "@[@[∀%a@](@[%a@])@]"
      F.(list pp_var) bindings pp_expr expression
  | Exists {bindings; expression} ->
    F.fprintf fmt "@[@[∃%a@](@[%a@])@]"
      F.(list pp_var) bindings pp_expr expression
  | Project {argument; index} ->
    F.fprintf fmt "@[Proj_{%d}(%a)@]"
      index pp_expr argument
  | Tuple {exprs} ->
    F.fprintf fmt "@[Tuple(%a)@]"
      F.(list pp_expr) exprs
  | Getfield {argument; field} ->
    F.fprintf fmt "@[Getfield(%a, %s)@]"
      pp_expr argument
      field

and pp_type_db_entry fmt (t:type_db_entry) =
  match t with
  | SubType s ->
    F.fprintf fmt "@[{x:%a | %a x}@]"
      (F.opt pp_typeref) s.supertype
      (F.opt pp_expr) s.predicate
  | FunctionType f ->
    F.fprintf fmt "@[[%a -> %a]@]"
      pp_typeref f.domain pp_typeref f.range
  | TupleType t ->
    F.fprintf fmt "@[(%a)@]"
      F.(list ~sep:(return " * ") pp_typeref) t.types
  | TypeName t ->
    F.string fmt t.id
  | DepBinding _ ->
    F.fprintf fmt "<DepBinding>"
  | RecordType _ ->
    F.fprintf fmt "<RecordType>"

and pp_const fmt c =
  match c.actuals with
  | (ConstActual _ :: _) as xs ->
    F.fprintf fmt "@[%s[%a]@]"
      c.id F.(list pp_actual) xs
  | _ ->
    F.string fmt c.id

and pp_actual fmt a =
  match a with
  | ConstActual c ->
    pp_expr fmt c.expr
  | TypeActual _t ->
    F.string fmt ""
    (* F.string fmt t.type_ *)

and pp_assignment fmt (a:assignment) =
  F.fprintf fmt "@[(%a) := %a@]"
    F.(list pp_expr) a.arguments
    pp_expr a.expr

and pp_let_binding fmt (b:let_binding) =
  F.fprintf fmt "@[(%s = %a)@]"
    b.id
    pp_expr b.expr

and op_is_eq operator =
  match operator with
  | Constant c -> c.id = "="
  | _ -> false

and op_is_or operator =
  match operator with
  | Constant c -> c.id = "OR"
  | _ -> false

and op_is_and operator =
  match operator with
  | Constant c -> c.id = "AND"
  | _ -> false

and op_is_implies operator =
  match operator with
  | Constant c -> c.id = "IMPLIES"
  | _ -> false

and op_is_iff operator =
  match operator with
  | Constant c -> c.id = "IFF"
  | _ -> false

and op_is_plus operator =
  match operator with
  | Constant c -> c.id = "+"
  | _ -> false

and op_is_times operator =
  match operator with
  | Constant c -> c.id = "*"
  | _ -> false

and op_is_minus operator =
  match operator with
  | Constant c -> c.id = "-"
  | _ -> false

and op_is_lt operator =
  match operator with
  | Constant c -> c.id = "<"
  | _ -> false

and op_is_gt operator =
  match operator with
  | Constant c -> c.id = ">"
  | _ -> false

and op_is_geq operator =
  match operator with
  | Constant c -> c.id = ">="
  | _ -> false

and op_is_leq operator =
  match operator with
  | Constant c -> c.id = "<="
  | _ -> false

and pp_selection fmt {pattern; expr} =
  F.fprintf fmt "@[@[%a@] ->@ @[%a@]@]"
    pp_pattern pattern
    pp_expr expr

and pp_pattern fmt {expr; variables} =
  F.fprintf fmt "@[%a%a@]"
    pp_expr expr
    pp_pattern_vars variables

and pp_pattern_vars fmt vars =
  match vars with
  | [] -> F.fprintf fmt ""
  | _::_ -> F.fprintf fmt "(%a)" F.(list pp_var) vars

and pp_typeref fmt ty =
  F.fprintf fmt "%S" ty

let pp_formula_decl fmt (d:formula_decl) =
  F.fprintf fmt "@[Formula %s (%s) =@\n@ @[%a@]@]"
    d.id
    d.label
    F.(list pp_expr) d.definition

let pp_expr_opt fmt e =
  match e with
  | Some e -> pp_expr fmt e
  | None -> F.fprintf fmt "None"

let pp_const_decl fmt (d:const_decl) =
  F.fprintf fmt "@[Const %s : %S@ @\n@ = @[%a@]@]"
    d.id
    d.type_
    pp_expr_opt d.const_def

let pp_var_decl fmt (d:var_decl) =
  F.fprintf fmt "@[Var %s : %S@]"
    d.id d.type_

let pp_type_eq_decl fmt (d:type_eq_decl) =
  F.fprintf fmt "@[TypeEq %s : %S@]"
    d.name d.type_

let pp_type_decl fmt (d:type_decl) =
  F.fprintf fmt "@[Type %s @]"
    d.name

let pp_conversion_decl fmt (d:conversion_decl) =
  F.fprintf fmt "@[Conversion %s = %a@]"
    d.id
    pp_expr d.expr

let pp_application_judgement fmt (d:application_judgement) =
  F.fprintf fmt "@[Judgement %s %a @]"
    d.id
    pp_expr d.name

let pp_subtype_judgement fmt (d:subtype_judgement) =
  F.fprintf fmt "@[SubtypeJudgement %s =@ %a SUBTYPE_OF %a @]"
    d.id
    pp_typeref d.subtype
    pp_typeref d.declared_type

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
  | TypeDecl d ->
    pp_type_decl fmt d
  | ConversionDecl d ->
    pp_conversion_decl fmt d
  | ApplicationJudgement d ->
    pp_application_judgement fmt d
  | SubtypeJudgement d ->
    pp_subtype_judgement fmt d

let rec pp_theory fmt (t:theory) =
  F.fprintf fmt "@[@[@{<Green>Theory@} %s [%a]@] =@\nBegin @[%a@]@\nEnd.@]@\n"
    t.id
    F.(list pp_formal_type_decl) t.formals
    F.(list ~sep:(return "@\n") pp_decl) t.declarations

and opt_no_prefix pp fmt x = match x with
  | None -> CCFormat.pp_print_string fmt ""
  | Some x -> CCFormat.fprintf fmt "%a" pp x

and pp_datatype fmt (d:datatype) =
  F.fprintf fmt "@[@{<Green>Datatype@} @[%a =@]@\n@[| %a@]@]@."
    F.(opt_no_prefix @@ list pp_formal_type_decl) d.formals
    F.(list ~sep:(return "@\n| ") pp_constructors) d.constructors

and pp_formal_type_decl fmt (d:formal_type_decl) =
  F.fprintf fmt "@[%s@]" d.id

and pp_constructors fmt (c:constructor) =
  F.fprintf fmt "@[%s@ @[(@[%a@])@ (@[%s@])@]@]"
    c.id
    F.(list pp_accessor) c.accessors
    c.recognizer

and pp_accessor fmt (a:accessor) =
  F.fprintf fmt "@[%s : (%a)@]"
    a.id
    F.(list string) a.declared_type

let pp_entry fmt = function
  | Theory d -> pp_theory fmt d
  | DataType d -> pp_datatype fmt d

let pp_module fmt m =
  F.fprintf fmt "@[@{<Blue>Module@}@[@ %a @]@\n  End.@]@."
    F.(list ~sep:(return "@\n") pp_entry) m

let pp_type_db fmt (db : (string,type_db_entry) Hashtbl.t) =
  F.fprintf fmt "\n@[Type DB:@\n@\n@[%a@]@]@."
    F.(list ~sep:(return "@\n") @@ pair string pp_type_db_entry)
    (CCList.uniq ~eq:(=) @@ List.of_seq @@ Hashtbl.to_seq db)

let pp fmt m =
  pp_module fmt m.module_;
  pp_type_db fmt m.type_hash
