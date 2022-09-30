(* Imandra<->PVS: PVS AST represented in Imandra *)

type typeref =
  | Ref of string
  | Resolved of ty

and ty =
  | SubType of subtype
  | FunctionType of functiontype
  | TupleType of tupletype
  | TypeName of typename
  | DepBinding of dep_binding
  | RecordType of record_type

and accessor = {
    id: string;
    type_: typeref list;
  }

and constructor = {
  id: string;
  accessors: accessor list;
  recognizer: string;
  assuming: unit;
}

and datatype = {
  id: string;
  formals: (formal_type_decl list) option;
  constructors: constructor list;
  assuming: unit;
}

and formal_type_decl = { id : string; theory : string }

and subtype = {
  supertype: typeref option;
  predicate: expr option;
}

and functiontype = {
  domain: typeref;
  range: typeref;
}

and tupletype = {
  types: typeref list;
}

and typename = {
  id: string;
}

and dep_binding = {
  id: string;
  type_: typeref;
}

and record_type = {
  fields: field list;
}

and field = {
  id: string;
  type_: typeref;
}

and type_actual = {
  type_: typeref;
}

and variable = {
  variable_name: string;
  type_: typeref;
}

and integer = {
  integer_value: int;
}

and string_const = {
  string_value: string;
}

and formal_constant = {
  constant_name: string;
  theory: string;
}

and expr =
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
  | Record of record

and record = {
  assignments: rec_row list;
}

and rec_row = {
  field: string;
  expr: expr;
}

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
  type_: typeref list;
  name: expr;
  formals: expr list list;
  judgement_type: typeref list;
}

type subtype_judgement = {
  id: string;
  type_: typeref;
  subtype: typeref;
}

type conversion_decl = {
  id: string;
  expr: expr;
}

type name_judgement = {
  id: string;
  name: expr;
  types: typeref list;
}

type auto_rewrite_decl = {
  rewrite_names: rewrite_name list;
}

and rewrite_name = {
  id: string;
  theory: string;
  actuals: actual list;
}

type declaration =
  | FormulaDecl of formula_decl
  | VarDecl of var_decl
  | ConstDecl of const_decl
  | TypeDecl of type_decl
  | TypeEqDecl of type_eq_decl
  | ConversionDecl of conversion_decl
  | AutoRewriteDecl of auto_rewrite_decl
  | ApplicationJudgement of application_judgement
  | SubtypeJudgement of subtype_judgement
  | NameJudgement of name_judgement

type theory = {
  id: string;
  formals: formal_type_decl list;
  declarations: declaration list;
  assuming: unit;
}

type type_db = (string, typeref) Hashtbl.t

type module_entry =
  | Theory of theory
  | DataType of datatype

type module_with_hash = {
  module_: module_entry list;
  type_hash: type_db;
}

module F = CCFormat

let rec pp_expr ~db fmt e =
  let pp_expr = pp_expr ~db in
  match e with
  | Variable v -> pp_var fmt v
  | Constant c -> pp_const ~db fmt c
  | FormalConstant c -> F.string fmt c.constant_name
  | Lambda l ->
    let bs = l.bindings in
    let e = l.expression in
    F.fprintf fmt "@[@[λ%a@].@[(@[%a@])@]@]"
      F.(list (pp_var)) bs
      pp_expr e
  | Let {bindings; body} ->
    F.fprintf fmt "@[let (%a) in %a@]"
      (F.list @@ pp_let_binding ~db) bindings
      pp_expr body
  | Apply {operator; argument=[x;y]} when op_is_eq operator ->
    F.fprintf fmt "@[(%a@ =@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_neq operator ->
    F.fprintf fmt "@[(%a@ /=@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x]} when op_is_not operator ->
    F.fprintf fmt "@[¬%a@]"
      pp_expr x
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
      F.(list ~sep:(return "@\n| ") @@ pp_selection ~db) c.selections
  | Integer n -> F.fprintf fmt "%d" n.integer_value
  | String s -> F.fprintf fmt "%S" s.string_value
  | If {test; then_; else_} ->
    F.fprintf fmt "(if %a then %a else %a)"
      pp_expr test pp_expr then_ pp_expr else_
  | Update u ->
    F.fprintf fmt "@[%a WITH [%a]@]"
      pp_expr u.expr F.(list @@ pp_assignment ~db) u.assignments
  | Forall {bindings; expression} ->
    F.fprintf fmt "@[@[∀%a@].@[(@[%a@])@]@]"
      F.(list (pp_var)) bindings pp_expr expression
  | Exists {bindings; expression} ->
    F.fprintf fmt "@[@[∃%a@].@[(@[%a@])@]@]"
      F.(list (pp_var_typed ~db)) bindings pp_expr expression
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
  | Record {assignments} ->
    F.fprintf fmt "@[Record{@[%a@]}@]"
      F.(list ~sep:(return ";@ ") (pp_rec_row ~db)) assignments

and pp_var fmt v =
  F.fprintf fmt "%s" v.variable_name

and pp_var_typed ~db fmt v =
  F.fprintf fmt "%s:%a"
    v.variable_name (pp_type_db_entry ~db) v.type_

and pp_rec_row ~db fmt a =
  F.fprintf fmt "@[%s = %a@]"
    a.field (pp_expr ~db) a.expr

and cfg_resolve_types = ref false
and cfg_full_id_paths = ref false

and pp_type_db_entry ~db fmt t =
  let pp_type_db_entry = pp_type_db_entry ~db in
  match t with
  | Ref s ->
    if not !cfg_resolve_types then (
      F.fprintf fmt "ref %S" s
    ) else (
    (* We check to see if the target has been resolved in the Type DB.
       If it has, we print the resolution.
       If it hasn't, we print the reference. *)
    begin match Hashtbl.find_opt db s with
      | Some (Resolved _ as t) -> pp_type_db_entry fmt t
      | Some (Ref t) ->
        F.fprintf fmt "ref %S" t
      | None -> failwith "type not in DB"
    end
  )
  | Resolved (SubType s) ->
    F.fprintf fmt "@[{x:%a | %a x}@]"
      (pp_type_db_entry_opt ~db) s.supertype
      (pp_expr_opt ~db) s.predicate
  | Resolved (FunctionType f) ->
    F.fprintf fmt "@[[%a@ ->@ %a]@]"
      pp_type_db_entry f.domain pp_type_db_entry f.range
  | Resolved (TupleType t) ->
    F.fprintf fmt "@[(%a)@]"
      F.(list ~sep:(return "@ *@ ") pp_type_db_entry) t.types
  | Resolved (TypeName t) ->
    F.string fmt t.id
  | Resolved (DepBinding {id;type_}) ->
    F.fprintf fmt "@[DepBinding(id=%s, type=%a)@]"
      id
      pp_type_db_entry type_
  | Resolved (RecordType {fields}) ->
    F.fprintf fmt "@\n@ RecordType@[{@[%a@]}@]"
      F.(list @@ pp_rec_field ~db) fields

and pp_rec_field ~db fmt f =
  F.fprintf fmt "%s : %a"
    f.id
    (pp_type_db_entry ~db) f.type_

and pp_type_db_entry_opt ~db fmt e =
  match e with
  | Some e -> pp_type_db_entry ~db fmt e
  | None -> F.fprintf fmt "None"

and pp_expr_opt ~db fmt e =
  match e with
  | Some e -> pp_expr ~db fmt e
  | None -> F.fprintf fmt "None"

and pp_const ~db fmt c =
  match c.actuals with
  | (ConstActual _ :: _) as xs ->
    F.fprintf fmt "@[%s[%a]@]"
      c.id F.(list @@ pp_actual ~db) xs
  | _ ->
    if !cfg_full_id_paths then (
      F.fprintf fmt "%s.%s" c.theory c.id
    ) else (
      F.string fmt c.id
    )

and pp_actual ~db fmt a =
  match a with
  | ConstActual c ->
    pp_expr ~db fmt c.expr
  | TypeActual _t ->
    F.string fmt "<typeActual>"
    (* F.string fmt t.type_ *)

and pp_assignment ~db fmt (a:assignment) =
  F.fprintf fmt "@[(%a) := %a@]"
    F.(list @@ pp_expr ~db) a.arguments
    (pp_expr ~db) a.expr

and pp_let_binding ~db fmt (b:let_binding) =
  F.fprintf fmt "@[(%s = %a)@]"
    b.id
    (pp_expr ~db) b.expr

and op_is_eq operator =
  match operator with
  | Constant c ->
    c.theory = "equalities"
    && c.id = "="
  | _ -> false

and op_is_not operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "NOT"
  | _ -> false

and op_is_neq operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "/="
  | _ -> false

and op_is_or operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "OR"
  | _ -> false

and op_is_and operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "AND"
  | _ -> false

and op_is_implies operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "IMPLIES"
  | _ -> false

and op_is_iff operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "IFF"
  | _ -> false

and op_is_plus operator =
  match operator with
  | Constant c ->
    c.id = "+"
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

and pp_selection ~db fmt {pattern; expr} =
  F.fprintf fmt "@[@[%a@] ->@ @[%a@]@]"
    (pp_pattern ~db) pattern
    (pp_expr ~db) expr

and pp_pattern ~db fmt {expr; variables} =
  F.fprintf fmt "@[%a%a@]"
    (pp_expr ~db) expr
    (pp_pattern_vars) variables

and pp_pattern_vars fmt vars =
  match vars with
  | [] -> F.fprintf fmt ""
  | _::_ -> F.fprintf fmt "(%a)" F.(list @@ pp_var) vars

and resolve_ty (db:type_db) (t:typeref) =
  match t with
  | Ref (s:string) ->
    let (ty:ty) = begin
      match Hashtbl.find db s with
      | Ref _ as r -> resolve_ty db r
      | Resolved ty -> resolve_ty_aux db ty
    end
    in
    resolve_ty_aux db ty
  | Resolved ty ->
    resolve_ty_aux db ty

and resolve_ty_aux (db:type_db) (ty:ty) =
  match (ty:ty) with
  | TupleType {types} ->
    TupleType {types = List.map (fun x -> Resolved (resolve_ty db x)) types}
  | SubType {supertype; predicate} ->
    SubType {supertype = CCOption.map (fun x -> Resolved (resolve_ty db x)) supertype;
             predicate}
  | FunctionType {domain;range} ->
    FunctionType {domain = Resolved (resolve_ty db domain);
                  range = Resolved (resolve_ty db range)}
  | x -> x

let pp_formula_decl ~db fmt (d:formula_decl) =
  F.fprintf fmt "@[Formula %s (%s) =@\n@ @[%a@]@]@\n"
    d.id
    d.label
    F.(list (pp_expr ~db)) d.definition

let pp_const_decl ~db fmt (d:const_decl) =
  F.fprintf fmt "@[Const %s : %a@ @ = @[%a@]@]@\n"
    d.id
    (pp_type_db_entry ~db) d.type_
    (pp_expr_opt ~db) d.const_def

let pp_var_decl ~db fmt (d:var_decl) =
  F.fprintf fmt "@[Var %s : %a@]@\n"
    d.id
    (pp_type_db_entry ~db) d.type_

let pp_type_eq_decl ~db fmt (d:type_eq_decl) =
  F.fprintf fmt "@[TypeEq %s = %a@]@\n"
    d.name
    (pp_type_db_entry ~db) d.type_

let pp_type_decl fmt (d:type_decl) =
  F.fprintf fmt "@[Type %s @]@\n"
    d.name

let pp_conversion_decl ~db fmt (d:conversion_decl) =
  F.fprintf fmt "@[Conversion %s = %a@]@\n"
    d.id
    (pp_expr ~db) d.expr

let pp_application_judgement ~db fmt (d:application_judgement) =
  F.fprintf fmt "@[Judgement (%s) (%a) @]@\n"
    d.id
    (pp_expr ~db) d.name

let pp_subtype_judgement ~db fmt (d:subtype_judgement) =
  F.fprintf fmt "@[SubtypeJudgement %s =@\n @[@[%a@]@\n SUBTYPE_OF@\n@[%a@]@]@]@\n"
    d.id
    (pp_type_db_entry ~db) d.type_
    (pp_type_db_entry ~db) d.subtype

let pp_decl ~db fmt d =
  match d with
  | FormulaDecl d ->
    (pp_formula_decl ~db) fmt d
  | VarDecl d ->
    (pp_var_decl ~db) fmt d
  | ConstDecl d ->
    (pp_const_decl ~db) fmt d
  | TypeEqDecl d ->
    (pp_type_eq_decl ~db) fmt d
  | TypeDecl d ->
    pp_type_decl fmt d
  | ConversionDecl d ->
    pp_conversion_decl ~db fmt d
  | ApplicationJudgement d ->
    pp_application_judgement ~db fmt d
  | SubtypeJudgement d ->
    (pp_subtype_judgement ~db) fmt d
  | NameJudgement _j ->
    F.fprintf fmt "<NameJudgement>"
  | AutoRewriteDecl _ ->
    F.fprintf fmt "<AutoRewriteDecl>"

let rec pp_theory ~db fmt (t:theory) =
  F.fprintf fmt "@[@[@{<Green>Theory@} %s [@[%a@]]@] =@\nBegin @[%a@]@\nEnd.@]@\n"
    t.id
    F.(list pp_formal_type_decl) t.formals
    F.(list ~sep:(return "@\n") (pp_decl ~db)) t.declarations

and opt_no_prefix pp fmt x = match x with
  | None -> CCFormat.pp_print_string fmt ""
  | Some x -> CCFormat.fprintf fmt "%a" pp x

and pp_datatype ~db fmt (d:datatype) =
  F.fprintf fmt "@[@{<Green>Datatype@} @[%a =@]@\n@[| %a@]@]@."
    F.(opt_no_prefix @@ list pp_formal_type_decl) d.formals
    F.(list ~sep:(return "@\n| ") (pp_constructors ~db)) d.constructors

and pp_formal_type_decl fmt (d:formal_type_decl) =
  if !cfg_full_id_paths then (
    F.fprintf fmt "@[%s.%s@]"
      d.theory
      d.id
  ) else (
    F.fprintf fmt "@[%s@]" d.id
  )

and pp_constructors ~db fmt (c:constructor) =
  F.fprintf fmt "@[%s@ @[(@[%a@])@ (@[%s@])@]@]"
    c.id
    F.(list (pp_accessor ~db)) c.accessors
    c.recognizer

and pp_accessor ~db fmt (a:accessor) =
  F.fprintf fmt "@[%s : (%a)@]"
    a.id
    F.(list (pp_type_db_entry ~db)) a.type_

let pp_entry fmt = function
  | Theory d -> pp_theory fmt d
  | DataType d -> pp_datatype fmt d

let pp_module ~db fmt m =
  F.fprintf fmt "@[@{<Blue>Module@}@[@ %a @]@\n  End.@]@."
    F.(list ~sep:(return "@\n") (pp_entry ~db)) m

let pp_type_db fmt (db:type_db) =
  F.fprintf fmt "\n@[Type DB%s:@\n@\n@[%a@]@]@."
    (if !cfg_resolve_types then " (resolved)" else "")
    F.(list ~sep:(return "@\n") @@ pair string (pp_type_db_entry ~db))
    (CCList.uniq ~eq:(=) @@ List.of_seq @@ Hashtbl.to_seq db)

let resolve_types (db:type_db) =
  let f k v =
      Hashtbl.replace db k (Resolved (resolve_ty db v))
  in
  Hashtbl.iter f db

let pp fmt m =
  pp_module ~db:m.type_hash fmt m.module_;
  pp_type_db fmt m.type_hash
