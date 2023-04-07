(* Imandra<->PVS: PVS AST represented in Imandra, converted to OCaml *)

open PVS_ast

module F = CCFormat

module Config = struct

  let resolve_types = ref true
  let full_id_paths = ref false
  let named_constructor_params = ref false

  let show () =
    F.printf "-- PVS<->Imandra configuration\n";
    F.printf "Resolve types: %b\n" !resolve_types;
    F.printf "Full ID paths: %b\n" !full_id_paths;
    F.printf "Named constructor params: %b\n" !named_constructor_params;

end

module Env = struct

  type t =
    | Theory of Id.t
    | Datatype of Id.t

end

module Context = struct

  type t = {
    cur_env: Env.t option;
    vars: (var_decl list) option;
    type_decls: (formal_type_decl list) option;
    type_db: (string, PVS_ast.typeref) Hashtbl.t;
  }

  let mk ?cur_env ?vars ?type_decls ~db () = {
    cur_env;
    vars;
    type_decls;
    type_db=db;
  }

  let extend_with_formals ~ctx formals = {
    ctx with
    type_decls = formals;
  }

  let extend_with_env ~ctx env = {
    ctx with
    cur_env = Some env;
  }

  let is_type_var ~ctx (s:typename) =
    match ctx.type_decls with
    | None -> false
    | Some ds ->
      CCList.mem ~eq:(=) s.id
        (CCList.map (fun (x:formal_type_decl) -> x.id)
           ds)

  let is_env_datatype ~ctx (s:typename) =
    match ctx.cur_env with
    | Some (Env.Datatype t) when t=s.id ->
      true
    | _ -> false

  let is_known_constructor ~ctx (s:constant) =
    let _ctx = ctx in
    History.seen_constructor s

  let type_vars_of_ctx ~ctx =
    match ctx.type_decls with
    | None -> []
    | Some ds -> List.map (fun (x:formal_type_decl) -> x.id) ds

end

module Translate = struct

  (* Initial list of pretty ML type vars.
     These get extended programatically. *)

  let init_type_vars = ["a";"b";"c"]

  let mod_name s =
    String.capitalize_ascii s

  let constructor s =
    match s with
    | "cons" -> "::"
    | "null" -> "[]"
    | s -> String.capitalize_ascii s

  let type_var s =
    "'" ^ String.lowercase_ascii s

end

module Traverse = struct

  let free_vars ?(bs=[]) e =
    let rec self bs e =
      let free v = not (List.mem v bs) in
      match e with
      | Variable v -> if free v then [v] else []
      | Lambda l ->
        self (l.bindings @ bs) l.expression
      | Cases cs ->
        self bs cs.expr
        @ CCList.flat_map
          (fun (x:selection) ->
             let pat_vars = x.pattern.variables in
             self (pat_vars @ bs) x.expr) cs.selections
        @ CCOption.map_or ~default:[] (self bs) cs.else_part
      | Apply {operator; argument} ->
        self bs operator
        @ CCList.flat_map (self bs) argument
      | If {test; then_; else_} ->
        self bs test @ self bs then_ @ self bs else_
      | _ -> []
    in
    History.sort_vars @@ CCList.uniq ~eq:(=) @@ self bs e

  let gather_rec_calls (f_id:Id.t) body =
    let rec self e =
      match e with
      | Variable _ -> []
      | Lambda l ->
        self l.expression
      | Cases cs ->
        self cs.expr
        @ CCList.flat_map
          (fun (x:selection) ->
             self x.expr) cs.selections
        @ CCOption.map_or ~default:[] self cs.else_part
      | Apply {operator; argument} ->
        if op_eq_id operator f_id then (
          e :: CCList.flat_map self argument
        ) else (
          []
        )
      | If {test; then_; else_} ->
        self test @ self then_ @ self else_
      | _ -> []
    in
    CCList.uniq ~eq:(=) @@ self body

end

let rec pp_expr ?(initial_constructor=false) ~ctx fmt e =
  let pp_expr = pp_expr ~initial_constructor:false ~ctx in
  match e with
  | Variable v -> pp_var fmt v
  | Constant _ when op_is_true e ->
    F.fprintf fmt "@[true@]"
  | Constant _ when op_is_false e ->
    F.fprintf fmt "@[false@]"
  | Constant c -> pp_const ~initial_constructor ~ctx fmt c
  | FormalConstant c -> F.string fmt c.constant_name
  | Lambda l ->
    let bs = l.bindings in
    let e = l.expression in
    F.fprintf fmt "@[@[(fun %a@] -> @[(@[%a@]))@]@]"
      F.(list (pp_var)) bs
      pp_expr e
  | Let {bindings; body} ->
    F.fprintf fmt "@[let (%a) in %a@]"
      (F.list @@ pp_let_binding ~ctx) bindings
      pp_expr body
  | Apply {operator; argument=[x;y]} when op_is_eq operator ->
    F.fprintf fmt "@[(%a@ =@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_neq operator ->
    F.fprintf fmt "@[(%a@ <>@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x]} when op_is_not operator ->
    F.fprintf fmt "@[not (%a)@]"
      pp_expr x
  | Apply {operator; argument=[x;y]} when op_is_or operator ->
    F.fprintf fmt "@[(%a@ ||@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_and operator ->
    F.fprintf fmt "@[(%a@ &&@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_implies operator ->
    F.fprintf fmt "@[(%a@ ==>@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_iff operator ->
    F.fprintf fmt "@[(%a@ <==>@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_plus operator ->
    F.fprintf fmt "@[(%a@ +@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_times operator ->
    F.fprintf fmt "@[(%a@ *@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[x;y]} when op_is_div operator ->
    F.fprintf fmt "@[(%a@ /@ %a)@]"
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
  | Apply {operator; argument=[x;y]} when op_is_cons ~ctx operator ->
    F.fprintf fmt "@[(%a@ ::@ %a)@]"
      pp_expr x pp_expr y
  | Apply {operator; argument=[]} when op_is_empty_list ~ctx operator ->
    F.fprintf fmt "@[[]@]"
  (* Treat constructor applications specially with tupled arguments *)
  | Apply {operator=Constant c; argument} when Context.is_known_constructor ~ctx c ->
    F.fprintf fmt "@[%a(@[@[%a@]@])@]"
      (pp_const ~initial_constructor ~ctx) c F.(list ~sep:(return ", ") pp_expr) argument
  (* General application *)
  | Apply {operator; argument} ->
    F.fprintf fmt "@[(%a@ @[@[%a@]@])@]"
      pp_expr operator F.(list ~sep:(return " ") pp_expr) argument
  | Cases c ->
    F.fprintf fmt "(@[match @[%a@] with@\n@[| %a@]@])"
      pp_expr c.expr
      F.(list ~sep:(return "@\n| ") @@ pp_selection ~ctx) c.selections
  | Integer n -> F.fprintf fmt "%d" n.integer_value
  | String s -> F.fprintf fmt "%S" s.string_value
  | If {test; then_; else_} ->
    F.fprintf fmt "(if %a then %a else %a)"
      pp_expr test pp_expr then_ pp_expr else_
  | Update u ->
    F.fprintf fmt "@[%a WITH [%a]@]"
      pp_expr u.expr F.(list @@ pp_assignment ~ctx) u.assignments
  | Forall {bindings; expression} ->
    F.fprintf fmt "@[@[∀%a@].@[(@[%a@])@]@]"
      F.(list (pp_var)) bindings pp_expr expression
  | Exists {bindings; expression} ->
    F.fprintf fmt "@[@[∃%a@].@[(@[%a@])@]@]"
      F.(list (pp_var_typed ~ctx)) bindings pp_expr expression
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
      F.(list ~sep:(return ";@ ") (pp_rec_row ~ctx)) assignments

and pp_var fmt v =
  F.fprintf fmt "%s" v.variable_name

and pp_var_typed ~ctx fmt v =
  F.fprintf fmt "%s:%a"
    v.variable_name (pp_type_db_entry ~ctx) v.type_

and pp_rec_row ~ctx fmt a =
  F.fprintf fmt "@[%s = %a@]"
    a.field (pp_expr ~initial_constructor:false ~ctx) a.expr

and pp_type_db_entry ~ctx fmt t =
  let pp_type_db_entry = pp_type_db_entry ~ctx in
  match t with
  | Ref s ->
    if not !Config.resolve_types then (
      F.fprintf fmt "ref %S" s
    ) else (
    (* We check to see if the target has been resolved in the Type DB.
       If it has, we print the resolution.
       If it hasn't, we print the reference. *)
    begin match Hashtbl.find_opt ctx.Context.type_db s with
      | Some (Resolved _ as t) -> pp_type_db_entry fmt t
      | Some (Ref t) ->
        F.fprintf fmt "ref %S" t
      | None -> failwith "type not in DB"
    end
  )
  | Resolved (SubType s) ->
    F.fprintf fmt "@[{@[x@[:%a@ | %a x@]@]}@]"
      (pp_type_db_entry_opt ~ctx) s.supertype
      (pp_expr_opt ~ctx) s.predicate
  | Resolved (FunctionType f) ->
    F.fprintf fmt "@[[@[%a@ ->@ %a@]]@]"
      pp_type_db_entry f.domain pp_type_db_entry f.range
  | Resolved (TupleType t) ->
    F.fprintf fmt "@[(%a)@]"
      F.(list ~sep:(return "@ *@ ") pp_type_db_entry) t.types
  | Resolved (TypeName t) ->
    if Context.is_type_var ~ctx t then (
      F.fprintf fmt "%s" (Translate.type_var t.id)
    ) else if Context.is_env_datatype ~ctx t then (
      F.fprintf fmt "(%a) %s"
        F.(list string) (CCList.map Translate.type_var (Context.type_vars_of_ctx ~ctx))
        t.id
    ) else (
      F.fprintf fmt "%s" t.id
    )
  | Resolved (DepBinding {id;type_}) ->
    F.fprintf fmt "@[DepBinding(id=%s, type=%a)@]"
      id
      pp_type_db_entry type_
  | Resolved (RecordType {fields}) ->
    F.fprintf fmt "@\n@ RecordType@[{@[%a@]}@]"
      F.(list @@ pp_rec_field ~ctx) fields
  | Resolved (DepTupleType _) ->
    F.fprintf fmt "@[<DepTupleType>@]"
  | Resolved (DepFunctionType _) ->
    F.fprintf fmt "@[<DepFunctionType>@]"

and pp_rec_field ~ctx fmt f =
  F.fprintf fmt "%s : %a"
    f.id
    (pp_type_db_entry ~ctx) f.type_

and pp_type_db_entry_opt ~ctx fmt e =
  match e with
  | Some e -> pp_type_db_entry ~ctx fmt e
  | None -> F.fprintf fmt "None"

and pp_expr_opt ~ctx fmt e =
  match e with
  | Some e -> pp_expr ~ctx fmt e
  | None -> F.fprintf fmt "None"

and pp_const ?(initial_constructor=false) ~ctx fmt c =
  match c.actuals with
  | (ConstActual _ :: _) as xs ->
    F.fprintf fmt "@[%s[%a]@]"
      c.id F.(list @@ pp_actual ~ctx) xs
  | _ ->
    let id = if initial_constructor then
        Translate.constructor c.id
      else if Context.is_known_constructor ~ctx c then
        Translate.constructor c.id
      else c.id
    in
    if !Config.full_id_paths then (
      F.fprintf fmt "%s.%s"
        (Translate.mod_name c.theory)
        id
    ) else (
      F.string fmt id
    )

and pp_actual ~ctx fmt a =
  match a with
  | ConstActual c ->
    pp_expr ~ctx fmt c.expr
  | TypeActual _t ->
    F.string fmt "<typeActual>"
    (* F.string fmt t.type_ *)

and pp_assignment ~ctx fmt (a:assignment) =
  F.fprintf fmt "@[(%a) := %a@]"
    F.(list @@ pp_expr ~ctx) a.arguments
    (pp_expr ~initial_constructor:false ~ctx) a.expr

and pp_let_binding ~ctx fmt (b:let_binding) =
  F.fprintf fmt "@[(%s = %a)@]"
    b.id
    (pp_expr ~initial_constructor:false ~ctx) b.expr

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

and op_is_true operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "TRUE"
  | _ -> false

and op_is_false operator =
  match operator with
  | Constant c ->
    c.theory = "booleans"
    && c.id = "FALSE"
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

and op_is_div operator =
  match operator with
  | Constant c -> c.id = "/"
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

and op_is_cons ~ctx operator =
  match operator with
  | Constant c ->
    Context.is_known_constructor ~ctx c && c.id = "cons"
  | _ -> false

and op_is_empty_list ~ctx operator =
  match operator with
  | Constant c ->
    Context.is_known_constructor ~ctx c && c.id = "null"
  | _ -> false

and pp_selection ~ctx fmt {pattern; expr} =
  F.fprintf fmt "@[@[%a@] ->@ @[%a@]@]"
    (pp_pattern ~ctx) pattern
    (pp_expr ~initial_constructor:false ~ctx) expr

and pp_pattern ~ctx fmt {expr; variables} =
  match expr, variables with
  | Constant c, [v1;v2] when c.id = "cons" ->
    F.fprintf fmt "@[%a :: %a@]" pp_var v1 pp_var v2
  | _ ->
  F.fprintf fmt "@[%a%a@]"
    (pp_expr ~initial_constructor:true ~ctx) expr
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

let pp_formula_decl ~ctx fmt (d:formula_decl) =
  F.fprintf fmt "@[Formula %s (%s) =@\n@ @[%a@]@]@\n"
    d.id
    d.label
    F.(list (pp_expr ~ctx)) d.definition

let pp_const_decl ~ctx fmt (d:const_decl) =
  let args = match d.const_def with
    | None -> []
    | Some _ -> d.parameters
  in
  let is_recursive = d.recursive
  in
  F.fprintf fmt "@[let%s %s %a@ = @[%a@]@]@\n"
    (if is_recursive then " rec" else "")
    d.id
    F.(list ~sep:(return " ") @@ pp_expr ~ctx) args
    (* (pp_type_db_entry ~ctx) d.type_ *)
    (pp_expr_opt ~ctx) d.const_def

let pp_var_decl ~ctx fmt (d:var_decl) =
  F.fprintf fmt "@[Var %s : %a@]@\n"
    d.id
    (pp_type_db_entry ~ctx) d.type_

let pp_type_eq_decl ~ctx fmt (d:type_eq_decl) =
  F.fprintf fmt "@[TypeEq %s = %a@]@\n"
    d.name
    (pp_type_db_entry ~ctx) d.type_

let pp_type_decl fmt (d:type_decl) =
  F.fprintf fmt "@[Type %s @]@\n"
    d.name

let pp_conversion_decl ~ctx fmt (d:conversion_decl) =
  F.fprintf fmt "@[Conversion %s = %a@]@\n"
    d.id
    (pp_expr ~initial_constructor:false ~ctx) d.expr

let pp_application_judgement ~ctx fmt (d:application_judgement) =
  F.fprintf fmt "@[Judgement (%s) (%a) @]@\n"
    d.id
    (pp_expr ~initial_constructor:false ~ctx) d.name

let pp_subtype_judgement ~ctx fmt (d:subtype_judgement) =
  F.fprintf fmt "@[SubtypeJudgement %s =@\n @[@[%a@]@\n SUBTYPE_OF@\n@[%a@]@]@]@\n"
    d.id
    (pp_type_db_entry ~ctx) d.type_
    (pp_type_db_entry ~ctx) d.subtype

let pp_decl ~ctx fmt d =
  match d with
  (* | FormulaDecl d ->
   *   (pp_formula_decl ~ctx) fmt d
   * | VarDecl d ->
   *   (pp_var_decl ~ctx) fmt d *)
  | ConstDecl d ->
    (pp_const_decl ~ctx) fmt d
  | TypeEqDecl d ->
    (pp_type_eq_decl ~ctx) fmt d
  | TypeDecl d ->
    pp_type_decl fmt d
  (* | ConversionDecl d ->
   *   pp_conversion_decl ~ctx fmt d
   * | ApplicationJudgement d ->
   *   pp_application_judgement ~ctx fmt d
   * | SubtypeJudgement d ->
   *   (pp_subtype_judgement ~ctx) fmt d
   * | NameJudgement _j ->
   *   F.fprintf fmt "<NameJudgement>"
   * | AutoRewriteDecl _ ->
   *   F.fprintf fmt "<AutoRewriteDecl>" *)
  | _ -> ()

let is_computational = function
  | ConstDecl _
  | TypeDecl _
  | TypeEqDecl _ -> true
  | _ -> false

let rec pp_theory ~ctx fmt (t:theory) =
  F.fprintf fmt "@[module %s = struct@\n@\n @[%a@]@\nend@]@\n"
    (Translate.mod_name t.id)
    F.(list ~sep:(return "@\n") (pp_decl ~ctx))
    (CCList.filter is_computational t.declarations)

and opt_no_prefix pp fmt x = match x with
  | None -> CCFormat.pp_print_string fmt ""
  | Some x -> CCFormat.fprintf fmt "%a" pp x

and pp_datatype ~ctx fmt (d:datatype) =
  let ctx = Context.extend_with_formals ~ctx d.formals in
  let ctx = Context.extend_with_env ~ctx (Env.Datatype d.id) in
  F.fprintf fmt "@[module %s = struct@\n@\n @[type @[%a %s =@]@\n@[| %a@]@]@\n@\nend@]@."
    (Translate.mod_name d.id)
    F.(opt_no_prefix @@ list pp_formal_type_decl) d.formals
    d.id
    F.(list ~sep:(return "@\n| ") (pp_constructors ~ctx)) d.constructors

and pp_formal_type_decl fmt (d:formal_type_decl) =
  if !Config.full_id_paths then (
    F.fprintf fmt "@[%s.%s@]"
      d.theory
      (Translate.type_var d.id)
  ) else (
    F.fprintf fmt "@[%s@]" (Translate.type_var d.id)
  )

and pp_constructors ~ctx fmt (c:constructor) =
  match c.accessors with
  | [] ->
    F.fprintf fmt "@[%s@]"
      (Translate.constructor c.id)
  | _ ->
    if !Config.named_constructor_params then (
      F.fprintf fmt "@[%s@ of @[{@[%a@]}@]@]"
        (Translate.constructor c.id)
        F.(list ~sep:(return ";@ ") (pp_accessor ~ctx)) c.accessors
    ) else (
      F.fprintf fmt "@[%s@ of @[(@[%a@])@]@]"
        (Translate.constructor c.id)
        F.(list ~sep:(return " *@ ") (pp_accessor ~ctx)) c.accessors
    )

and pp_accessor ~ctx fmt (a:accessor) =
  if !Config.named_constructor_params then (
    F.fprintf fmt "@[%s: %a@]"
      a.id
      F.(list (pp_type_db_entry ~ctx)) a.type_
  ) else (
    F.fprintf fmt "@[%a@]"
      F.(list (pp_type_db_entry ~ctx)) a.type_
  )

let pp_entry ~ctx fmt = function
  | Theory d -> pp_theory ~ctx fmt d
  | DataType d -> pp_datatype ~ctx fmt d

let pp_module ~ctx fmt m =
  F.fprintf fmt "%a"
    F.(list ~sep:(return "@\n") (pp_entry ~ctx)) m

let pp_type_db fmt ctx =
  F.fprintf fmt "\n@[Type DB%s:@\n@\n@[%a@]@]@."
    (if !Config.resolve_types then " (resolved)" else "")
    F.(list ~sep:(return "@\n") @@ pair string (pp_type_db_entry ~ctx))
    (CCList.uniq ~eq:(=) @@ List.of_seq @@ Hashtbl.to_seq ctx.type_db)

let resolve_types (db:type_db) =
  let f k v =
      Hashtbl.replace db k (Resolved (resolve_ty db v))
  in
  Hashtbl.iter f db

let pp fmt m =
  pp_module ~ctx:(Context.mk ~db:m.type_hash ()) fmt m.module_;
  (* pp_type_db fmt m.type_hash *)
