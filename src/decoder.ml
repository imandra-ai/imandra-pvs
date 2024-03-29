(* Imandra<->PVS: JSON Decoder *)

module D = Decoders_yojson.Basic.Decode

let (let*) = D.(let*)
let (>>=) = D.(>>=)

open PVS_ast

let tag : string D.decoder =
  D.one_of
  [ ( "string_tag" , D.string);
    ( "list_string_tag", D.list D.string >>= function
        | [] -> D.fail "empty tag list"
        | h::[] -> D.succeed h
        | _ -> D.fail "non-single tag list not implemented"
    )
  ]

let typeref : typeref D.decoder = D.string >>= fun x -> D.succeed (Ref x)

let typeref_w : typeref D.decoder =
  D.one_of
    [ ("flat int typeref", D.string >>= fun x -> D.succeed @@ Ref x)
    ; ("typeref in dict", D.field "typehash" D.string >>= fun x -> D.succeed @@ Ref x)
    ; ("int typeref in dict", D.field "typehash" D.int >>= fun x -> D.succeed @@ Ref (string_of_int x))
    ]

let type_actual : type_actual D.decoder =
  let* type_ = D.field "typehash" typeref_w in
  D.succeed { type_ }

let const_actual_ expr : const_actual D.decoder =
  let* expr = D.field "expr" expr in
  D.succeed { expr }

let actual_ expr : actual D.decoder =
  let* tag = D.field "tag" tag in
  match tag with
  | "type-actual" -> type_actual >>= fun x -> D.succeed @@ TypeActual x
  | "const-actual" -> const_actual_ expr >>= fun x -> D.succeed @@ ConstActual x
  | s -> D.fail @@ "Unknown actual tag" ^ s

let list_or_null (f : 'a D.decoder) : 'a list D.decoder =
  D.one_of
    [ ("list" , D.list f);
      ("null" , D.null >>= fun () -> D.succeed []);
    ]

let constant_ expr: constant D.decoder =
  let* actuals = D.maybe @@ D.field "actuals" ( D.list @@ actual_ expr ) in
  let* id = D.field "id" D.string in
  let* type_ = D.field "type" typeref_w in
  let* theory = D.field "theory" D.string in
  D.succeed ({ actuals=(match actuals with Some xs -> xs | None -> []);
               id;
               type_;
               theory;
             } : constant)

let variable : variable D.decoder =
  let* variable_name = D.field "id" D.string in
  let* type_ = D.field "type" typeref_w in
  D.succeed {
    variable_name;
    type_;
  }

let integer : integer D.decoder =
  let* integer_value = D.field "integer-value" D.int in
  D.succeed {
    integer_value;
  }

let string_const : string_const D.decoder =
  let* string_value = D.field "string-value" D.string in
  D.succeed {
    string_value;
  }

let formal_constant: formal_constant D.decoder =
  let* constant_name = D.field "id" D.string in
  let* theory = D.field "theory" D.string in
  D.succeed { constant_name; theory }

(* Workaround for arguments decoding -- it seems that there
   are two types of argument json encodings:

   - a single argument expression
   - a tuple with [["tag", "tuple"], ... rest arguments ... ]
 *)

let arguments_ (expr : expr D.decoder) =
  D.one_of
    [ ("tuple_exprs",
       (
          (* TODO: Speak to Sam about this need for `D.index 0` below;
             it might be a bug in the JSON generation. *)
           let* args = D.field "exprs" @@ D.list expr in
           D.succeed args
         )
      );
      ("normal_expr", expr >>= fun x -> D.succeed [ x ] )
    ]

let apply_ expr : apply D.decoder =
  let* argument = D.field "argument" (arguments_ expr) in
  let* operator = D.field "operator" expr in
  D.succeed
    { argument : expr list;
      operator : expr;
    }

let lambda_ expr : lambda D.decoder =
  let* expression = D.field "expression" expr in
  let* bindings = D.field "bindings" (D.list variable) in
  D.succeed (
    { expression;
      bindings;
    } : lambda )

let if_ expr : if_ D.decoder =
  let* test = D.field "test" expr in
  let* then_ = D.field "then" expr in
  let* else_ = D.field "else" expr in
  D.succeed { test; then_; else_ }

let pattern_ expr : pattern D.decoder =
  let* p1 = D.index 0 D.string in
  if p1 <> ("apply") then
    D.fail @@ "Not implemented pattern kind " ^ p1
  else
  let* expr = D.index 1 (expr) in
  let* variables = (D.index 2 (list_or_null variable )) in
  D.succeed {expr ; variables}

let selection_ expr : selection D.decoder =
  let* pattern = D.field "pattern" ( pattern_ expr ) in
  let* expr = D.field "expr" expr in
  D.succeed ( { expr ; pattern } : selection )

let cases_ expr : cases D.decoder =
  let* selections = D.field "selections" (D.list (selection_ expr)) in
  let* else_part = D.field "else-part" (D.maybe expr) in
  let* expr = D.field "expr" expr in
  D.succeed {
    selections;
    expr;
    else_part;
  }

let bindings_ expr : bindings D.decoder =
  let* expression = D.field "expression" expr in
  let* bindings = D.field "bindings" (D.list variable) in
  D.succeed {
    expression;
    bindings;
  }

let assignment expr : assignment D.decoder =
  (* There's an extra list around arguments, e.g.,
     "arguments": [ [ { "field": "length" } ] ]
     in `prelude4/list2finseq.json`.
   *)
  let* arguments = D.field "arguments" (D.list (D.index 0 expr)) in
  let* expr = D.field "expr" expr in
  D.succeed {
    arguments;
    expr;
  }

let update expr : update D.decoder =
  let* expression = D.field "expression" (expr) in
  let* assignments = D.field "assignments" (D.list @@ assignment expr) in
  D.succeed {
    expr=expression;
    assignments;
  }

let binding expr : let_binding D.decoder =
  let* id = D.field "id" D.string in
  let* expr = D.field "expr" expr in
  D.succeed ({
    id;
    expr;
  } : let_binding)

let let_ expr : let_ D.decoder =
  let* bindings = D.field "bindings" @@ D.list (binding expr) in
  let* body = D.field "body" expr in
  D.succeed {
    bindings;
    body;
  }

let project expr : project D.decoder =
  let* argument = D.field "argument" expr in
  let* index = D.field "index" D.int in
  D.succeed {
    argument;
    index;
  }

let tuple expr : tuple D.decoder =
  let* exprs = D.field "exprs" (D.list expr) in
  D.succeed ({
   exprs
  } : tuple)

let getfield expr : getfield D.decoder =
  let* argument = D.field "argument" expr in
  let* field = D.field "field" D.string in
  D.succeed ({
      argument;
      field;
    } : getfield)

let rec_row expr : rec_row D.decoder =
  let* expr = D.field "expr" expr in
  let* field = D.field "arguments" (D.index 0 (D.index 0 (D.field "field" D.string))) in
  D.succeed ({
      expr;
      field;
    } : rec_row)

let assignments expr : record D.decoder =
  let* assignments = D.field "assignments" @@ D.index 0 @@ D.list (rec_row expr) in
  D.succeed ({assignments} : record)

let expr : expr D.decoder =
  D.fix @@ fun expr ->
  let* tag = D.field "tag" tag in
  match tag with
  | "variable" ->
    let* variable = variable in
    D.succeed @@ Variable variable
  | "constant" ->
    let* constant = constant_ expr in
    D.succeed @@ Constant constant
  | "lambda" ->
    let* lambda = lambda_ expr in
    D.succeed @@ Lambda lambda
  | "apply" ->
    let* apply = apply_ expr in
    D.succeed @@ Apply apply
  | "let" ->
    let* let_ = let_ expr in
    D.succeed @@ Let let_
  | "cases" ->
    let* cases = cases_ expr in
    D.succeed @@ Cases cases
  | "if" ->
    let* if_ = if_ expr in
    D.succeed @@ If if_
  | "integer" ->
    let* integer = integer in
    D.succeed @@ Integer integer
  | "string" ->
    let* string = string_const in
    D.succeed @@ String string
  | "update" ->
    let* update = update expr in
    D.succeed @@ Update update
  | "forall" ->
    let* bindings = bindings_ expr in
    D.succeed @@ Forall bindings
  | "exists" ->
    let* bindings = bindings_ expr in
    D.succeed @@ Exists bindings
  | "formal-constant" ->
    let* formal_constant = formal_constant in
    D.succeed @@ FormalConstant formal_constant
  | "project" ->
    let* project = project expr in
    D.succeed @@ Project project
  | "tuple" ->
    let* tuple = tuple expr in
    D.succeed @@ Tuple tuple
  | "getfield" ->
    let* getfield = getfield expr in
    D.succeed @@ Getfield getfield
  | "record" ->
    (* D.fail "unsupported record" *)
    (* To see weird record encoding with nested lists, enable the above D.fail and then
       try decoding "test/json-prelude-theories4/PartialFunctionDefinitions.json" *)
    let* assignments = assignments expr in
    D.succeed @@ Record assignments
  | s -> D.fail @@ "Unknown expression tag '" ^ s ^ "'"

let subtype : subtype D.decoder =
  let* supertype = (* TODO: make strict with D.field_opt once JSON gen bug is fixed;
                        see `functions.json` from prelude-jsons3 *)
    D.maybe (D.field "supertype" typeref_w)
  in
  let* predicate = D.field "predicate" (D.nullable expr) in
  D.succeed
  { supertype : typeref option
  ; predicate : expr option
  }

let functiontype _ty : functiontype D.decoder =
  let* domain = D.field "domain" typeref_w in
  let* range = D.field "range" typeref_w in
  D.succeed {
    domain;
    range;
  }

let tupletype : tupletype D.decoder =
  let trlist : typeref list D.decoder =
    D.one_of
    [ ("single typeref", typeref_w >>= fun x -> D.succeed [x])
    ; ("typeref list", D.list typeref_w)
    ]
    in
  let* types = D.field "types" trlist in
  D.succeed { types }

let recordtype : record_type D.decoder =
  let field : field D.decoder =
    let* id = D.field "id" D.string in
    let* type_ = D.field "type" typeref_w in
    D.succeed ({
        id;
        type_;
      } : field)
  in
  let* fields = D.field "fields" (D.list field) in
  D.succeed { fields }

let dep_binding : dep_binding D.decoder =
  D.field "id" D.string >>= fun id ->
  D.field "type" typeref_w >>= fun type_ ->
  D.succeed @@ ({ id ; type_} : dep_binding)

let dep_fun_type decode_ty : dep_function_type D.decoder =
  let* domain = D.field "domain" dep_binding in
  let* range = D.field "range" (D.list decode_ty) in
  D.succeed ({ domain; range } : dep_function_type)

let typelist_entry : ty D.decoder =
  D.fix @@ fun ty ->
  let* tag = D.field "tag" tag in
  match tag with
  | "subtype" -> subtype >>= fun x -> D.succeed @@ SubType x
  | "functiontype" -> functiontype ty >>= fun x -> D.succeed @@ FunctionType x
  | "tupletype" -> tupletype >>= fun x -> D.succeed @@ TupleType x
  | "typename" -> D.field "id" D.string >>= fun id -> D.succeed @@ TypeName { id }
  | "dep-binding" -> dep_binding >>= fun x -> D.succeed @@ DepBinding x
  | "recordtype" -> recordtype >>= fun x -> D.succeed @@ RecordType x
  | "dependent-tupletype" ->
    (* TODO: Ignoring for now; needs some recursion refactoring *)
    (* D.field "types" @@ D.list typeref_w >>= fun types -> *)
    D.succeed @@ DepTupleType { types=[] }
  | "dependent-functiontype" ->
    dep_fun_type ty >>= fun x -> D.succeed @@ DepFunctionType x
  (*  | "enumtype" -> enumtype >>= fun x -> D.succeed @@ EnumType x *)
  | s -> D.fail @@ "Unknown typelist entry tag " ^ s

let type_db (ht : type_db) : type_db D.decoder =
  let* kvs = D.key_value_pairs (typelist_entry) in
  let () = kvs |> List.iter @@ fun (k,v) -> Hashtbl.add ht k (Resolved v) in
  D.succeed ht

let const_decl : const_decl D.decoder =
  let* id = D.field "id" D.string in
  let* type_ = D.field "type" typeref_w in
  (* A constant may not be defined. But if it is, the definition better parse!
     Thus, we use `D.field_opt` which is more forceful than just using `D.maybe expr.` *)
  let* const_def = D.field_opt "const-def" expr in
  let* theory = D.field "theory" D.string in
  let* parameters = D.field "parameters" @@ D.maybe @@ D.index 0 @@ D.list expr in
  let params = match parameters with
    | None -> []
    | Some ps -> ps
  in
  D.succeed ({
    id;
    type_;
    const_def;
    theory;
    parameters=params;
    recursive=false;
  } : const_decl)

let rec_func_decl : const_decl D.decoder =
  let* id = D.field "id" D.string in
  let* type_ = D.field "type" typeref_w in
  (* A constant may not be defined. But if it is, the definition better parse!
     Thus, we use `D.field_opt` which is more forceful than just using `D.maybe expr.` *)
  let* const_def = D.field "recursive-def" expr in
  let* theory = D.field "theory" D.string in
  let* parameters = D.field "parameters" @@ D.maybe @@ D.index 0 @@ D.list expr in
  let params = match parameters with
    | None -> []
    | Some ps -> ps
  in
  D.succeed ({
      id;
      type_;
      const_def=Some const_def;
      theory;
      parameters=params;
      recursive=true;
    } : const_decl)


let var_decl : var_decl D.decoder =
  let* id = D.field "id" D.string in
  let* type_ = D.field "type" typeref_w in
  D.succeed ({
      id;
      type_;
    } : var_decl)

let proof_info : proof_info D.decoder =
  let* script = D.field "script" D.string in
  let* status = D.field "status" D.string in
  D.succeed ({
    script: string;
    status: string
  } : proof_info)

let formula_decl : formula_decl D.decoder =
  let* label = D.field "label" D.string in
  let* definition = D.field "definition" expr in
  let* id = D.field "id" D.string in
  let* proof = D.maybe @@ D.field "proof" proof_info in
  D.succeed ({
    label: string;
    definition=[definition];
    id: string;
    proof;
  } : formula_decl)

let type_eq_decl : type_eq_decl D.decoder =
  let* name = D.field "id" D.string in
  let* type_ = D.field "type" typeref_w in
  D.succeed ({
      name;
      type_;
    } : type_eq_decl)

let type_decl : type_decl D.decoder =
  let* name = D.field "id" D.string in
  D.succeed ({
      name;
    } : type_decl)

let conversion_decl expr : conversion_decl D.decoder =
  let* id = D.field "id" D.string in
  let* expr = D.field "expr" expr in
  D.succeed ({
      id;
      expr;
    } : conversion_decl)

let application_judgement : application_judgement D.decoder =
  let* id = D.field "id" D.string in
  let* ty = D.field "type" ( typeref_w ) in
  let* name = D.field "name" expr in
  let* formals = D.field "formals" (list_or_null @@ D.list @@ expr) in
  let* judgement_type = D.field "judgement-type" ( D.list typeref_w ) in
  D.succeed ({
    id: string;
    type_=([ty] : typeref list);
    name: expr;
    formals: expr list list;
    judgement_type: typeref list;
  } : application_judgement)

let subtype_judgement : subtype_judgement D.decoder =
  let* id = D.field "id" D.string in
  let* type_ = D.field "type" typeref_w in
  let* subtype = D.field "subtype" typeref_w in
  D.succeed ({
    id: string;
    type_: typeref;
    subtype: typeref;
  } : subtype_judgement)

let name_judgement expr : name_judgement D.decoder =
  let* id = D.field "id" D.string in
  let* types = D.field "type" (typeref_w) in
  let* name = D.field "name" expr in
  D.succeed ({
      id: string;
      types = [types];
      name: expr;
    } : name_judgement)

let auto_rewrite_decl expr : auto_rewrite_decl D.decoder =
  let rewrite_name =
    let* id = D.field "id" D.string in
    let* theory = D.field "theory" D.string in
    let* actuals = D.field "actuals" (D.list @@ actual_ expr) in
    D.succeed ({
        id: string;
        theory;
        actuals;
      } : rewrite_name)
  in
  let* rewrite_names = D.list rewrite_name in
  D.succeed ({
      rewrite_names;
    } : auto_rewrite_decl)

let declaration expr : declaration D.decoder =
  let* tag = D.field "tag" tag in
  match tag with
  | "formula-decl" -> formula_decl >>= fun x -> D.succeed @@ FormulaDecl x
  | "var-decl" -> var_decl >>= fun x -> D.succeed @@ VarDecl x
  | "const-decl" -> const_decl >>= fun x -> D.succeed @@ ConstDecl x
  | "recursive-func-decl" -> rec_func_decl >>= fun x -> D.succeed @@ ConstDecl x
  | "type-eq-decl" -> type_eq_decl >>= fun x -> D.succeed @@ TypeEqDecl x
  | "type-decl" -> type_decl >>= fun x -> D.succeed @@ TypeDecl x
  | "conversion-decl" -> conversion_decl expr >>= fun x -> D.succeed @@ ConversionDecl x
  | "auto-rewrite-decl" -> auto_rewrite_decl expr >>= fun x -> D.succeed @@ AutoRewriteDecl x
  | "application-judgement" -> application_judgement >>= fun x -> D.succeed @@ ApplicationJudgement x
  | "subtype-judgement" -> subtype_judgement >>= fun x -> D.succeed @@ SubtypeJudgement x
  | "name-judgement" -> name_judgement expr >>= fun x -> D.succeed @@ NameJudgement x
  | s -> D.fail @@ "Unknown declaration tag " ^ s

let formal_type_decl : formal_type_decl D.decoder =
  let* id = D.field "id" D.string in
  let* theory = D.field "theory" D.string in
  D.succeed ({ id; theory } : formal_type_decl)

let theory expr : theory D.decoder =
  let* declarations = D.field "declarations" (D.list (declaration expr)) in
  let* id = D.field "id" D.string in
  let* formals = D.maybe @@ D.field "formals"  (list_or_null formal_type_decl) in
  D.succeed
  { declarations : declaration list
  ; id : string
  ; formals = match formals with Some f -> f | None -> [];
  ; assuming = ()
  }

let accessor : accessor D.decoder =
  let* id = D.field "id" D.string in
  let* type_ = D.field "type" (typeref_w) in
  D.succeed ({ id; type_=[type_] } : accessor)

let constructor : constructor D.decoder =
  let* id = D.field "id" D.string in
  let* accessors = D.maybe (D.field "accessors" (D.list accessor)) in
  let* recognizer = D.field "recognizer" D.string in
  D.succeed {
    id;
    (* accessors; *)
    accessors = (match accessors with None -> [] | Some xs -> xs);
    recognizer;
    assuming = ();
  }

let datatype : datatype D.decoder =
  let* id = D.field "id" D.string in
  let* formals = D.maybe (D.field "formals" (D.list formal_type_decl)) in
  let* constructors = D.field "constructors" (D.list constructor) in
  D.succeed ( {
      id;
      formals;
      constructors;
      assuming = ()
    } : datatype )

let module_entry : module_entry D.decoder =
  let* tag = D.field "tag" tag in
  match tag with
  | "theory" -> theory expr >>= fun x -> D.succeed @@ Theory x
  | "datatype" -> datatype >>= fun x -> D.succeed @@ DataType x
  | s -> D.fail @@ "Unknown module entry tag " ^ s

let module_with_hash : module_with_hash D.decoder =
  let* module_ = (D.field "module" module_entry) >>= fun x -> D.succeed [x] in
  let empty = Hashtbl.create 10 in
  let* type_hash =
    D.maybe @@ D.field "type-hash" (D.field "entries" (type_db @@ empty))
  in
  D.succeed
  { module_
  ; type_hash=empty
  }
