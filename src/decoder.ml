module D = Decoders_yojson.Basic.Decode

let (let*) = D.(let*)
let (>>=) = D.(>>=)


open Pvs_ast 

let typeref : typeref D.decoder = D.int 

let type_actual : type_actual D.decoder = 
  let* type_ = D.field "type" typeref in
  D.succeed { type_ }

let list_or_null (f : 'a D.decoder) : 'a list D.decoder =
  D.one_of 
  [ ("list" , D.list f)
  ; ("null" , D.null >>= fun () -> D.succeed [])
  ]  

let constant : constant D.decoder = 
  let* actuals = D.field "actuals" ( list_or_null type_actual ) in
  let* constant_name = D.field "constant-name" D.string in
  let* type_ = D.field "type" typeref in
  D.succeed 
  { actuals
  ; constant_name
  ; type_
  }

let variable : variable D.decoder = 
  let* variable_name = D.field "variable-name" D.string in
  let* type_ = D.field "type" typeref in  
  D.succeed 
  { variable_name 
  ; type_ 
  }

let integer : integer D.decoder =
  let* integer_value = D.field "integer-value" D.int in
  D.succeed 
  { integer_value 
  }

(* Workaround for arguments decoding -- it seems that there 
are two types of argument json encodings: 
 - a single argument expression
 - a tuple with [["tag", "tuple"], ... rest arguments ... ]
*)
let arguments_ (expr : expr D.decoder) : (expr list) D.decoder =
  D.one_of 
  [ ("normal_expr", expr >>= fun x -> D.succeed [ x ] )
  ; ("tuple_exprs",
    let* tagtuple = D.index 0 (D.list D.string) in
    if tagtuple <> ["tag"; "tuple"] then D.fail "Argument type not implemented" else
    let* args = (D.list @@ D.maybe expr) in
    D.succeed @@ List.filter_map (fun x -> x) args
    )
  ]


let apply_ expr : apply D.decoder =
  let* argument = D.field "argument" (arguments_ expr) in
  let* operator = D.field "operator" expr in
  D.succeed 
  { argument : expr list
  ; operator : expr 
  }

let lambda_ expr : lambda D.decoder = 
  let* expression = D.field "expression" expr in
  let* bindings = D.field "bindings" (D.list variable) in
  D.succeed (
  { expression
  ; bindings
  } : lambda )

let if_ expr : if_ D.decoder = 
  let* test = D.field "test" expr in
  let* else_ = D.field "else" expr in
  let* then_ = D.field "then" expr in
  D.succeed { test; else_ ; then_ }

let selection_ expr : selection D.decoder =
  let* expr = D.field "expr" expr in
  let* pattern = D.field "pattern" D.string in
  D.succeed { expr ; pattern }

let cases_ expr : cases D.decoder = 
  let* selections = D.field "selections" (D.list (selection_ expr)) in
  let* else_part = D.field_opt "else-part" expr in
  let* expr = D.field "expr" expr in
  D.succeed { selections; expr; else_part}

let bindings_ expr : bindings D.decoder =
  let* expression = D.field "expression" expr in
  let* bindings = D.field "bindings" (D.list variable) in
  D.succeed 
  { expression 
  ; bindings 
  } 

let tag : string D.decoder =
  D.one_of
  [ ( "string_tag" , D.string)
  ; ( "list_string_tag", D.list D.string >>= function 
    | [] -> D.fail "empty tag list"
    | h::[] -> D.succeed h
    | _ -> D.fail "non-single tag list not implemented"
    )
  ]

let expr : expr D.decoder =
  D.fix @@ fun expr ->
  let* tag = D.field "tag" tag in
  match tag with
  | "variable" ->
    let* variable = variable in
    D.succeed @@ Variable variable 
  | "constant" -> 
    let* constant = constant in
    D.succeed @@ Constant constant 
  | "lambda" -> 
    let* lambda = lambda_ expr in
    D.succeed @@ Lambda lambda 
  | "apply" -> 
    let* apply = apply_ expr in
    D.succeed @@ Apply apply     
  | "cases" -> 
    let* cases = cases_ expr in
    D.succeed @@ Cases cases     
  | "if" -> 
    let* if_ = if_ expr in
    D.succeed @@ If if_     
  | "integer" ->
    let* integer = integer in
    D.succeed @@ Integer integer
  | "forall" ->
    let* bindings = bindings_ expr in
    D.succeed @@ Forall bindings
  | "exists" ->
    let* bindings = bindings_ expr in
    D.succeed @@ Exists bindings
  | s -> D.fail @@ "Unknown expression tag" ^ s

let typeref_w : typeref D.decoder =
  D.one_of
  [ ("flat int typeref", D.int)
  ; ("typeref in dict", D.field "type" D.int)
  ]

let subtype : subtype D.decoder =
  let* supertype = D.field "supertype" typeref_w in
  let* predicate = D.field "predicate" expr in
  D.succeed
  { supertype : typeref
  ; predicate : expr
  } 

let functiontype : functiontype D.decoder = 
  let* domain = D.field "domain" typeref_w in
  let* range = D.field "range" typeref_w in  
  D.succeed
  { domain 
  ; range 
  } 

let tupletype : tupletype D.decoder = 
  let trlist : typeref list D.decoder =
    D.one_of
    [ ("single typeref", typeref_w >>= fun x -> D.succeed [x])
    ; ("typeref list", D.list typeref_w)
    ]
    in
  let* types = D.field "types" trlist in
  D.succeed
  { types 
  } 

let typelist_entry : typelist_entry D.decoder = 
  let* tag = D.field "tag" tag in
  match tag with
  | "subtype" -> subtype >>= fun x -> D.succeed @@ SubType x 
  | "functiontype" -> functiontype >>= fun x -> D.succeed @@ FunctionType x 
  | "tupletype" -> tupletype >>= fun x -> D.succeed @@ TupleType x
  | "typename" -> D.field "id" D.string >>= fun id -> D.succeed @@ TypeName { id }
  | s -> D.fail @@ "Unknown typelist entry tag" ^ s

let typelist : typelist D.decoder =
  let result : typelist = Hashtbl.create 10 in
  let* kvs = D.key_value_pairs typelist_entry in
  let () = kvs |> List.iter @@ fun (k,v) -> Hashtbl.add result k v in
  D.succeed result


let const_decl : const_decl D.decoder = 
  let* name = D.field "name" D.string in
  let* type_ = D.field "type" typeref_w in
  let* const_def = D.field "const-def" expr in
  D.succeed
  { name  
  ; type_ 
  ; const_def 
  }

let var_decl : var_decl D.decoder = 
  let* declared_type = D.field "declared-type" (D.list typeref_w) in
  let* id = D.field "id" D.string in
  let* type_ = D.field "type"  typeref_w in
  D.succeed
  { declared_type
  ; id 
  ; type_ 
  }

let proof_info : proof_info D.decoder =
  let* script = D.field "script" D.string in
  let* status = D.field "status" D.string in
  D.succeed
  { script : string
  ; status : string
  }

let formula_decl : formula_decl D.decoder  = 
  let* label = D.field "label" D.string in
  let* definition = D.field "definition" ( D.list expr ) in 
  let* id = D.field "id" D.string in
  let* proof = D.field "proof" proof_info in
  D.succeed
  { label : string
  ; definition : expr list
  ; id : string
  ; proof : proof_info
  }

let declaration : declaration D.decoder =
  let* tag = D.field "tag" tag in
  match tag with
  | "formula-decl" -> formula_decl >>= fun x -> D.succeed @@ FormulaDecl x
  | "var-decl" -> var_decl >>= fun x -> D.succeed @@ VarDecl x
  | "const-decl" -> const_decl >>= fun x -> D.succeed @@ ConstDecl x
  | s -> D.fail @@ "Unknown declaration tag " ^ s

let formal_type_decl : formal_type_decl D.decoder = 
  let* name = D.field "name" D.string in
  D.succeed { name }

let theory : theory D.decoder = 
  let* declarations = D.field "declarations" ( D.list declaration ) in
  let* id = D.field "id" D.string in
  let* formals = D.field "formals"  (D.list formal_type_decl) in
  D.succeed
  { declarations : declaration list
  ; id : string
  ; formals : formal_type_decl list
  ; assuming = ()
  }

