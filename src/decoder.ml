module D = Decoders_yojson.Basic.Decode

let (let*) = D.(let*)
let (>>=) = D.(>>=)


open Pvs_ast 

let typeref : typeref D.decoder = D.int 

let type_actual : type_actual D.decoder = 
  let* type_ = D.field "type" typeref in
  D.succeed { type_ }
  
let constant : constant D.decoder = 
  let* actuals = D.field "actuals" ( D.list type_actual ) in
  let* constant_name = D.field "constant_name" D.string in
  let* type_ = D.field "type" typeref in
  D.succeed 
  { actuals
  ; constant_name
  ; type_
  }

let variable : variable D.decoder = 
  let* variable_name = D.field "variable_name" D.string in
  let* type_ = D.field "type" typeref in  
  D.succeed 
  { variable_name 
  ; type_ 
  }

let integer : integer D.decoder =
  let* integer_value = D.field "integer_value" D.int in
  D.succeed 
  { integer_value 
  }

let apply_ expr : apply D.decoder =
  let* argument = D.field "argument" (D.list expr) in
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

let expr : expr D.decoder =
  let tag : string D.decoder =
    D.one_of
    [ ( "string_tag" , D.string)
    ; ( "list_string_tag", D.list D.string >>= function 
      | [] -> D.fail "empty tag list"
      | h::_ -> D.succeed h
      )
    ]
    in
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
  | s -> D.fail @@ "Unknown expression tag" ^ s