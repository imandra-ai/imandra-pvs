(* Imandra<->PVS: Event history and logical theory management *)

let m = ref (None : PVS_ast.module_with_hash option)

let seen_constructor (s:PVS_ast.constant) =
  match s.id with
  | "cons" | "null" -> true
  | _ -> false

let sort_vars (args:PVS_ast.variable list) =
  let cmp (x:PVS_ast.variable) (y:PVS_ast.variable) = compare x.type_ y.type_ in
  List.sort cmp args

let set v =
  m := Some v

let get () =
  match !m with
  | None -> raise Not_found
  | Some m -> m

let non_rec = " rec"
