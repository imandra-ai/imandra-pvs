(* Imandra<->PVS: Event history and logical theory management *)

let m = ref (None : Pvs_ast.module_with_hash option)

let set v =
  m := Some v

let get () =
  match !m with
  | None -> raise Not_found
  | Some m -> m
