(* Imandra<->PVS: Module loading and top-level API *)

module D = Decoders_yojson.Basic.Decode

exception Error of D.error

let top (filename:string) : PVS_ast.module_with_hash =
  match D.decode_file Decoder.module_with_hash filename with
  | Ok p ->
    begin
      History.set p;
      p
    end
  | Error e -> raise (Error e)

let () = CCFormat.set_color_default true
