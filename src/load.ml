(* Imandra<->PVS: Module loading and top-level API *)

module D = Decoders_yojson.Basic.Decode

exception Error of D.error

let top (filename:string) : unit =
  match D.decode_file Decoder.module_with_hash filename with
  | Ok p -> History.set p
  | Error e -> raise (Error e)
