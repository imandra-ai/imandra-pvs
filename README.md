# Imandra <-> PVS interface

## Example

```ocaml
dune utop src/

...

# #install_printer Imandra_PVS.PVS_ast_ml.pp;;
# Imandra_PVS.Load.top "test/json-prelude-theories/list_props.json";;
- : Imandra_PVS.PVS_ast.module_with_hash =
module List_props = struct

 let rec length l = (match l with
                     | [] -> 0
                     | x :: y -> ((length y) + 1))

 let rec member x l
 = (match l with
    | [] -> false
    | hd :: tl -> ((x = hd) || (member x tl)))

 let rec nth l n = (if (n = 0) then (car l) else (nth (cdr l) (n - 1)))

 let rec append l1 l2
 = (match l1 with
    | [] -> l2
    | x :: y -> (x :: (append y l2)))

 let rec reverse l
 = (match l with
    | [] -> l
    | x :: y -> (append (reverse y) (x :: [])))

end
```

Without a pretty-printer installed, you will see the `Imandra_PVS.PVS_ast.module_with_hash`
representation:

```ocaml
dune utop src/

# Imandra_PVS.Load.top "test/json-prelude-theories/list.json";;
- : Imandra_PVS.PVS_ast.module_with_hash =
{Imandra_PVS.PVS_ast.module_ =
  [Imandra_PVS.PVS_ast.DataType
    {Imandra_PVS.PVS_ast.id = "list";
     formals = Some [{Imandra_PVS.PVS_ast.id = "T"; theory = "list"}];
     constructors =
      [{Imandra_PVS.PVS_ast.id = "null"; accessors = []; recognizer = "null?";
        assuming = ()};
       {Imandra_PVS.PVS_ast.id = "cons";
        accessors =
         [{Imandra_PVS.PVS_ast.id = "car";
           type_ =
            [Imandra_PVS.PVS_ast.Ref "c2c53d66948214258a26ca9ca845d7ac0c17f8e7"]};
          {Imandra_PVS.PVS_ast.id = "cdr";
           type_ =
            [Imandra_PVS.PVS_ast.Ref "38b62be4bddaa5661c7d6b8e36e28159314df5c7"]}];
        recognizer = "cons?"; assuming = ()}];
     assuming = ()}];
 type_hash = <abstr>}

```

# Key modules

In `utop`, you can use `#show Imandra_PVS` to see the module structure:

```ocaml
# #show Imandra_PVS;;
module Imandra_PVS :
  sig
    module Decoder = Imandra_PVS.Decoder
    module History = Imandra_PVS.History
    module Id = Imandra_PVS.Id
    module Load = Imandra_PVS.Load
    module PVS_ast = Imandra_PVS.PVS_ast
    module PVS_ast_ml = Imandra_PVS.PVS_ast_ml
  end
```

- `Decoder`: The main JSON decoder. This parses ("decodes") a PVS theory from JSON into `PVS_ast`.
   The key entrypoint is `Decoder.module_with_hash`.
- `Load`: The main file loader.
   The key entrypoint is `Load.top`.
- `Imandra_PVS` and `Imandra_PVS_ml`: Key AST representations.
   For obtaining an OCaml version of a theory, the `Imandra_PVS.PVS_ast_ml.pp` is the key printer.

