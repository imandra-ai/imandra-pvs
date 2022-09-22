# Imandra <-> PVS interface

## Example

```ocaml
dune utop src/

...

# #install_printer Imandra_PVS.PVS_ast.pp;;
# Imandra_PVS.Load.top "test/list_props.json";;
# Imandra_PVS.History.get ();;
```

# Failing
We currently fail on:
 - `"tag": "getfield"` - example `"test/json-prelude-theories3/finite_sequences.json"` and `"test/json-prelude-theories3/list2finseq.json"` and `"test/json-prelude-theories3/more_finseq.json"` and `"test/json-prelude-theories3/PartialFunctionComposition.json"` and `"test/json-prelude-theories3/stdtokenizer.json"`
 - `"tag": "name-judgment"` - example `"test/json-prelude-theories3/finite_sets.json"` and `"test/json-prelude-theories3/real_props.json"` and `"test/json-prelude-theories3/sets_lemmas.json"`
 - `"tag": "auto-rewrite-decl"` - example `"test/json-prelude-theories3/sets.json"`
 - `"tag": "enumtype"` - example `"test/json-prelude-theories3/stdio.json"` and `"test/json-prelude-theories3/stdmath.json"`
 
```ocaml
utop # let m = Imandra_PVS.Load.top "test/json-prelude-theories3/integertypes.json";;
Exception:
Imandra_PVS.Load.Error
 While reading test/json-prelude-theories3/integertypes.json:
   Yojson.Json_error("Line 1, bytes 7115-7135:\nInt overflow '9223372036854775807'").
 ```

# Working (moved from Failing)

 - `"tag": "update"` - example `"test/json-prelude-theories3/bv_arithmetic_defs.json"`
 - `"tag": "conversion-decl"` - example `"test/json-prelude-theories3/bv_nat.json"` and `"test/json-prelude-theories3/bv.json"` and `"test/json-prelude-theories3/ctlops.json"` and `"test/json-prelude-theories3/extend_bool.json"` and `"test/json-prelude-theories3/Fairctlops.json"`
 - `"tag": "project"` - example `"test/json-prelude-theories3/bv_caret.json"` and `"test/json-prelude-theories3/QuotientDistributive.json"`
 
 
# Notes

- A nice example of `actuals` - `bv_extend_defs.json`, in particular `zero_extend`
