# Imandra <-> PVS interface

## Example

```ocaml
dune utop src/

...

# #install_printer Pvs.Pvs_ast.pp;;
# Pvs.Load.top "test/list_props.json";;
# Pvs.History.get ();;
```

## Notes

Note that all files prefixed with `err_` under `test/` are currently erroring on load.
