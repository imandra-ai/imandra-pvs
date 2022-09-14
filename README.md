# Imandra <-> PVS interface

## Example

```ocaml
dune utop src/

...

# #install_printer Imandra_PVS.PVS_ast.pp;;
# Imandra_PVS.Load.top "test/list_props.json";;
# Imandra_PVS.History.get ();;
```
