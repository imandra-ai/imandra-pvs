type typeref = int

type type_actual = { type_ : typeref }  

type constant = 
  { actuals : type_actual list
  ; constant_name : string
  ; type_ : typeref
  }

type variable = 
  { variable_name : string
  ; type_ : typeref
  }

type integer =
  { integer_value : int
  }

type expr =
  | Variable of variable 
  | Constant of constant
  | Lambda of lambda
  | Apply of apply
  | Cases of cases
  | If of if_
  | Integer of integer
and apply =
  { argument : expr list
  ; operator : expr 
  }
and lambda = 
  { expression : expr
  ; bindings : variable list  
  }
and if_ = 
  { test : expr
  ; else_ : expr
  ; then_ : expr
  }
and cases = 
  { selections : selection list
  ; expr : expr
  ; else_part : expr option
  }
and selection =
  { expr : expr
  ; pattern : string
  }


type const_decl = 
  { name  : string
  ; type_ : typeref
  ; const_def : expr
  }

type var_decl = 
  { declared_type : typeref list
  ; id : string
  ; type_ : typeref
  }

type proof_info = 
  { script : string
  ; status : string
  }

type exists = 
  { expression : expr
  ; bindings : variable list
  }

type forall = 
  { expression : expr
  ; bindings : variable list
  }

type definition =
  | Forall of forall
  | Exists of exists

type formula_decl = 
  { label : string
  ; definition : definition list
  ; id : string
  ; proof : proof_info
  }

type declaration =
  | FormulaDecl of formula_decl
  | VarDecl of var_decl
  | ConstDecl of const_decl

type formal_type_decl = { name : string }

type theory = 
  { declarations : declaration list
  ; id : string
  ; formals : formal_type_decl list
  ; assuming : unit
  }


type subtype = 
  { supertype : typeref
  ; predicate : expr
  } 

type functiontype = 
  { domain : typeref
  ; range : typeref
  } 

type tupletype = { types : typeref list }

type typename = { id : string }

type typelist_entry =
  | SubType of subtype 
  | FunctionType of functiontype 
  | TupleType of tupletype
  | TypeName of typename

type typelist = (string, typelist_entry) Hashtbl.t

type module_with_hash = 
  { module_ : theory list
  ; type_hash : typelist
  }