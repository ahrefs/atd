val error : Atd_ast.loc -> string -> 'a

val error2 : Atd_ast.loc -> string -> Atd_ast.loc -> string -> 'a

val error3
  : Atd_ast.loc
  -> string
  -> Atd_ast.loc
  -> string
  -> Atd_ast.loc
  -> string -> 'a
