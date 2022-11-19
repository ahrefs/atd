(*
   A minimal OCaml AST used to pretty-print OCaml type definitions.
*)

val ocaml_of_atd
  : ?pp_convs:Ocaml.pp_convs
  -> target:Ocaml.target
  -> type_aliases:string option
  -> ((Atd.Ast.loc * Atd.Ast.annot)
      * Atd.Ast.import list
      * (bool * Atd.Ast.type_def list) list)
  -> string
