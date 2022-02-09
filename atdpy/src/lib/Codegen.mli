(*
   Python code generation for JSON support (no biniou support)
*)

(** Take ATD type definitions and translate them to Python, writing
    them out to a file which should have the '.py' extension. *)
val to_file : atd_filename:string -> Atd.Ast.module_body -> string -> unit
