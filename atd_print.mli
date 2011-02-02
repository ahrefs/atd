(* $Id: atd_print.mli 46617 2010-08-11 18:13:30Z martin $ *)

(** Pretty-printing of ATD data *)

val format : Atd_ast.full_module -> Easy_format.t
  (** Pretty-printing. Use the functions of the [Easy_format.Pretty]
      module to convert an [Easy_format.t] into a string
      or add it to a channel or buffer. *)

val string_of_type_name : 
  string -> Atd_ast.type_expr list -> Atd_ast.annot -> string
  (** Convert a type name with its arguments and its annotations
      into a string. *)
