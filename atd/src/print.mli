(** Pretty-printing of ATD data *)

val default_format_annot : Ast.annot_section -> Easy_format.t

(** Same as [format] but works on any kind of node wrapped into [any]
    e.g. [format_any (Variant x)].
*)
val format :
  ?format_annot: (Ast.annot_section -> Easy_format.t) ->
  Ast.any -> Easy_format.t

(** Turn any AST node into a string. See also [format]. *)
val to_string :
  ?format_annot: (Ast.annot_section -> Easy_format.t) ->
  Ast.any -> string

(** Convert a type name with its arguments and its annotations
    into a string. *)
val string_of_type_inst :
  Ast.type_name -> Ast.type_expr list -> Ast.annot -> string

(** Concatenate the components of a type name into a string.
    For example, [TN ["foo"; "bar"]] gives ["foo.bar"].
    Shorthand for [Type_name.to_string]. *)
val tn : Ast.type_name -> string
