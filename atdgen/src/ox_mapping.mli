open Atd.Ast

type analyze_field =
  { ocaml_default : string option
  ; unwrapped : bool
  }

val analyze_field : loc -> field_kind -> annot -> analyze_field
