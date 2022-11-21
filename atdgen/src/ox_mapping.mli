open Atd.Ast

type analyze_field = {
  ocaml_default : string option;
  unwrapped : bool
}

val analyze_field :
  Ocaml_repr.target -> loc -> field_kind -> annot -> analyze_field
