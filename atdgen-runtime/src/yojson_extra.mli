(**
   This module complements the Yojson.Safe lexer and was extracted from the
   original source file [read.mll].
   It provides the functions [start_any_variant] and [start_any_tuple]
   that were removed accidentally during the transition from Yojson 2 to
   Yojson 3.
*)

type variant_kind = [ `Square_bracket | `Double_quote ]

(** Parse an opening square bracket or a double quote which are the two
    valid openings for a variant according to ATD's convention.
    Raise an error otherwise. *)
val start_any_variant : Yojson.Safe.lexer_state -> Lexing.lexbuf -> variant_kind

(** Parse an opening square bracket or raise an error. *)
val start_any_tuple : Yojson.Safe.lexer_state -> Lexing.lexbuf -> bool
