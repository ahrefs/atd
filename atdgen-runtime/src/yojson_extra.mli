(**
   This module complements the Yojson.Safe lexer and was extracted from the
   original source file [read.mll].

   It provides the functions that were removed accidentally during
   the transition from Yojson 2 to Yojson 3.
   See https://github.com/ocaml-community/yojson/pull/158#issuecomment-3630022314
*)

exception End_of_tuple

type variant_kind = [ `Square_bracket | `Double_quote ]

(** Parse an opening square bracket or a double quote which are the two
    valid openings for a variant according to ATD's convention.
    Raise an error otherwise. *)
val start_any_variant : Yojson.Safe.lexer_state -> Lexing.lexbuf -> variant_kind

(** Parse an opening square bracket or raise an error. *)
val start_any_tuple : Yojson.Safe.lexer_state -> Lexing.lexbuf -> unit

(** Read a closing square bracket and raise [End_of_tuple] or read nothing. *)
val read_tuple_end2 : Yojson.Safe.lexer_state -> Lexing.lexbuf -> unit

(** Read a comma. If instead a closing square bracket is found,
    raise [End_of_tuple]. If neither is found, raise an error. *)
val read_tuple_sep2 : Yojson.Safe.lexer_state -> Lexing.lexbuf -> unit
