(**
   Dlang-specific ATD annotations.

   This interface serves as a reference of which Dlang-specific
   ATD annotations are supported. Atdd also honors JSON-related annotations
   defined in [Atd.Json].
*)

(** Extract ["42"] from [<dlang default="42">].
    The provided default must be a well-formed Dlang immutable expression.
*)
val get_dlang_default : Atd.Annot.t -> string option

(** Whether an association list of ATD type [(string * foo) list]
    must be represented in Dlang as a list of pairs or as a dictionary.
    This is independent of the JSON representation.
*)
type assoc_repr =
  | List
  | Dict


(** How to represent a variant or record.
    [Recursive] Use this option when the type has recursive references, leading to members defined as pointers.
    [Enum] If a variant doesn't contain fields, this option will allow to represent it as an enum instead of a sum type.
*)
type shape =
  | Enum
  | Default
  | Recursive

val get_dlang_type_shape : Atd.Annot.t -> shape

(** Inspect annotations placed on lists of pairs such as
    [(string * foo) list <dlang repr="dict">].
    Permissible values for the [repr] field are ["dict"] and ["list"].
    The default is ["list"].
*)
val get_dlang_assoc_repr : Atd.Annot.t -> assoc_repr

(** Returns text the user wants to be inserted at the beginning of the
    Dlang file such as imports. *)
val get_dlang_import : Atd.Annot.t -> string list



type atd_dlang_wrap = {
  dlang_wrap_t : string;
  dlang_wrap : string;
  dlang_unwrap : string;
}

val get_dlang_wrap : Atd.Ast.loc ->
  Atd.Annot.t -> atd_dlang_wrap option
