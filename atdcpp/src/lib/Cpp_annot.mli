(**
   cpp-specific ATD annotations.

   This interface serves as a reference of which cpp-specific
   ATD annotations are supported. atdcpp also honors JSON-related annotations
   defined in [Atd.Json].
*)

(** Extract ["42"] from [<cpp default="42">].
    The provided default must be a well-formed cpp immutable expression.
*)
val get_cpp_default : Atd.Annot.t -> string option

(** Whether an association list of ATD type [(string * foo) list]
    must be represented in cpp as a list of pairs or as a dictionary.
    This is independent of the JSON representation.
*)
type assoc_repr =
  | List
  | Dict

(** Whether a sum type must be represented in cpp as a variant or as an enum.
    This is independent of the JSON representation.
*)
type sumtype_repr =
  | Variant
  | Enum

(** Inspection of annotations placed on sum types such as
    [type foo = A | B | C <cpp repr="enum">].
    Permissible values for the [repr] field are ["enum"] and ["variant"].
    The default is ["variant"].
*)
val get_cpp_sumtype_repr : Atd.Annot.t -> sumtype_repr


(** Inspect annotations placed on lists of pairs such as
    [(string * foo) list <cpp repr="dict">].
    Permissible values for the [repr] field are ["dict"] and ["list"].
    The default is ["list"].
*)
val get_cpp_assoc_repr : Atd.Annot.t -> assoc_repr

(** Returns text the user wants to be inserted at the beginning of the
    cpp file such as include. *)
val get_cpp_include : Atd.Annot.t -> string list

(** Return the namespace that needs to be used for the types defined in the
    ATD file. *)
val get_cpp_namespace : Atd.Annot.t -> string option


type atd_cpp_wrap = {
  cpp_wrap_t : string;
  cpp_wrap : string;
  cpp_unwrap : string;
  cpp_templatize : bool;
}

val get_cpp_wrap : Atd.Ast.loc ->
  Atd.Annot.t -> atd_cpp_wrap option
