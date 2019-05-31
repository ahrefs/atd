(**
  Mapping from ATD to WWW-form data
*)

(** Association between languages and www adapter for that language.
    The specification of each www adapter is language-specific. *)
type www_adapter = {
  ocaml_adapter : string option;
    (** A module implementing [normalize] and [restore]. *)
}

val no_adapter : www_adapter

type www_float =
  | Float of int option (* max decimal places *)
  | Int

type www_list = Array | Object

type www_variant = { www_cons : string }

type www_field = {
  www_fname  : string;           (* <www name=...> *)
  www_unwrapped : bool;
}

type www_record = {
  www_record_adapter : www_adapter;
}

type www_sum = {
  www_sum_adapter : www_adapter;
  www_open_enum : bool;
  www_lowercase_tags : bool;
}

(** The different kinds of ATD nodes with their www-specific options. *)
type www_repr =
  | Bool
  | Cell
  | Def
  | External
  | Field of www_field
  | Float of www_float
  | Int
  | List of www_list
  | Nullable
  | Option
  | Record of www_record
  | String
  | Sum of www_sum
  | Tuple
  | Unit
  | Variant of www_variant
  | Wrap

val get_www_list : Atd.Annot.t -> www_list

val get_www_float : Atd.Annot.t -> www_float

val get_www_cons : string -> Atd.Annot.t -> string

val get_www_fname : string -> Atd.Annot.t -> string

val get_www_record : Atd.Annot.t -> www_record

val get_www_sum : Atd.Annot.t -> www_sum

val tests : (string * (unit -> bool)) list

