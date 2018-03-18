(*
  Mapping from ATD to JSON
*)

(* Name of the json adapter, referencing a pair of functions
   to convert to/from an atd-compatible json tree. *)
type json_adapter = string

type json_float = [ `Float of int option (* max decimal places *)
                  | `Int ]

type json_list = [ `Array | `Object ]

type json_variant = { json_cons : string option }

type json_field = {
  json_fname  : string;           (* <json name=...> *)
  json_tag_field : string option; (* <json tag_field=...> *)
  json_unwrapped : bool;
}

type json_record = {
  json_keep_nulls : bool; (* { ... } <json keep_nulls> *)
}

type json_repr =
  [ `Bool
  | `Cell
  | `Def
  | `External
  | `Field of json_field
  | `Float of json_float
  | `Int
  | `List of json_list
  | `Nullable
  | `Option
  | `Record of json_record
  | `String
  | `Sum of json_adapter option
  | `Tuple
  | `Unit
  | `Variant of json_variant
  | `Wrap ]

val get_json_sum : Atd.Annot.t -> json_adapter option

val get_json_list : Atd.Annot.t -> json_list

val get_json_float : Atd.Annot.t -> json_float

val get_json_cons : string -> Atd.Annot.t -> string

val get_json_fname : string -> Atd.Annot.t -> string

val get_json_tag_field : Atd.Annot.t -> string option

val get_json_untyped : Atd.Annot.t -> bool

val get_json_record : Atd.Annot.t -> json_record
