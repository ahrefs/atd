
type json_float = [ `Float of int option (* max decimal places *)
                  | `Int ]

type json_list = [ `Array | `Object ]

type json_variant = { json_cons : string }

type json_field = {
  json_fname  : string;           (* <json name=...> *)
  json_tag_field : string option; (* <json tag_field=...> *)
  json_unwrapped : bool
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
  | `Record
  | `String
  | `Sum
  | `Tuple
  | `Unit
  | `Variant of json_variant
  | `Wrap ]


val get_json_list : Atd_annot.t -> json_list

val get_json_float : Atd_annot.t -> json_float

val get_json_cons : string -> Atd_annot.t -> string

val get_json_fname : string -> Atd_annot.t -> string

val get_json_tag_field : Atd_annot.t -> string option
