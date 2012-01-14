
(*
  Mapping from ATD to JSON
*)

type json_list = [ `Array | `Object ]

type json_variant = { json_cons : string }

type json_field = {
  json_fname : string;
  json_unwrapped : bool
}

type json_repr =
    [ 
    | `Unit
    | `Bool
    | `Int
    | `Float

    | `String
    | `Sum
    | `Record
    | `Tuple
    | `List of json_list
    | `Option
    | `Shared
    | `External

    | `Cell
    | `Field of json_field
    | `Variant of json_variant
    | `Def
    ]

let json_list_of_string s : json_list option =
  match s with
      "array" -> Some `Array
    | "object" -> Some `Object
    | _ -> None

let get_json_list an =
  Atd_annot.get_field json_list_of_string `Array ["json"] "repr" an

let get_json_cons default an =
  Atd_annot.get_field (fun s -> Some s) default ["json"] "name" an

let get_json_fname default an =
  Atd_annot.get_field (fun s -> Some s) default ["json"] "name" an

