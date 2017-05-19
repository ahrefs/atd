(*
  Mapping from ATD to biniou
*)

type biniou_int =
    [ `Svint | `Uvint | `Int8 | `Int16 | `Int32 | `Int64 ]

type biniou_float = [ `Float32 | `Float64 ]

type biniou_list = [ `Array | `Table ]

type biniou_field = { biniou_unwrapped : bool }

type biniou_repr =
    [
    | `Unit
    | `Bool
    | `Int of biniou_int
    | `Float of biniou_float

    | `String
    | `Sum
    | `Record
    | `Tuple
    | `List of biniou_list
    | `Option
    | `Nullable
    | `Wrap
    | `External

    | `Cell
    | `Field of biniou_field
    | `Variant
    | `Def
    ]

let biniou_int_of_string s : biniou_int option =
  match s with
      "svint" -> Some `Svint
    | "uvint" -> Some `Uvint
    | "int8" -> Some `Int8
    | "int16" -> Some `Int16
    | "int32" -> Some `Int32
    | "int64" -> Some `Int64
    | _ -> None

let biniou_float_of_string s : biniou_float option =
  match s with
      "float32" -> Some `Float32
    | "float64" -> Some `Float64
    | _ -> None

let biniou_list_of_string s : biniou_list option =
  match s with
      "array" -> Some `Array
    | "table" -> Some `Table
    | _ -> None

let get_biniou_int an =
  Atd_annot.get_field biniou_int_of_string `Svint ["biniou"] "repr" an

let get_biniou_float an =
  Atd_annot.get_field biniou_float_of_string `Float64 ["biniou"] "repr" an

let get_biniou_list an =
  Atd_annot.get_field biniou_list_of_string `Array ["biniou"] "repr" an
