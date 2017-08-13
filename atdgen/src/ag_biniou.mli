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

val get_biniou_float : Atd_annot.t -> biniou_float
val get_biniou_int : Atd_annot.t -> biniou_int
val get_biniou_list : Atd_annot.t -> biniou_list
