(*
   ATD annotations to be interpreted specifically by atdpy.

   Atdpy also honors json-related annotations defined in Atdgen_emit.Json.
*)

type assoc_repr =
  | List
  | Dict

let get_python_default an : string option =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["python"]
      ~field:"default"
      an

let get_python_assoc_repr an : assoc_repr =
  Atd.Annot.get_field
    ~parse:(function
      | "list" -> Some List
      | "dict" -> Some Dict
      | _ -> None
    )
    ~default:List
    ~sections:["python"]
    ~field:"repr"
    an
