(*
   ATD annotations to be interpreted specifically by atdd.

   Atdd also honors json-related annotations defined in Atd.Json.
*)

type assoc_repr =
  | List
  | Dict

let get_dlang_default an : string option =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["dlang"]
      ~field:"default"
      an

let get_dlang_assoc_repr an : assoc_repr =
  Atd.Annot.get_field
    ~parse:(function
      | "list" -> Some List
      | "dict" -> Some Dict
      | _ -> None
    )
    ~default:List
    ~sections:["dlang"]
    ~field:"repr"
    an

(* imports etc. *)
let get_dlang_text an : string list =
  Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["dlang"]
    ~field:"text"
    an

let get_dlang_json_text an : string list =
  get_dlang_text an
  @ Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["dlang"]
    ~field:"json_dlang.text"
    an
