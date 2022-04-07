(*
   ATD annotations to be interpreted specifically by atdts.

   Atdts also honors json-related annotations defined in Atdgen_emit.Json.
*)

let get_ts_default an : string option =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["ts"]
      ~field:"default"
      an
