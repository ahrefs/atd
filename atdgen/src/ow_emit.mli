(** Code generator for www-form data serialization. *)

type field =
  { mapping : (Ocaml.Repr.t, Www_form.www_repr) Mapping.field_mapping
  ; ocaml_fname : string
  ; www_fname : string
  ; ocaml_default : string option
  ; optional : bool
  ; unwrapped : bool
  }

val make_ocaml_files
  :  unknown_field_handler:string option
  -> preprocess_input:string option
  -> opens:string list
  -> with_typedefs:bool
  -> with_create:bool
  -> with_fundefs:bool
  -> all_rec:bool
  -> pos_fname:string option
  -> pos_lnum:int option
  -> type_aliases:string option
  -> force_defaults:bool
  -> ocaml_version:(int * int) option
  -> pp_convs:Ocaml.pp_convs
  -> string option -> Ox_emit.target -> unit
