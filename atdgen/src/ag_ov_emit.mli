
val make_ocaml_files
  : opens:string list
  -> with_typedefs:bool
  -> with_create:bool
  -> with_fundefs:bool
  -> all_rec:bool
  -> pos_fname:string option
  -> pos_lnum:int option
  -> type_aliases:string option
  -> force_defaults:'a
  -> name_overlap:bool
  -> ocaml_version:'b
  -> pp_convs:[ `Camlp4 of string list | `Ppx of string list ]
  -> string option -> [< `Files of string | `Stdout ] -> unit
