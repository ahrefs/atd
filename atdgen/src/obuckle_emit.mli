
val make_ocaml_files
  :  opens:string list
  -> with_typedefs:'b
  -> with_create:'c
  -> with_fundefs:'d
  -> all_rec:bool
  -> pos_fname:string option
  -> pos_lnum:int option
  -> type_aliases:string option
  -> force_defaults:'e
  -> ocaml_version:'g
  -> pp_convs:'h
  -> string option
  -> Ox_emit.target
  -> unit
