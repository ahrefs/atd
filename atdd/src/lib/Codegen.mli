(*
   Dlang code generation for JSON support (no biniou support)
*)

(** Take ATD type definitions and translate them to Dlang, writing
    them out to a file which should have the '.d' extension. *)
val run_file : tags:string list -> string -> unit
