(*
  Mapping from ATD to "validate"
*)

type validate_repr = (string option * bool)
    (* (opt_v, b)
       is obtained by analyzing all available type definitions.
       The first value opt_v is the optional local validator
       coming from an ATD annotation (see `Local).
       The second value b is true iff the data doesn't need scanning.

       There are four cases:
       opt_v = None && b = true => no validation is needed at all
       opt_v = None && b = false => validators must be called on some
                                    sub-fields of the data
       opt_v <> None && b = true => the given validator must be called
                                    but there's no need to look into
                                    the sub-fields
       opt_v <> None && b = false => the given validator must be called
                                     in addition to scanning sub-fields
    *)

let get_validator an =
  Atd_annot.get_field (fun s -> Some (Some s)) None
    ["ocaml"] "validator" an

