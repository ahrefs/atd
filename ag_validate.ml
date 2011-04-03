(*
  Mapping from ATD to "validate"
*)

type validate_repr = string option

let get_validator an =
  Atd_annot.get_field (fun s -> Some (Some s)) None ["ocaml"] "validator" an
