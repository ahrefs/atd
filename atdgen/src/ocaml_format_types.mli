(*
   A minimal OCaml AST used to pretty-print OCaml type definitions.
*)

type create_fields = {
  intf_params: string;
  impl_params: string;
  impl_fields: string;
}

val ocaml_of_atd :
  ?pp_convs:Ocaml_repr.pp_convs ->
  target:Ocaml_repr.target ->
  type_aliases:string option ->
  ((Atd.Ast.loc * Atd.Ast.annot)
   * Atd.Ast.import list
   * (bool * Atd.Ast.type_def list) list) ->
  string

val string_of_ocaml_int : Ocaml_repr.atd_ocaml_int -> string

val tick : Ocaml_repr.atd_ocaml_sum -> string

val dot : Ocaml_repr.atd_ocaml_record -> string

val map_record_creator_field :
  ((Ocaml_repr.t, 'a) Mapping.t ->
   (Ocaml_repr.t, 'b) Mapping.t) ->
  (Ocaml_repr.t, 'a) Mapping.field_mapping ->
  create_fields

val unwrap_option :
  ('b, 'c) Mapping.t ->
  ('b, 'c) Mapping.t
