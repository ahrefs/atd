let read_lexbuf
    ?annot_schema
    ?(expand = false)
    ?keep_builtins
    ?keep_poly
    ?(xdebug = false)
    ?(inherit_fields = false)
    ?(inherit_variants = false)
    ?(pos_fname = "")
    ?(pos_lnum = 1)
    lexbuf =

  Lexer.init_fname lexbuf pos_fname pos_lnum;
  let module_ = Parser.module_ Lexer.token lexbuf in
  Check.check module_;
  let type_defs =
    if inherit_fields || inherit_variants then
      Inherit.expand_module_body ~inherit_fields ~inherit_variants
        module_.imports module_.type_defs
    else
      module_.type_defs
  in
  let type_defs =
    if expand then
      Expand.expand_type_defs ?keep_builtins ?keep_poly ~debug: xdebug
        type_defs
    else
      type_defs
  in
  let module_ =
    { module_ with type_defs }
  in
  (match annot_schema with
   | None -> ()
   | Some schema ->
       Annot.validate schema (Ast.Module module_)
  );
  module_

let read_channel
    ?annot_schema ?expand ?keep_builtins ?keep_poly ?xdebug
    ?inherit_fields ?inherit_variants
    ?pos_fname ?pos_lnum
    ic =
  let lexbuf = Lexing.from_channel ic in
  let pos_fname =
    if pos_fname = None && ic == stdin then
      Some "<stdin>"
    else
      pos_fname
  in
  read_lexbuf ?annot_schema ?expand ?keep_builtins ?keep_poly ?xdebug
    ?inherit_fields ?inherit_variants ?pos_fname ?pos_lnum lexbuf

let load_file
    ?annot_schema ?expand ?keep_builtins ?keep_poly ?xdebug
    ?inherit_fields ?inherit_variants
    ?pos_fname ?pos_lnum
    file =
  let ic = open_in file in
  let finally () = close_in_noerr ic in
  try
    let pos_fname =
      match pos_fname with
          None -> Some file
        | Some _ -> pos_fname
    in
    let ast =
      read_channel
        ?annot_schema ?expand ?keep_builtins ?keep_poly ?xdebug
        ?inherit_fields ?inherit_variants ?pos_fname ?pos_lnum ic
    in
    finally ();
    ast
  with e ->
    finally ();
    raise e

let load_string
    ?annot_schema ?expand ?keep_builtins ?keep_poly ?xdebug
    ?inherit_fields ?inherit_variants
    ?pos_fname ?pos_lnum
    s =
  let lexbuf = Lexing.from_string s in
  read_lexbuf ?annot_schema ?expand ?keep_builtins ?keep_poly ?xdebug
    ?inherit_fields ?inherit_variants ?pos_fname ?pos_lnum lexbuf

module Tsort = Sort.Make (
  struct
    type t = Ast.type_def
    type id = string (* type name *)
    let id (x : t) = x.name
    let to_string name = name
  end
)

let tsort ?(all_rec = false) type_defs0 =
  let ignorable = [ "unit"; "bool"; "int"; "float"; "string"; "abstract" ] in
  if all_rec then
    [(true, type_defs0)]
  else
    let type_defs =
      List.map (fun (x : Ast.type_def) ->
        let deps = Ast.extract_type_names ~ignorable x.value in
        (x, deps)
      ) type_defs0
    in
    List.rev (Tsort.sort type_defs)
