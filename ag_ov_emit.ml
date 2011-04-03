(*
  Validators of OCaml data whose types are defined using ATD.
*)

open Atd_ast
open Ag_error

let name_of_var s = "_" ^ s


let make_ocaml_validate_intf buf deref defs =
  List.iter (
    fun x ->
      let s = x.def_name in
      if s <> "" && s.[0] <> '_' && x.def_value <> None then (
        let full_name = Ag_ocaml.get_full_type_name x in
        let validator_params =
          String.concat "" (
            List.map
              (fun s -> sprintf "\n  ('%s -> bool) ->" s)
              x.def_param
          )
        in
	bprintf buf "\
val validate_%s :%s
  %s -> bool
  (** Validate a value of type {!%s}. *)

"
          s writer_params
          full_name
          s;
      )
  ) (flatten defs)

(*
  Glue
*)

let translate_mapping (l : (bool * Atd_ast.module_body) list) =
  defs_of_atd_modules l

let write_opens buf l =
  List.iter (fun s -> bprintf buf "open %s\n" s) l;
  bprintf buf "\n"

let make_mli
    ~header ~opens ~with_typedefs ~with_fundefs
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_validate_intf buf deref defs;
  Buffer.contents buf

let make_ml
    ~header ~opens ~with_typedefs ~with_fundefs
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_validate_impl buf deref defs;
  Buffer.contents buf

let make_ocaml_files
    ~opens
    ~with_typedefs 
    ~with_fundefs
    ~all_rec
    ~std
    ~unknown_field_handler
    ~pos_fname
    ~pos_lnum
    ~type_aliases
    atd_file out =
  let head, m0 =
    match atd_file with
        Some file ->
          Atd_util.load_file
            ~expand:false ~inherit_fields:true ~inherit_variants:true
            ?pos_fname ?pos_lnum
            file
      | None ->
          Atd_util.read_channel
            ~expand:false ~inherit_fields:true ~inherit_variants:true
            ?pos_fname ?pos_lnum
            stdin
  in
  let m1 =
    if all_rec then
      [ (true, m0) ]
    else
      Atd_util.tsort m0
  in
  let defs1 = translate_mapping m1 in
  Ag_ox_emit.check defs1;
  let m2 = Atd_util.tsort (Atd_expand.expand_module_body ~keep_poly:true m0) in
  (* m0 = original type definitions
     m1 = original type definitions after dependency analysis
     m2 = monomorphic type definitions after dependency analysis *)
  let ocaml_typedefs =
    Ag_ocaml.ocaml_of_atd ~target:`Default ~type_aliases (head, m1) in
  let defs = translate_mapping m2 in
  let header = 
    let src =
      match atd_file with
          None -> "stdin"
        | Some path -> sprintf "%S" (Filename.basename path)
    in
    sprintf "(* Auto-generated from %s *)\n" src
  in
  let mli = 
    make_mli ~header ~opens ~with_typedefs ~with_fundefs
      ocaml_typedefs (Ag_mapping.make_deref defs1) defs1
  in
  let ml =
    make_ml ~header ~opens ~with_typedefs ~with_fundefs
      ocaml_typedefs (Ag_mapping.make_deref defs) defs
  in
  Ag_ox_emit.write_ocaml out mli ml
