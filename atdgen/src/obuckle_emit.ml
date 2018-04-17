open Atd.Import
open Indent

type param =
  { deref
    : (Ocaml.Repr.t, Json.json_repr) Mapping.mapping
      -> (Ocaml.Repr.t, Json.json_repr) Mapping.mapping;
  }


let runtime_module = "Atdgen_codec_runtime"

let decoder_ident = sprintf "%s.Decode.%s" runtime_module

let encoder_ident = sprintf "%s.Encode.%s" runtime_module

let decoder_make = decoder_ident "make"

let encoder_make = encoder_ident "make"

let decoder_t s = sprintf "%s %s" s (decoder_ident "t")

let encoder_t s = sprintf "%s %s" s (encoder_ident "t")

let make_json_string s = Yojson.Safe.to_string (`String s)

let destruct_sum (x : Oj_mapping.t) =
  let open Mapping in
  match x with
  | Sum (_, a, Sum x, Sum) ->
      let tick = Ocaml.tick x in
      tick, a
  | Unit _ -> Error.error (loc_of_mapping x) "Cannot destruct unit"
  | Bool _ -> Error.error (loc_of_mapping x) "Cannot destruct bool"
  | Int _ -> Error.error (loc_of_mapping x) "Cannot destruct int"
  | Float _ -> Error.error (loc_of_mapping x) "Cannot destruct float"
  | String _ -> Error.error (loc_of_mapping x) "Cannot destruct string"
  | Name (_,name,_,_,_) ->
      Error.error (loc_of_mapping x) ("Cannot destruct name " ^ name)
  | External _ -> Error.error (loc_of_mapping x) "Cannot destruct external"
  | Tvar _ -> Error.error (loc_of_mapping x) "Cannot destruct tvar"
  | Record _ -> Error.error (loc_of_mapping x) "Cannot destruct record"
  | Tuple _ -> Error.error (loc_of_mapping x) "Cannot destruct tuple"
  | List _ -> Error.error (loc_of_mapping x) "Cannot destruct list"
  | Option _ -> Error.error (loc_of_mapping x) "Cannot destruct option"
  | Nullable _ -> Error.error (loc_of_mapping x) "Cannot destruct nullable"
  | Wrap _ -> Error.error (loc_of_mapping x) "Cannot destruct wrap"
  | _ -> Error.error (loc_of_mapping x) "Cannot destruct unknown type"


let make_ocaml_bs_intf buf _deref defs =
  List.concat_map snd defs
  |> List.filter Ox_emit.include_intf
  |> List.iter (fun (x : (_, _) Mapping.def) ->
    let s = x.def_name in
    let full_name = Ox_emit.get_full_type_name x in
    let params t =
      String.concat " " (
        List.map (fun s -> sprintf "%s ->" (t ("'" ^ s))) x.def_param
      ) in
    let read_params = params decoder_t in
    let write_params = params encoder_t in
    bprintf buf "val read_%s : %s %s\n\n"
      s read_params (decoder_t full_name);
    bprintf buf "val write_%s : %s %s\n\n"
      s write_params (encoder_t full_name)
  )

let rec get_reader_name
    ?(paren = false)
    ?(name_f = fun s -> "read_" ^ s)
    p (x : Oj_mapping.t) : string =
  match x with
    Unit (_, Unit, Unit) -> decoder_ident "unit"
  | Bool (_, Bool, Bool) -> decoder_ident "bool"
  | Int (_, Int o, Int) ->
      decoder_ident (
        match o with
        | Int -> "int"
        | Char ->  "char"
        | Int32 -> "int32"
        | Int64 -> "int64"
        | Float -> "float"
      )
  | Float (_, Float, Float _) -> decoder_ident "float"
  | String (_, String, String) -> decoder_ident "string"
  | Tvar (_, s) -> "read_" ^ Ox_emit.name_of_var s

  | Name (_, s, args, None, None) ->
      let l = List.map (get_reader_name ~paren:true p) args in
      let s = String.concat " " (name_f s :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | External (_, _, args,
              External (_, main_module, ext_name),
              External) ->
      let f = main_module ^ "." ^ name_f ext_name in
      let l = List.map (get_reader_name ~paren:true p) args in
      let s = String.concat " " (f :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | _ -> assert false

let rec make_reader p type_annot (x : Oj_mapping.t) : Indent.t list =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Indent.Line (get_reader_name p x) ]
  | Record (loc, a, Record o, Record j) ->
      Ocaml.obj_unimplemented loc o;
      [ Annot ("fun", Line (sprintf "%s (fun json ->" decoder_make))
      ; Block (make_record_reader p type_annot loc a j)
      ; Line ")"
      ]
  | Tuple (_, a, Tuple, Tuple) ->
      [ Line (decoder_ident (sprintf "tuple%d" (Array.length a)))
      ; Block (
          a
          |> Array.to_list
          |> List.map (fun (cm : (_, _) Mapping.cell_mapping) ->
            Block (make_reader p None cm.cel_value)
            |> Indent.paren
          )
        )
      ]
  | List (_, x, List o, List j) ->
      let () =
        match j with
        | Object -> failwith "Unable to read objects into lists"
        | Array -> () in
      [ Line (sprintf "%s ("
                (match o with
                 | List -> decoder_ident "list"
                 | Array -> decoder_ident "array"))
      ; Block (make_reader p None x)
      ; Line ")"
      ]
  | Sum (_, a, Sum osum, Sum) ->
      let cases =
        Array.to_list a
        |> List.map
          (fun (r : (Ocaml.Repr.t, Json.json_repr) Mapping.variant_mapping) ->
             let (o, j) =
               match r.var_arepr, r.var_brepr with
               | Ocaml.Repr.Variant o, Json.Variant j -> o, j
               | _ -> assert false in
             ( r.var_arg
             , (match j.json_cons with
                | None -> o.ocaml_cons
                | Some j -> j)
             , o.ocaml_cons
             )
          ) in
      let cases =
        let tick = Ocaml.tick osum in
        cases
        |> List.concat_map (fun (arg, j, o) ->
          let codec_cons =
            match arg with
            | None ->
                [Line (sprintf "`Single (%s%s)" tick o)]
            | Some v ->
                [ Line "`Decode ("
                ; Inline (make_reader p None v)
                ; Line (
                    sprintf "|> %s (fun x -> %s%s x)" (decoder_ident "map") tick o
                  )
                ; Line ")"
                ]
          in
          [Block
             [ Line "("
             ; Line (sprintf "%S" j)
             ; Line ","
             ; Block codec_cons
             ; Line ")"
             ]
          ])
        |> Indent.concat (Line ";")
      in
      [ Line (decoder_ident "enum")
      ; Line "["
      ; Block cases
      ; Line "]"
      ]
  | Wrap (_, x, Wrap o, Wrap) ->
      (match o with
       | None -> make_reader p type_annot x
       | Some w ->
           [ Line "("
           ; Block (make_reader p type_annot x)
           ; Line (sprintf ") |> (%s (%s))" (decoder_ident "map") w.ocaml_wrap)
           ])
  | Option (_, x, Option, Option) ->
      [ Line (sprintf "%s (" (decoder_ident "option_as_constr"))
      ; Block (make_reader p type_annot x)
      ; Line ")"
      ]
  | Nullable (_, _, Nullable, Nullable) -> failwith "TODO: Nullable"
  | _ -> failwith "TODO: make reader"

and make_record_reader
    (p : param)
    _type_annot
    _loc
    (a : (Ocaml.Repr.t, Json.json_repr) Mapping.field_mapping array)
    _json_options
  =
  let create_record =
    Array.map (function (x : (_, _) Mapping.field_mapping) ->
    match x.f_arepr, x.f_brepr with
    | Ocaml.Repr.Field o, Json.Field _ ->
        let oname = o.Ocaml.ocaml_fname in
        Block
          [ Line (sprintf "%s =" oname)
          ;  Block
              [ Line (decoder_ident "decode")
              ; Line "("
              ; Block
                  [ Inline (make_reader p None x.f_value)
                  ; Line (sprintf "|> %s \"%s\"" (decoder_ident "field") x.f_name)
                  ]
              ; Line ") json;"
              ]
          ]
    | _ -> assert false
    ) a
    |> Array.to_list
  in
  [ Line "("
  ; Block
      [ Line "{"
      ; Block create_record
      ; Line "}"
      ]
  ; Line ")"
  ]

let get_left_reader_name p name param =
  let args = List.map (fun s -> Mapping.Tvar (Atd.Ast.dummy_loc, s)) param in
  get_reader_name p (Mapping.Name (Atd.Ast.dummy_loc, name, args, None, None))

let make_ocaml_bs_reader p ~original_types is_rec let1 _let2
    (def : (_, _) Mapping.def) =
  let x = match def.def_value with None -> assert false | Some x -> x in
  let name = def.def_name in
  let type_constraint = Ox_emit.get_type_constraint ~original_types def in
  let param = def.def_param in
  let read = get_left_reader_name p name param in
  let type_annot =
    match Ox_emit.needs_type_annot x with
    | true -> Some type_constraint
    | false -> None
  in
  let reader_expr = make_reader p type_annot x in
  let eta_expand = is_rec && not (Ox_emit.is_function reader_expr) in
  let extra_param, extra_args =
    if eta_expand then " js", " js"
    else "", ""
  in
  [
    Line (sprintf "%s %s%s = (" let1 read extra_param);
    Block (List.map Indent.strip reader_expr);
    Line (sprintf ")%s" extra_args);
  ]

let rec get_writer_name
    ?(paren = false)
    ?(name_f = fun s -> "write_" ^ s)
    (p : param) (x : Oj_mapping.t) : string =
  match x with
  | Unit (_, Ocaml.Repr.Unit, Unit) ->
      encoder_ident "unit"
  | Bool (_, Bool, Bool) ->
      encoder_ident "bool"
  | Int (_, Int o, Int) ->
      encoder_ident (
        match o with
        | Int -> "int"
        | Char ->  "char"
        | Int32 -> "int32"
        | Int64 -> "int64"
        | Float -> "float"
      )
  | Float (_, Float, Float j) ->
      encoder_ident (
        match j with
        | Float None -> "float"
          (* TODO *)
        | Float (Some _precision) -> sprintf "float"
        | Int -> "float"
      )

  | String (_, String, String) -> encoder_ident "string"

  | Tvar (_, s) -> "write_" ^ (Ox_emit.name_of_var s)

  | Name (_, s, args, None, None) ->
      let l = List.map (get_writer_name ~paren:true p) args in
      let s = String.concat " " (name_f s :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | External (_, _, args,
              External (_, main_module, ext_name),
              External) ->
      let f = main_module ^ "." ^ name_f ext_name in
      let l = List.map (get_writer_name ~paren:true p) args in
      let s = String.concat " " (f :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | _ -> assert false

let get_left_writer_name p name param =
  let args = List.map (fun s -> Mapping.Tvar (Atd.Ast.dummy_loc, s)) param in
  get_writer_name p (Name (Atd.Ast.dummy_loc, name, args, None, None))

let rec make_writer p (x : Oj_mapping.t) : Indent.t list =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Line (get_writer_name p x) ]
  | Tuple (_, a, Tuple, Tuple) ->
      [ Line (encoder_ident (sprintf "tuple%d" (Array.length a)))
      ; Block (
          Array.to_list a
          |> List.map (fun (cm : (_, _) Mapping.cell_mapping) ->
            Block (make_writer p cm.cel_value)
            |> Indent.paren
          )
        )
      ]
  | List (_, x, List o, List _) ->
      [ Line (sprintf "%s ("
                (match o with
                 | List -> encoder_ident "list"
                 | Array -> encoder_ident "array"))
      ; Block (make_writer p x)
      ; Line ")"
      ]
  | Record (_, a, Record o, Record _) ->
      [ Annot ("fun", Line (sprintf "%s (fun t ->" encoder_make))
      ; Block (make_record_writer p a o)
      ; Line ")"
      ]
  | Sum (_, _a, Sum _osum, Sum) ->
      make_sum_writer p x
  | Wrap (_, x, Wrap o, Wrap) ->
      begin match o with
        | None -> make_writer p x
        | Some { Ocaml.ocaml_unwrap ; _ } ->
            [ Block (make_writer p x)
            ; Line (sprintf "|> %s (%s)" (encoder_ident "contramap")
                      ocaml_unwrap)
            ]
      end
  | Nullable (_, x, Nullable, Nullable) ->
      [ Line (sprintf "%s (" (encoder_ident "nullable"))
      ; Block (make_writer p x)
      ; Line ")"
      ]
  | _ -> []

and make_record_writer p a _record_kind =
  let write_record =
    a
    |> Array.map (fun (x : (_, _) Mapping.field_mapping) ->
      match x.f_arepr, x.f_brepr with
      | Ocaml.Repr.Field o, Json.Field _ ->
          let oname = o.Ocaml.ocaml_fname in
          Block
            [ Line (sprintf "%S,"  x.f_name)
            ; Block
                [ Line (sprintf "%s" (encoder_ident "encode"))
                ; Line "("
                ; Inline (make_writer p x.f_value)
                ; Line ")"
                ; Line (sprintf "t.%s" oname)
                ]
            ]
      | _ -> assert false
    )
    |> Array.to_list
    |> Indent.concat (Line ";") in
  [ Line "("
  ; Line (encoder_ident "obj")
  ; Block
      [ Line "["
      ; Block write_record
      ; Line "]"
      ]
  ; Line ")"
  ]

and make_sum_writer (p : param)
    (sum : (Ocaml.Repr.t, Json.json_repr) Mapping.mapping) =
  let tick, a = destruct_sum (p.deref sum) in
  let cases =
    a
    |> Array.map (
      fun (x : (Ocaml.Repr.t, Json.json_repr) Mapping.variant_mapping) ->
        let o, j =
          match x.var_arepr, x.var_brepr with
          | Ocaml.Repr.Variant o, Json.Variant j -> o, j
          | _ -> assert false in
        let ocaml_cons = o.Ocaml.ocaml_cons in
        let json_cons =
          match j.Json.json_cons with
          | None -> failwith "TODO"
          | Some json_cons -> json_cons in
        Inline (
          begin match x.var_arg with
            | None ->
                [ Line (sprintf "| %s%s ->" tick ocaml_cons)
                ; Line (sprintf "%s %s" (encoder_ident "constr0")
                          (make_json_string json_cons))
                ]
            | Some v ->
                [ Line (sprintf "| %s%s x ->" tick ocaml_cons)
                ; Line (sprintf "%s %s (" (encoder_ident "constr1")
                          (make_json_string json_cons))
                ; Block (make_writer p v)
                ; Line ") x"
                ]
          end)
    )
    |> Array.to_list
  in
  [ Line (encoder_ident "make (function")
  ; Block cases
  ; Line ")"]

let make_ocaml_bs_writer p ~original_types:_ is_rec let1 _let2
    (def : (_, _) Mapping.def) =
  let x = match def.def_value with None -> assert false | Some x -> x in
  let name = def.def_name in
  let param = def.def_param in
  let read = get_left_writer_name p name param in
  let writer_expr = make_writer p x in
  let eta_expand = is_rec && not (Ox_emit.is_function writer_expr) in
  let extra_param, extra_args =
    if eta_expand then " js", " js"
    else "", ""
  in
  [
    Line (sprintf "%s %s%s = (" let1 read extra_param);
    Block (List.map Indent.strip writer_expr);
    Line (sprintf ")%s" extra_args);
  ]

let make_ocaml_bs_impl
    ~original_types
    buf deref defs =
  let p = {deref = deref;} in
  defs
  |> List.concat_map (fun (is_rec, l) ->
    let l = List.filter
        (fun (x : (Ocaml.Repr.t, Json.json_repr) Mapping.def) ->
           x.def_value <> None) l in
    let writers =
      List.map_first (fun ~is_first def ->
        let let1, let2 = Ox_emit.get_let ~is_rec ~is_first in
        make_ocaml_bs_writer p ~original_types is_rec let1 let2 def
      ) l in
    let readers =
      List.map_first (fun ~is_first def ->
        let let1, let2 = Ox_emit.get_let ~is_rec ~is_first in
        make_ocaml_bs_reader p ~original_types is_rec let1 let2 def
      ) l
    in
    List.flatten (writers @ readers))
  |> Indent.to_buffer buf

let make_ml
    ~opens
    ~header
    ~original_types
    _ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  Ox_emit.write_opens buf opens;
  make_ocaml_bs_impl ~original_types buf deref defs;
  Buffer.contents buf

let make_mli
    ~opens
    ~header
    ~original_types:_
    _ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  Ox_emit.write_opens buf opens;
  make_ocaml_bs_intf buf deref defs;
  Buffer.contents buf

let make_ocaml_files
    ~opens
    ~all_rec
    ~pos_fname
    ~pos_lnum
    ~type_aliases
    atd_file out =
  let ((head, m0), _) =
    match atd_file with
      Some file ->
        Atd.Util.load_file
          ~expand:false ~inherit_fields:true ~inherit_variants:true
          ?pos_fname ?pos_lnum
          file
    | None ->
        Atd.Util.read_channel
          ~expand:false ~inherit_fields:true ~inherit_variants:true
          ?pos_fname ?pos_lnum
          stdin
  in

  let tsort =
    if all_rec then
      function m -> [ (true, m) ]
    else
      Atd.Util.tsort
  in
  let m1 = tsort m0 in
  let (m1', original_types) =
    Atd.Expand.expand_module_body ~keep_poly:true m0
  in
  let m2 = tsort m1' in
  (* m0 = original type definitions
     m1 = original type definitions after dependency analysis
     m2 = monomorphic type definitions after dependency analysis *)
  let ocaml_typedefs =
    Ocaml.ocaml_of_atd ~pp_convs:(Ppx []) ~target:Json ~type_aliases (head, m1) in
  let defs = Oj_mapping.defs_of_atd_modules m2 in
  let header =
    let src =
      match atd_file with
        None -> "stdin"
      | Some path -> sprintf "%S" (Filename.basename path)
    in
    sprintf {|(* Auto-generated from %s *)
              [@@@ocaml.warning "-27-32-35-39"]|} src
  in
  let ml =
    make_ml ~opens ~header ~original_types
      ocaml_typedefs (Mapping.make_deref defs) defs
  in
  let mli =
    make_mli ~opens ~header ~original_types
      ocaml_typedefs (Mapping.make_deref defs) defs
  in
  Ox_emit.write_ocaml out mli ml

let make_ocaml_files
    ~opens
    ~with_typedefs:_
    ~with_create:_
    ~with_fundefs:_
    ~all_rec
    ~pos_fname
    ~pos_lnum
    ~type_aliases
    ~force_defaults:_
    ~name_overlap:_
    ~ocaml_version:_
    ~pp_convs:_
    atd_file out =
  make_ocaml_files ~opens ~all_rec ~pos_fname ~pos_lnum ~type_aliases atd_file
    out
