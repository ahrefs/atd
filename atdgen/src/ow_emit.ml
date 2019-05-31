(*
  OCaml code generator for www-form data serialization.
*)

open Atd.Import
open Indent

open Atd.Ast
open Mapping

let target : Ocaml.target = WWW

let runtime_module = "Atdgen_runtime.Ow_run"

let runtime fmt = Printf.ksprintf (fun x -> sprintf "%s.%s" runtime_module x) fmt

let runtime_state = runtime "state"

let runtime_acc = runtime "acc"

type mapping = (Ocaml.Repr.t, Www_form.www_repr) Mapping.mapping
type field_mapping = (Ocaml.Repr.t, Www_form.www_repr) Mapping.field_mapping

type field =
  { mapping : field_mapping
  ; ocaml_fname : string
  ; www_fname : string
  ; ocaml_default : string option
  ; optional : bool
  ; unwrapped : bool
  }

type param = {
  deref : mapping -> mapping;
  unknown_field_handler : string option;
  (* Optional handler that takes a field name as argument
     and does something with it such as displaying a warning message. *)

  force_defaults : bool;

  preprocess_input : string option;
  (* intended for UTF-8 validation *)

  ocaml_version: (int * int) option;

}

let get_fields deref a =
  List.map (fun x ->
    let ocamlf, wwwf =
      match x.f_arepr, x.f_brepr with
      | Ocaml.Repr.Field o, Www_form.Field j -> o, j
      | _ -> assert false
    in
    let ocaml_fname = ocamlf.Ocaml.ocaml_fname in
    let ocaml_default =
      match x.f_kind, ocamlf.Ocaml.ocaml_default with
      | With_default, None ->
          (match Ocaml.get_implicit_ocaml_default (deref x.f_value) with
           | None -> Error.error x.f_loc "Missing default field value"
           | Some d -> Some d
          )
      | With_default, Some d -> Some d
      | Optional, _ -> Some "None"
      | Required, _ -> None
    in
    let www_fname = wwwf.Www_form.www_fname in
    let optional = not (Atd.Ast.is_required x.f_kind) in
    let unwrapped = wwwf.Www_form.www_unwrapped in
    { mapping = x
    ; ocaml_fname
    ; ocaml_default
    ; www_fname
    ; optional
    ; unwrapped
    }
  ) (Array.to_list a)

let get_assoc_type deref loc x =
  match deref x with
  | Tuple (_, [| k; v |], Ocaml.Repr.Tuple, Www_form.Tuple) ->
      if not (Ox_emit.is_string deref k.cel_value) then
        Error.error loc "Due to <www repr=\"object\"> keys must be strings";
      (k.cel_value, v.cel_value)
  | _ ->
      Error.error loc "Expected due to <www repr=\"object\">: (string * _) list"

let make_ocaml_www_form_intf ~with_create buf deref defs =
  List.concat_map snd defs
  |> List.filter Ox_emit.include_intf
  |> List.iter (fun x ->
    let s = x.def_name in
    let full_name = Ox_emit.get_full_type_name x in
    let writer_params =
      String.concat "" (
        List.map
          (fun s -> sprintf "\n  (%s -> %s -> '%s -> %s) ->" runtime_state runtime_acc s runtime_acc)
          x.def_param
      )
    in
    bprintf buf "\
val write_%s :%s
  %s -> %s -> %s -> %s
  (** Output a www-form data of type {!%s}. *)

"
      s writer_params
      runtime_state runtime_acc full_name runtime_acc
      s;

    bprintf buf "\
val www_form_of_%s :%s
  %s -> (string * string) list
  (** Serialize a value of type {!%s}
      into a www-form assoc list. *)

"
      s writer_params
      full_name
      s;
  )

(*
  ('a, 'b) t            -> write_t write__a write__b
  ('a, foo) t           -> write_t write__a write_foo
  ('a, (foo, 'b) bar) t -> write_t write__a (write_bar write_foo write__b)
*)
let rec get_writer_name
    ?(paren = false)
    ?(name_f = fun s -> "write_" ^ s)
    p (x : mapping) : string =
  match x with
    Unit (_, Ocaml.Repr.Unit, Unit) ->
      runtime "write_null"
  | Bool (_, Bool, Bool) ->
      runtime "write_bool"
  | Int (_, Int o, Int) ->
      (match o with
         Int -> runtime "write_int"
       | Char -> runtime "write_int8"
       | Int32 -> runtime "write_int32"
       | Int64 -> runtime "write_int64"
       | Float -> runtime "write_float_as_int"
      )

  | Float (_, Float, Float j) ->
      (match j with
         Float None ->
           runtime "write_float"
       | Float (Some precision) ->
           runtime "write_float_prec %i" precision
       | Int ->
           runtime "write_float_as_int"
      )

  | String (_, String, String) ->
      runtime "write_string"

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
  let args = List.map (fun s -> Tvar (dummy_loc, s)) param in
  get_writer_name p (Name (dummy_loc, name, args, None, None))

let get_left_to_string_name p name param =
  let name_f s = "www_form_of_" ^ s in
  let args = List.map (fun s -> Tvar (dummy_loc, s)) param in
  get_writer_name ~name_f p (Name (dummy_loc, name, args, None, None))


let write_with_adapter adapter writer =
  match adapter.Www_form.ocaml_adapter with
  | None -> writer
  | Some adapter_path ->
      let restore =
        Oj_mapping.json_restorer_of_adapter_path adapter_path in
      [
        Line (
          runtime "write_with_adapter %s (" restore
        );
        Block writer;
        Line ")";
      ]

let unwrap_f_value ({ mapping; unwrapped; _} : field) (p : param) =
  if unwrapped then
    Ocaml.unwrap_option (p.deref mapping.f_value)
  else
    mapping.f_value

let rec make_writer ?type_constraint p (x : mapping) : Indent.t list =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Line (get_writer_name p x) ]

  | Sum (_, a, Sum o, Sum j) ->
      let tick = Ocaml.tick o in
      let open_enum = j.Www_form.www_open_enum in
      let body : Indent.t list =
        [
          Line "match x with";
          Block (
            Array.to_list (
              Array.map
                (fun x -> Inline (make_variant_writer p ~tick ~open_enum x))
                a
            )
          )
        ]
      in
      let standard_writer = [
        Annot ("fun", Line "fun state acc x ->");
        Block body
      ] in
      let adapter = j.www_sum_adapter in
      write_with_adapter adapter standard_writer

  | Record (_, a, Record o, Record j) ->
      let xval =
        match type_constraint with
        | Some x -> sprintf "(x : %s)" x
        | None -> "x"
      in
      let standard_writer = [
        Annot ("fun", Line (sprintf "fun state acc %s ->" xval));
        Block (make_record_writer p a o);
      ] in
      let adapter = j.www_record_adapter in
      write_with_adapter adapter standard_writer

  | Tuple (_, a, Tuple, Tuple) ->
      let len = Array.length a in
      let a =
        Array.mapi (
          fun i x ->
            Inline [
              Line "let acc =";
              Block [
                Line (sprintf "let %s = x in" (Ox_emit.nth "x" i len));
                Line "(";
                Block (make_writer p x.cel_value);
                Line ") state acc x";
              ];
              Line "in";
            ]
        ) a
      in
      let l = Array.to_list a in
      [
        Annot ("fun", Line "fun state acc x ->");
        Block [
          Inline l;
          Line "acc";
        ];
      ]

  | List (loc, x, List o, List j) ->
      (match j with
         Array ->
           let write =
             match o with
               List -> runtime "write_list ("
             | Array -> runtime "write_array ("
           in
           [
             Line write;
             Block (make_writer p x);
             Line ")";
           ]

       | Object ->
           let k, v = get_assoc_type p.deref loc x in
           let write =
             match o with
               List -> runtime "write_assoc_list ("
             | Array -> runtime "write_assoc_array ("
           in
           [
             Line write;
             Block (make_writer p k);
             Line ") (";
             Block (make_writer p v);
             Line ")";
           ]
      )

  | Option (_, x, Option, Option) ->
      [
        Line (runtime "write_option (");
        Block (make_writer p x);
        Line ")";
      ]

  | Nullable (_, x, Nullable, Nullable) ->
      [
        Line (runtime "write_nullable (");
        Block (make_writer p x);
        Line ")";
      ]

  | Wrap (_, x, Wrap o, Wrap) ->
      (match o with
         None -> make_writer p x
       | Some { Ocaml.ocaml_unwrap; _} ->
           [
             Line "fun state acc x -> (";
             Block [
               Line (sprintf "let x = ( %s ) x in (" ocaml_unwrap);
               Block (make_writer p x);
               Line ") state acc x)";
             ]
           ]
      )

  | _ -> assert false


and make_variant_writer p ~tick ~open_enum x : Indent.t list =
  let o, j =
    match x.var_arepr, x.var_brepr with
      Variant o, Variant j -> o, j
    | _ -> assert false
  in
  let ocaml_cons = o.Ocaml.ocaml_cons in
  let www_cons = j.Www_form.www_cons in
  match x.var_arg with
  | None ->
      [
        Line (sprintf "| %s%s -> %s state acc %S"
                tick ocaml_cons
                (runtime "write_string")
                www_cons)
      ]
  | Some v when open_enum ->
      (* v should resolve to type string. *)
      [
        Line (sprintf "| %s%s x -> (" tick ocaml_cons);
        Block [
          Block (make_writer p v);
          Line ") state acc x";
        ];
      ]
  | Some v ->
      [
        Line (sprintf "| %s%s x ->" tick ocaml_cons);
        Block [
          Line (sprintf "let state = %s state in" (runtime "make_item"));
          Line (sprintf "let acc = %s state acc %S in" (runtime "write_string") www_cons);
          Line "(";
          Block (make_writer p v);
          Line ") state acc x";
        ]
      ]

and make_record_writer p a record_kind =
  let dot = Ocaml.dot record_kind in
  let fields = get_fields p.deref a in
  let write_fields =
    List.map (
      fun ({ mapping ; ocaml_fname ; ocaml_default
           ; www_fname ; optional ; unwrapped } as field) ->
        let f_value = unwrap_f_value field p in
        let app v =
          [
            Line "(";
            Block (make_writer p f_value);
            Line ")";
            Block [
              Line (sprintf "(%s state %S) acc %s" (runtime "make_field") www_fname v)
            ];
          ]
        in
        let v =
          if optional then
            sprintf "x.%s" ocaml_fname
          else
            sprintf "x%s%s" dot ocaml_fname
        in
        let l =
          if unwrapped then
            [
              Line (sprintf "match %s with None -> acc | Some x ->" v);
              Block (app "x");
            ]
          else if optional && not p.force_defaults then
            [
              Line (sprintf "if %s <> %s then ("
                      v (Option.value_exn ocaml_default));
              Block (app v);
              Line ") else acc"
            ]
          else
            app v
        in
        Inline [
          Line "let acc =";
          Block l;
          Line "in";
        ]
    ) fields
  in
  [
    Inline write_fields;
    Line "acc";
  ]

let make_ocaml_www_data_writer p ~original_types is_rec let1 let2 def =
  let x = Option.value_exn def.def_value in
  let name = def.def_name in
  let type_constraint = Ox_emit.get_type_constraint ~original_types def in
  let param = def.def_param in
  let write = get_left_writer_name p name param in
  let to_string = get_left_to_string_name p name param in
  let needs_annot = Ox_emit.needs_type_annot x in
  let writer_expr =
    if needs_annot
    then make_writer ~type_constraint p x
    else make_writer p x
  in
  let eta_expand = is_rec && not (Ox_emit.is_lambda writer_expr) in
  let extra_param, extra_args, type_annot =
    match eta_expand, needs_annot with
    | true, false -> " state acc x", " state acc x", None
    | true, true -> sprintf " state acc (x : %s)" type_constraint, " state acc x", None
    | false, false -> "", "", None
    | false, true -> "", "", Some (sprintf "_ -> _ -> %s -> _" type_constraint)
  in
  [
    Line (sprintf "%s %s = ("
            let1 (Ox_emit.opt_annot_def type_annot (write ^ extra_param)));
    Block (List.map Indent.strip writer_expr);
    Line (sprintf ")%s" extra_args);
    Line (sprintf "%s %s x =" let2 to_string);
    Block [
      Line (sprintf "%s (" (runtime "www_form_of_acc"));
      Block [
        Line write;
      ];
      Line ") x";
    ]
  ]

let make_ocaml_www_form_impl
    ~unknown_field_handler
    ~with_create ~preprocess_input ~original_types
    ~force_defaults ~ocaml_version
    buf deref defs =
  let p =
    { deref
    ; unknown_field_handler
    ; force_defaults
    ; preprocess_input
    ; ocaml_version
    } in
  defs
  |> List.concat_map (fun (is_rec, l) ->
    let l = List.filter (fun x -> x.def_value <> None) l in
    let writers =
      List.map_first (fun ~is_first def ->
        let let1, let2 = Ox_emit.get_let ~is_rec ~is_first in
        make_ocaml_www_data_writer p ~original_types is_rec let1 let2 def
      ) l
    in
    List.flatten writers)
  |> Indent.to_buffer buf;
  Ox_emit.maybe_write_creator_impl ~with_create deref buf defs


(*
  Translation of the types into the ocaml/www-form mapping.
*)

let check_www_sum loc www_sum_param variants =
  if www_sum_param.Www_form.www_open_enum then (
    let variants_with_arg =
      List.filter (function {var_arg = Some _; _} -> true | _ -> false) variants
    in
    match variants_with_arg with
    | [] ->
        Error.error loc
          "Missing catch-all case of the form `| Other of string`, \
           required by <www open_enum>."
    | [x] -> (
        match x.var_arg with
        | None -> assert false
        | Some (String _) -> ()
        | Some mapping ->
            let loc = Mapping.loc_of_mapping mapping in
            Error.error loc
              "The argument of this variant must be of type string \
               as imposed by <www open_enum>."
      )
    | _ ->
        Error.error loc
          "Multiple variants have arguments, which doesn't make sense \
           when combined with <www open_enum>."
  )

let rec mapping_of_expr (x : type_expr) =
  match x with
  | Sum (loc, l, an) ->
      let ocaml_t = Ocaml.Repr.Sum (Ocaml.get_ocaml_sum WWW an) in
      let www_sum_param = Www_form.get_www_sum an in
      let www_t = Www_form.Sum (Www_form.get_www_sum an) in
      let variants = List.map mapping_of_variant l in
      check_www_sum loc www_sum_param variants;
      Sum (loc, Array.of_list variants,
           ocaml_t, www_t)

  | Record (loc, l, an) ->
      let ocaml_t = Ocaml.Repr.Record (Ocaml.get_ocaml_record WWW an) in
      let ocaml_field_prefix = Ocaml.get_ocaml_field_prefix WWW an in
      let www_t = Www_form.Record (Www_form.get_www_record an) in
      Record (loc,
              Array.of_list
                (List.map (mapping_of_field ocaml_field_prefix) l),
              ocaml_t, www_t)

  | Tuple (loc, l, _) ->
      let ocaml_t = Ocaml.Repr.Tuple in
      let www_t = Www_form.Tuple in
      Tuple (loc, Array.of_list (List.map mapping_of_cell l),
             ocaml_t, www_t)

  | List (loc, x, an) ->
      let ocaml_t = Ocaml.Repr.List (Ocaml.get_ocaml_list WWW an) in
      let www_t = Www_form.List (Www_form.get_www_list an) in
      List (loc, mapping_of_expr x, ocaml_t, www_t)

  | Option (loc, x, _) ->
      let ocaml_t = Ocaml.Repr.Option in
      let www_t = Www_form.Option in
      Option (loc, mapping_of_expr x, ocaml_t, www_t)

  | Nullable (loc, x, _) ->
      let ocaml_t = Ocaml.Repr.Nullable in
      let www_t = Www_form.Nullable in
      Nullable (loc, mapping_of_expr x, ocaml_t, www_t)

  | Shared (loc, _, _) ->
      Error.error loc "Sharing is not supported by the WWW-form interface"

  | Wrap (loc, x, an) ->
      let ocaml_t =
        Ocaml.Repr.Wrap (Ocaml.get_ocaml_wrap ~type_param:[] WWW loc an) in
      let www_t = Www_form.Wrap in
      Wrap (loc, mapping_of_expr x, ocaml_t, www_t)

  | Name (loc, (_, s, l), an) ->
      (match s with
         "unit" ->
           Unit (loc, Unit, Unit)
       | "bool" ->
           Bool (loc, Bool, Bool)
       | "int" ->
           let o = Ocaml.get_ocaml_int WWW an in
           Int (loc, Int o, Int)
       | "float" ->
           let j = Www_form.get_www_float an in
           Float (loc, Float, Float j)
       | "string" ->
           String (loc, String, String)
       | s ->
           Name (loc, s, List.map mapping_of_expr l, None, None)
      )
  | Tvar (loc, s) ->
      Tvar (loc, s)

and mapping_of_cell (cel_loc, x, an) =
  { cel_loc
  ; cel_value = mapping_of_expr x
  ; cel_arepr =
      Ocaml.Repr.Cell
        { Ocaml.ocaml_default = Ocaml.get_ocaml_default WWW an
        ; ocaml_fname = ""
        ; ocaml_mutable = false
        ; ocaml_fdoc = Atd.Doc.get_doc cel_loc an
        }
  ; cel_brepr = Www_form.Cell
  }

and mapping_of_variant = function
  | Inherit _ -> assert false
  | Variant (var_loc, (var_cons, an), o) ->
      { var_loc
      ; var_cons
      ; var_arg = Option.map mapping_of_expr o
      ; var_arepr = Ocaml.Repr.Variant
            { Ocaml.ocaml_cons = Ocaml.get_ocaml_cons WWW var_cons an
            ; ocaml_vdoc = Atd.Doc.get_doc var_loc an
            }
      ; var_brepr =
          Www_form.Variant
            { Www_form.www_cons = Www_form.get_www_cons var_cons an
            }
      }

and mapping_of_field ocaml_field_prefix = function
  | `Inherit _ -> assert false
  | `Field (f_loc, (f_name, f_kind, an), x) ->
      let { Ox_mapping.ocaml_default; unwrapped } =
        Ox_mapping.analyze_field WWW f_loc f_kind an in
      { f_loc
      ; f_name
      ; f_kind
      ; f_value = mapping_of_expr x
      ; f_arepr = Ocaml.Repr.Field
            { Ocaml.ocaml_default
            ; ocaml_fname =
                Ocaml.get_ocaml_fname WWW (ocaml_field_prefix ^ f_name) an
            ; ocaml_mutable = Ocaml.get_ocaml_mutable WWW an
            ; ocaml_fdoc = Atd.Doc.get_doc f_loc an
            }
      ; f_brepr = Www_form.Field
            { Www_form.www_fname = Www_form.get_www_fname f_name an
            ; www_unwrapped = unwrapped
            }
      }

let defs_of_atd_modules l =
  List.map (fun (is_rec, l) ->
    ( is_rec
    , List.map (function Atd.Ast.Type atd ->
        Ox_emit.def_of_atd atd ~target ~external_:Www_form.External
          ~mapping_of_expr ~def:Www_form.Def
      ) l
    )
  ) l


(*
  Glue
*)
let make_ocaml_files ~unknown_field_handler ~preprocess_input =
  Ox_emit.make_ocaml_files
    ~defs_of_atd_modules
    ~make_ocaml_intf:make_ocaml_www_form_intf
    ~make_ocaml_impl:(make_ocaml_www_form_impl ~unknown_field_handler ~preprocess_input)
    ~target:WWW
