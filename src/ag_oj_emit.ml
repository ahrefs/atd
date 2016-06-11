(*
  OCaml code generator for the json format.
*)


open Printf

open Atd_ast
open Ag_error
open Ag_mapping
open Ag_oj_mapping

(*
  OCaml code generator (json readers and writers)
*)

let name_of_var s = "_" ^ s

type param = {
  deref : (Ag_ocaml.atd_ocaml_repr, Ag_json.json_repr) Ag_mapping.mapping ->
               (Ag_ocaml.atd_ocaml_repr, Ag_json.json_repr) Ag_mapping.mapping;
  std : bool;
  unknown_field_handler : string option;
    (* Optional handler that takes a field name as argument
       and does something with it such as displaying a warning message. *)
  constr_mismatch_handler : string option;
    (* Optional handler that takes a constructor field name, a
       constructor field value, a value field name, and a value field
       value as arguments and does something with it such as displaying a
       warning message. *)

  force_defaults : bool;

  preprocess_input : string option;
    (* intended for UTF-8 validation *)

  ocaml_version: (int * int) option;

}


let make_ocaml_json_intf ~with_create buf deref defs =
  List.iter (
    fun x ->
      let s = x.def_name in
      if s <> "" && s.[0] <> '_' && x.def_value <> None then (
        let full_name = Ag_ox_emit.get_full_type_name x in
        let writer_params =
          String.concat "" (
            List.map
              (fun s -> sprintf "\n  (Bi_outbuf.t -> '%s -> unit) ->" s)
              x.def_param
          )
        in
        let reader_params =
          String.concat "" (
            List.map
              (fun s ->
                 sprintf "\n  (Yojson.Safe.lexer_state -> \
                               Lexing.lexbuf -> '%s) ->" s)
              x.def_param
          )
        in
        bprintf buf "\
val write_%s :%s
  Bi_outbuf.t -> %s -> unit
  (** Output a JSON value of type {!%s}. *)

"
          s writer_params
          full_name
          s;

        bprintf buf "\
val string_of_%s :%s
  ?len:int -> %s -> string
  (** Serialize a value of type {!%s}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

"
          s writer_params
          full_name
          s;

        bprintf buf "\
val read_%s :%s
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> %s
  (** Input JSON data of type {!%s}. *)

"
          s reader_params
          full_name
        s;

        bprintf buf "\
val %s_of_string :%s
  string -> %s
  (** Deserialize JSON data of type {!%s}. *)

"
          s reader_params
          full_name
          s;

        if with_create && Ag_ox_emit.is_exportable x then
          let create_record_intf, create_record_impl =
            Ag_ox_emit.make_record_creator deref x
          in
          bprintf buf "%s" create_record_intf;
          bprintf buf "\n";
      )
  )
    (flatten defs)

let is_json_string deref x =
  (*
    Calling 'unwrap' allows us to ignore 'wrap' constructors
    and determine that the JSON representation is a string.
    This assumes that no '<json>' annotation imposes
    another representation for the JSON string.
  *)
  match Ag_mapping.unwrap deref x with
  | `String _ -> true
  | _ -> false (* or maybe we just don't know *)

let get_assoc_type deref loc x =
  match deref x with
  | `Tuple (loc2, [| k; v |], `Tuple, `Tuple) ->
      if not (is_json_string deref k.cel_value) then
        error loc "Due to <json repr=\"object\"> keys must be strings";
      (k.cel_value, v.cel_value)
  | _ ->
      error loc "Expected due to <json repr=\"object\">: (string * _) list"


let nth name i len =
  let l =
    Array.to_list (Array.init len (fun j -> if i = j then name else "_")) in
  String.concat ", " l

type default_field =
| Default of string
| Checked of int

type parse_field = {
  mapping     : (o, j) field_mapping;
  default     : default_field;
  ocamlf      : Ag_ocaml.atd_ocaml_field;
  jsonf       : Ag_json.json_field;
  field_ref   : string;
  constructor : int option;
  payloads    : int list;
  implicit    : bool;
}

(* identifiers can't begin with digits *)
let implicit_field_name jname = "0jic_"^jname

let get_fields p a =
  let k, acc = Array.fold_left (fun (k,acc) (i, x) ->
    let ocamlf, default, jsonf, k =
      match x.f_arepr, x.f_brepr with
        `Field o, `Field j ->
          (match x.f_kind with
            `With_default ->
              (match o.Ag_ocaml.ocaml_default with
                None ->
                  let d =
                    Ag_ocaml.get_implicit_ocaml_default
                      p.deref x.f_value in
                  (match d with
                  | None -> error x.f_loc "Missing default field value"
                  | Some d -> o, Default d, j, k)
              | Some d -> o, Default d, j, k
              )
          | `Optional -> o, Default "None", j, k
          | `Required -> o, Checked k, j, k+1
          )
      | _ -> assert false
    in
    let field_ref = "field_"^ocamlf.Ag_ocaml.ocaml_fname in
    let constructor = None in
    let payloads = [] in
    k, {
      mapping=x; default; ocamlf; jsonf;
      field_ref; constructor; payloads; implicit=false;
    }::acc
  ) (0,[]) (Array.mapi (fun i x -> (i, x)) a) in

  let fc = List.length acc in
  let fm = Hashtbl.create fc in
  let jfdir = Hashtbl.create fc in
  let neg_one = List.fold_left (fun n f ->
    Hashtbl.replace fm n f;
    Hashtbl.replace jfdir f.jsonf.Ag_json.json_fname n;
    n - 1
  ) (fc - 1) acc in
  assert (neg_one = -1);

  let existing_constr constr =
    try Some (Hashtbl.find jfdir constr)
    with Not_found -> None
  in
  (* Add implicit fields and index the deconstructed/tag field relations *)
  let _k = Hashtbl.fold (fun i { jsonf = {Ag_json.json_tag_field} } k ->
    match json_tag_field with
    | None -> k
    | Some constr ->
      let field = Hashtbl.find fm i in
      match existing_constr constr with
      | Some c_i ->
        let consf = Hashtbl.find fm c_i in
        Hashtbl.replace fm i   { field with constructor = Some c_i };
        Hashtbl.replace fm c_i { consf with payloads = i::consf.payloads };
        k
      | None -> (* Synthesize implicit field *)
        let c_i = Hashtbl.length fm in
        let f_name = implicit_field_name constr in
        let ocamlf = {
          Ag_ocaml.ocaml_fname = f_name;
          ocaml_default = None;
          ocaml_mutable = false;
          ocaml_fdoc = None;
        } in
        let jsonf = {
          Ag_json.json_fname = constr;
          json_tag_field = None;
          json_unwrapped = false;
        } in
        let synloc = (Lexing.dummy_pos, Lexing.dummy_pos) in
        let mapping = {
          f_loc = synloc;
          f_name = f_name;
          f_kind = `Required;
          f_value = `String (synloc, `String, `String);
          f_arepr = `Field ocamlf;
          f_brepr = `Field jsonf;
        } in
        let imp = {
          mapping = mapping;
          default = Checked k;
          ocamlf = ocamlf;
          jsonf = jsonf;
          field_ref = "field_"^f_name;
          constructor = None;
          payloads = [i];
          implicit = true;
        } in
        Hashtbl.replace fm i   { field with constructor = Some c_i };
        Hashtbl.replace fm c_i imp;
        Hashtbl.replace jfdir constr c_i;
        k
  ) (Hashtbl.copy fm) k
  in
  let a = Array.make (Hashtbl.length fm) (Hashtbl.find fm 0) in
  Array.iteri (fun n _ -> a.(n) <- Hashtbl.find fm n) a;
  a

let insert sep l =
  let rec ins sep = function
      [] -> []
    | x :: l -> sep :: x :: ins sep l
  in
  match l with
      [] -> []
    | x :: l -> x :: ins sep l


let make_json_string s = Yojson.Safe.to_string (`String s)

let unopt = function None -> assert false | Some x -> x


(*
  ('a, 'b) t            -> write_t write__a write__b
  ('a, foo) t           -> write_t write__a write_foo
  ('a, (foo, 'b) bar) t -> write_t write__a (write_bar write_foo write__b)
*)
let rec get_writer_name
    ?(paren = false)
    ?(name_f = fun s -> "write_" ^ s)
    p (x : oj_mapping) : string =
  match x with
      `Unit (loc, `Unit, `Unit) ->
        "Yojson.Safe.write_null"
    | `Bool (loc, `Bool, `Bool) ->
        "Yojson.Safe.write_bool"
    | `Int (loc, `Int o, `Int) ->
        (match o with
             `Int -> "Yojson.Safe.write_int"
           | `Char -> "Ag_oj_run.write_int8"
           | `Int32 -> "Ag_oj_run.write_int32"
           | `Int64 -> "Ag_oj_run.write_int64"
           | `Float -> "Ag_oj_run.write_float_as_int"
        )

    | `Float (loc, `Float, `Float j) ->
        (match j with
            `Float None ->
              if p.std then "Yojson.Safe.write_std_float"
              else "Yojson.Safe.write_float"
          | `Float (Some precision) ->
              if p.std then
                sprintf "Yojson.Safe.write_std_float_prec %i" precision
              else
                sprintf "Yojson.Safe.write_float_prec %i" precision
          | `Int ->
              "Ag_oj_run.write_float_as_int"
        )

    | `String (loc, `String, `String) ->
        "Yojson.Safe.write_string"

    | `Tvar (loc, s) -> "write_" ^ name_of_var s

    | `Name (loc, s, args, None, None) ->
        let l = List.map (get_writer_name ~paren:true p) args in
        let s = String.concat " " (name_f s :: l) in
        if paren && l <> [] then "(" ^ s ^ ")"
        else s

    | `External (loc, s, args,
                 `External (types_module, main_module, ext_name),
                 `External) ->
        let f = main_module ^ "." ^ name_f ext_name in
        let l = List.map (get_writer_name ~paren:true p) args in
        let s = String.concat " " (f :: l) in
        if paren && l <> [] then "(" ^ s ^ ")"
        else s

    | _ -> assert false


let get_left_writer_name p name param =
  let args = List.map (fun s -> `Tvar (dummy_loc, s)) param in
  get_writer_name p (`Name (dummy_loc, name, args, None, None))

let get_left_to_string_name p name param =
  let name_f s = "string_of_" ^ s in
  let args = List.map (fun s -> `Tvar (dummy_loc, s)) param in
  get_writer_name ~name_f p (`Name (dummy_loc, name, args, None, None))


let rec get_reader_name
    ?(paren = false)
    ?(name_f = fun s -> "read_" ^ s)
    p (x : oj_mapping) : string =

  match x with
      `Unit (loc, `Unit, `Unit) -> "Ag_oj_run.read_null"
    | `Bool (loc, `Bool, `Bool) -> "Ag_oj_run.read_bool"
    | `Int (loc, `Int o, `Int) ->
        (match o with
             `Int -> "Ag_oj_run.read_int"
           | `Char -> "Ag_oj_run.read_int8"
           | `Int32 -> "Ag_oj_run.read_int32"
           | `Int64 -> "Ag_oj_run.read_int64"
           | `Float -> "Ag_oj_run.read_number"
        )

    | `Float (loc, `Float, `Float j) -> "Ag_oj_run.read_number"

    | `String (loc, `String, `String) -> "Ag_oj_run.read_string"

    | `Tvar (loc, s) -> "read_" ^ name_of_var s

    | `Name (loc, s, args, None, None) ->
        let l = List.map (get_reader_name ~paren:true p) args in
        let s = String.concat " " (name_f s :: l) in
        if paren && l <> [] then "(" ^ s ^ ")"
        else s

    | `External (loc, s, args,
                 `External (types_module, main_module, ext_name),
                 `External) ->
        let f = main_module ^ "." ^ name_f ext_name in
        let l = List.map (get_reader_name ~paren:true p) args in
        let s = String.concat " " (f :: l) in
        if paren && l <> [] then "(" ^ s ^ ")"
        else s

    | _ -> assert false


let get_left_reader_name p name param =
  let args = List.map (fun s -> `Tvar (dummy_loc, s)) param in
  get_reader_name p (`Name (dummy_loc, name, args, None, None))

let get_left_of_string_name p name param =
  let name_f s = s ^ "_of_string" in
  let args = List.map (fun s -> `Tvar (dummy_loc, s)) param in
  get_reader_name ~name_f p (`Name (dummy_loc, name, args, None, None))

let destruct_sum (x : oj_mapping) =
  match x with
      `Sum (loc, a, `Sum x, `Sum) ->
        let tick = match x with `Classic -> "" | `Poly -> "`" in
        tick, a
    | `Unit _ -> error (loc_of_mapping x) "Cannot destruct unit"
    | `Bool _ -> error (loc_of_mapping x) "Cannot destruct bool"
    | `Int _ -> error (loc_of_mapping x) "Cannot destruct int"
    | `Float _ -> error (loc_of_mapping x) "Cannot destruct float"
    | `String _ -> error (loc_of_mapping x) "Cannot destruct string"
    | `Name (_,name,_,_,_) ->
      error (loc_of_mapping x) ("Cannot destruct name " ^ name)
    | `External _ -> error (loc_of_mapping x) "Cannot destruct external"
    | `Tvar _ -> error (loc_of_mapping x) "Cannot destruct tvar"
    | `Record _ -> error (loc_of_mapping x) "Cannot destruct record"
    | `Tuple _ -> error (loc_of_mapping x) "Cannot destruct tuple"
    | `List _ -> error (loc_of_mapping x) "Cannot destruct list"
    | `Option _ -> error (loc_of_mapping x) "Cannot destruct option"
    | `Nullable _ -> error (loc_of_mapping x) "Cannot destruct nullable"
    | `Wrap _ -> error (loc_of_mapping x) "Cannot destruct wrap"
    | _ -> error (loc_of_mapping x) "Cannot destruct unknown type"

let make_sum_writer p sum f =
  let tick, a = destruct_sum (p.deref sum) in
  let cases = Array.to_list (Array.map (fun x ->
    let o, j =
      match x.var_arepr, x.var_brepr with
        `Variant o, `Variant j -> o, j
      | _ -> assert false
    in
    `Inline (f p tick o j x)) a
  ) in
  let body : Ag_indent.t list = [
    `Line "match sum with";
    `Block cases;
  ] in [
    `Annot ("fun", `Line "fun ob sum ->");
    `Block body
  ]

let is_optional = function
  | { default=Default _ } -> true
  | { default=Checked _ } -> false

let unwrap p { jsonf=jsonf; mapping=mapping } =
  if jsonf.Ag_json.json_unwrapped
  then Ag_ocaml.unwrap_option p.deref mapping.f_value
  else mapping.f_value

let string_expr_of_constr_field p v_of_field field =
  let v = v_of_field field in
  let f_value = unwrap p field in
  match f_value with
    `String _ -> [ `Line v ]
  | _ ->
    ( `Line "(" )::
      (make_sum_writer p f_value (fun p tick o j x ->
        let ocaml_cons = o.Ag_ocaml.ocaml_cons in
        let json_cons = j.Ag_json.json_cons in
        match x.var_arg with
          None -> [
            `Line (sprintf "| %s%s -> %S" tick ocaml_cons json_cons);
          ]
        | Some _ -> [
          `Line (sprintf "| %s%s _ -> %S" tick ocaml_cons json_cons);
        ]
       ))@[ `Line (sprintf ") () %s" v)]

let rec make_writer p (x : oj_mapping) : Ag_indent.t list =
  match x with
      `Unit _
    | `Bool _
    | `Int _
    | `Float _
    | `String _
    | `Name _
    | `External _
    | `Tvar _ -> [ `Line (get_writer_name p x) ]

    | `Sum _ -> make_sum_writer p x make_variant_writer

    | `Record (loc, a, `Record o, `Record) ->
        [
          `Annot ("fun", `Line "fun ob x ->");
          `Block (make_record_writer p a o);
        ]

    | `Tuple (loc, a, `Tuple, `Tuple) ->
        let len = Array.length a in
        let a =
          Array.mapi (
            fun i x ->
              `Inline [
                `Line (sprintf "(let %s = x in" (nth "x" i len));
                `Line "(";
                `Block (make_writer p x.cel_value);
                `Line ") ob x";
                `Line ");"
              ]
          ) a
        in
        let l =
          insert (`Line "Bi_outbuf.add_char ob ',';") (Array.to_list a)
        in
        let op, cl =
          if p.std then '[', ']'
          else '(', ')'
        in
        [
          `Annot ("fun", `Line "fun ob x ->");
          `Block [
            `Line (sprintf "Bi_outbuf.add_char ob %C;" op);
            `Inline l;
            `Line (sprintf "Bi_outbuf.add_char ob %C;" cl);
          ]
        ]

    | `List (loc, x, `List o, `List j) ->
        (match j with
             `Array ->
               let write =
                 match o with
                     `List -> "Ag_oj_run.write_list ("
                   | `Array -> "Ag_oj_run.write_array ("
               in
               [
                 `Line write;
                 `Block (make_writer p x);
                 `Line ")";
               ]

           | `Object ->
               let k, v = get_assoc_type p.deref loc x in
               let write =
                 match o with
                     `List -> "Ag_oj_run.write_assoc_list ("
                   | `Array -> "Ag_oj_run.write_assoc_array ("
               in
               [
                 `Line write;
                 `Block (make_writer p k);
                 `Line ") (";
                 `Block (make_writer p v);
                 `Line ")";
               ]
        )

    | `Option (loc, x, `Option, `Option) ->
        [
          `Line (sprintf "Ag_oj_run.write_%soption ("
                   (if p.std then "std_" else ""));
          `Block (make_writer p x);
          `Line ")";
        ]

    | `Nullable (loc, x, `Nullable, `Nullable) ->
        [
          `Line "Ag_oj_run.write_nullable (";
          `Block (make_writer p x);
          `Line ")";
        ]

    | `Wrap (loc, x, `Wrap o, `Wrap) ->
        (match o with
            None -> make_writer p x
          | Some { Ag_ocaml.ocaml_wrap_t; ocaml_wrap; ocaml_unwrap } ->
              [
                `Line "fun ob x -> (";
                `Block [
                  `Line (sprintf "let x = ( %s ) x in (" ocaml_unwrap);
                  `Block (make_writer p x);
                  `Line ") ob x)";
                ]
              ]
        )

    | _ -> assert false



and make_variant_writer p tick o j x : Ag_indent.t list =
  let ocaml_cons = o.Ag_ocaml.ocaml_cons in
  let json_cons = j.Ag_json.json_cons in
  match x.var_arg with
      None ->
        let enclose s =
          if p.std then s
          else "<" ^ s ^ ">"
        in
        [
          `Line (sprintf "| %s%s -> Bi_outbuf.add_string ob %S"
                   tick ocaml_cons
                   (enclose (make_json_string json_cons)))
        ]
    | Some v ->
        let op, sep, cl =
          if p.std then "[", ",", ']'
          else "<", ":", '>'
        in
        [
          `Line (sprintf "| %s%s x ->" tick ocaml_cons);
          `Block [
            `Line (sprintf "Bi_outbuf.add_string ob %S;"
                     (op ^ make_json_string json_cons ^ sep));
            `Line "(";
            `Block (make_writer p v);
            `Line ") ob x;";
            `Line (sprintf "Bi_outbuf.add_char ob %C" cl);
          ]
        ]

and make_deconstructed_writer f g p tick o j x : Ag_indent.t list =
  let ocaml_cons = o.Ag_ocaml.ocaml_cons in
  let json_cons = j.Ag_json.json_cons in
  match x.var_arg with
      None -> [
        `Line (sprintf "| %s%s ->" tick ocaml_cons);
        (g json_cons)
      ]
    | Some v -> [
      `Line (sprintf "| %s%s deconstr ->" tick ocaml_cons);
      (g json_cons);
      f (`Block [
        `Line "(";
        `Block (make_writer p v);
        `Line ") ob deconstr;";
      ])
    ]

and make_record_writer p a record_kind =
  let fields = get_fields p a in
  let sep =
    [
      `Line "if !is_first then";
      `Block [
        `Line "is_first := false"
      ];
      `Line "else";
      `Block [
        `Line "Bi_outbuf.add_char ob ',';";
      ];
    ]
  in
  let write_field_tag json_fname =
    sprintf "Bi_outbuf.add_string ob %S;"
      (make_json_string json_fname ^ ":")
  in
  let v_of_field field =
    let dot = match record_kind with
      | `Record -> "."
      | `Object -> "#"
    in
    let ocaml_fname = field.ocamlf.Ag_ocaml.ocaml_fname in
    if is_optional field then
      sprintf "x.%s" ocaml_fname
    else
      sprintf "x%s%s" dot ocaml_fname
  in
  let apply p f field =
    let v = v_of_field field in
    if field.jsonf.Ag_json.json_unwrapped then
      [
        `Line (sprintf "(match %s with None -> () | Some x ->" v);
        `Block (f "x");
        `Line ");"
      ]
    else match field.default with
    | Checked _ -> f v
    | Default _ when p.force_defaults -> f v
    | Default d ->
      [
        `Line (sprintf "if %s <> %s then (" v d);
        `Block (f v);
        `Line ");"
      ]
  in

  let constr_var constr = "constr_" ^ constr.mapping.f_name in

  let write_constr_ss = Array.map (function
    | { payloads = payload_i :: _ } as field ->
      `Inline [
        `Line (sprintf "let %s =" (constr_var field));
        `Block (string_expr_of_constr_field p v_of_field
                  (if field.implicit then fields.(payload_i) else field));
        `Line "in";
      ]
    | { payloads = [] } -> `Inline []
  ) fields in

  let v_or_constr v field = if field.implicit then constr_var field else v in

  let write_fields =
    Array.mapi (
      fun i field ->
        let json_fname = field.jsonf.Ag_json.json_fname in
        let app v =
          let f_value = unwrap p field in
          match field with
            | { constructor = Some constr_i } ->
              let constr = fields.(constr_i) in
              let cons_code json_cons =
                (* Tag will be written. Check equality. *)
                `Block (apply p (fun v ->
                  [
                    `Line (sprintf "if %s <> %S then"
                             (constr_var constr) json_cons);
                    (match p.constr_mismatch_handler with
                      None -> `Line "();"
                    | Some f ->
                      `Line (sprintf "(%s) %S %s %S %S;"
                               f (v_of_field constr) (constr_var constr)
                               (v_of_field field) json_cons));
                  ]
                ) field)
              in
              ( `Line "(" )::
              (make_sum_writer p f_value
                 (make_deconstructed_writer (fun write_deconstr ->
                   `Block [
                     `Inline sep;
                     `Line (write_field_tag json_fname);
                     write_deconstr;
                   ]
                  ) cons_code)
              )@[ `Line (sprintf ") ob %s;" (v_or_constr v field)) ]
            | { constructor = None } ->
              [
                `Inline sep;
                `Line (write_field_tag json_fname);
                `Line "(";
                `Block (make_writer p f_value);
                `Line ")";
                `Block [`Line (sprintf "ob %s;" (v_or_constr v field))]
              ]
        in
        `Inline (apply p app field)
    ) fields
  in
  [
    `Line "Bi_outbuf.add_char ob '{';";
    `Line "let is_first = ref true in";
    `Inline (Array.to_list write_constr_ss);
    `Inline (Array.to_list write_fields);
    `Line "Bi_outbuf.add_char ob '}';";
  ]

let study_record deref fields =
  let unset_field_value = "Obj.magic 0.0" in

  let _, field_assignments =
    Array.fold_right (fun field (i, field_assignments) ->
      let v = match field.default with
        | Checked _ -> unset_field_value
        | Default s -> s
      in
      let field_ref = field.field_ref in
      let init_f = `Line (sprintf "let %s = ref (%s) in" field_ref v) in
      let init = match field.constructor with
        | None -> init_f
        | Some _constr_i ->
          let oname = field.ocamlf.Ag_ocaml.ocaml_fname in
          `Inline [ (* prepare to defer parsing *)
            init_f;
            `Line (sprintf "let raw_%s = (" oname);
            `Line "Yojson.init_lexer ~lnum:(-1) ()";
            `Line ") in";
          ]
      in
      let create = if field.implicit
        then `Block [] (* implicit fields don't have realizations in OCaml *)
        else
          let oname = field.ocamlf.Ag_ocaml.ocaml_fname in
          `Line (sprintf "%s = !field_%s;" oname oname)
      in
      (i + 1, (init, create) :: field_assignments)
    ) fields (0,[])
  in
  let init_fields, create_record_fields = List.split field_assignments in

  let create_record = [ `Line "{"; `Block create_record_fields; `Line "}" ] in

  let n = Array.fold_left (fun n -> function
    | { default = Checked k } -> max n (k + 1)
    | { default = Default _ } -> n
  ) 0 fields in

  let k = n / 31 + (if n mod 31 > 0 then 1 else 0) in
  let init_bits =
    Array.to_list (
      Array.init k (
        fun i -> `Line (sprintf "let bits%i = ref 0 in" i)
      )
    )
  in
  let final_bits = Array.make k 0 in
  for z = 0 to n - 1 do
    let i = z / 31 in
    let j = z mod 31 in
    final_bits.(i) <- final_bits.(i) lor (1 lsl j);
  done;
  let set_bit z =
    let i = z / 31 in
    let j = z mod 31 in
    `Line (sprintf "bits%i := !bits%i lor 0x%x;" i i (1 lsl j))
  in

  let check_bits =
    let bool_expr =
      String.concat " || " (
        Array.to_list (
          Array.mapi (
            fun i x -> sprintf "!bits%i <> 0x%x" i x
          ) final_bits
        )
      )
    in
    let bit_fields =
      let a = Array.init k (fun i -> sprintf "!bits%i" i) in
      sprintf "[| %s |]" (String.concat "; " (Array.to_list a))
    in
    let field_names =
      let _, l =
        Array.fold_left (
          fun (i,acc) field -> match field.default with
          | Checked k ->
            assert (k = i);
            (i + 1, sprintf "%S" field.mapping.f_name :: acc)
          | Default _ -> (i,acc)
        ) (0,[]) fields
      in
      sprintf "[| %s |]" (String.concat "; " (List.rev l))
    in
    if k = 0 then []
    else
      [ `Line (sprintf "if %s then Ag_oj_run.missing_fields p %s %s;"
                 bool_expr bit_fields field_names) ]
  in
  init_fields, init_bits, set_bit, check_bits, create_record

let rec make_reader p type_annot (x : oj_mapping) : Ag_indent.t list =
  match x with
      `Unit _
    | `Bool _
    | `Int _
    | `Float _
    | `String _
    | `Name _
    | `External _
    | `Tvar _ -> [ `Line (get_reader_name p x) ]

    | `Sum (loc, a, `Sum x, `Sum) ->
        let tick =
          match x with
              `Classic -> ""
            | `Poly -> "`"
        in
        let cases =
          Array.to_list (
            Array.map
              (make_variant_reader p type_annot tick false)
              a
          )
        in
        let l0, l1 =
          List.partition (fun x -> x.var_arg = None) (Array.to_list a)
        in
        let cases0 = List.map (make_variant_reader p type_annot tick true) l0 in
        let cases1 = List.map (make_variant_reader p type_annot tick true) l1 in

        let error_expr1 =
          [ `Line "Ag_oj_run.invalid_variant_tag p (String.sub s pos len)" ]
        in
        let int_mapping_function, int_matching =
          Ag_string_match.make_ocaml_int_mapping
            ~error_expr1
            cases
        in
        let std_int_mapping_function0, std_int_matching0 =
          Ag_string_match.make_ocaml_int_mapping
            ~error_expr1
            cases0
        in
        let std_int_mapping_function1, std_int_matching1 =
          Ag_string_match.make_ocaml_int_mapping
            ~error_expr1
            cases1
        in
        let read_tag =
          [
            `Line "Yojson.Safe.read_space p lb;";
            `Line "match Yojson.Safe.start_any_variant p lb with";
            `Block [
              `Line "| `Edgy_bracket -> (";
              `Block [
                `Block [
                  `Line "Yojson.Safe.read_space p lb;";
                  `Line "let f =";
                  `Block int_mapping_function;
                  `Line "in";
                  `Line "let i = Yojson.Safe.map_ident p f lb in";
                  `Inline int_matching;
                ];
                `Line ")";
              ];
              `Line "| `Double_quote -> (";
              `Block [
                `Block [
                  `Line "let f =";
                  `Block std_int_mapping_function0;
                  `Line "in";
                  `Line "let i = Yojson.Safe.map_string p f lb in";
                  `Inline std_int_matching0;
                ];
                `Line ")";
              ];
              `Line "| `Square_bracket -> (";
              `Block [
                `Block [
                  `Line "Yojson.Safe.read_space p lb;";
                  `Line "let f =";
                  `Block std_int_mapping_function1;
                  `Line "in";
                  `Line "let i = Yojson.Safe.map_ident p f lb in";
                  `Inline std_int_matching1;
                ];
                `Line ")";
              ];
            ];
                ]
        in
        [
          `Annot ("fun", `Line "fun p lb ->");
          `Block [
            `Inline read_tag;
          ]
        ]

    | `Record (loc, a, `Record o, `Record) ->
        (match o with
             `Record -> ()
           | `Object ->
               error loc "Sorry, OCaml objects are not supported"
        );
        [
          `Annot ("fun", `Line "fun p lb ->");
          `Block (make_record_reader p type_annot loc a o)
        ]

    | `Tuple (loc, a, `Tuple, `Tuple) ->
        [
          `Annot ("fun", `Line "fun p lb ->");
          `Block (make_tuple_reader p a);
        ]

    | `List (loc, x, `List o, `List j) ->
        (match j with
             `Array ->
               let read =
                 match o with
                     `List -> "Ag_oj_run.read_list ("
                   | `Array -> "Ag_oj_run.read_array ("
               in
               [
                 `Line read;
                 `Block (make_reader p None x);
                 `Line ")";
               ]

           | `Object ->
               let k, v = get_assoc_type p.deref loc x in
               let read =
                 match o with
                     `List -> "Ag_oj_run.read_assoc_list ("
                   | `Array -> "Ag_oj_run.read_assoc_array ("
               in
               [
                 `Line read;
                 `Block (make_reader p None k);
                 `Line ") (";
                 `Block (make_reader p None v);
                 `Line ")";
               ]
        )

    | `Option (loc, x, `Option, `Option) ->

        let a = [|
          {
            var_loc = loc;
            var_cons = "None";
            var_arg = None;
            var_arepr = `Variant { Ag_ocaml.ocaml_cons = "None";
                                   ocaml_vdoc = None };
            var_brepr = `Variant { Ag_json.json_cons = "None" };
          };
          {
            var_loc = loc;
            var_cons = "Some";
            var_arg = Some x;
            var_arepr = `Variant { Ag_ocaml.ocaml_cons = "Some";
                                   ocaml_vdoc = None };
            var_brepr = `Variant { Ag_json.json_cons = "Some" };
          };
        |]
        in
        make_reader p (Some "_ option") (`Sum (loc, a, `Sum `Classic, `Sum))

    | `Nullable (loc, x, `Nullable, `Nullable) ->
        [
          `Line "fun p lb ->";
          `Block [
            `Line "Yojson.Safe.read_space p lb;";
            `Line "(if Yojson.Safe.read_null_if_possible p lb then None";
            `Line "else Some ((";
            `Block (make_reader p None x);
            `Line ") p lb) : _ option)"
          ]
        ]

    | `Wrap (loc, x, `Wrap o, `Wrap) ->
        (match o with
            None -> make_reader p type_annot x
          | Some { Ag_ocaml.ocaml_wrap_t; ocaml_wrap; ocaml_unwrap } ->
              [
                `Line "fun p lb ->";
                `Block [
                  `Line "let x = (";
                  `Block (make_reader p type_annot x);
                  `Line ") p lb in";
                  `Line (sprintf "( %s ) x" ocaml_wrap);
                ]
              ]
        )

    | _ -> assert false


and make_variant_reader p type_annot tick std x : (string * Ag_indent.t list) =
  let o, j =
    match x.var_arepr, x.var_brepr with
        `Variant o, `Variant j -> o, j
      | _ -> assert false
  in
  let ocaml_cons = o.Ag_ocaml.ocaml_cons in
  let json_cons = j.Ag_json.json_cons in
  let expr =
    match x.var_arg with
        None ->
          if std then
            [
              `Line (Ag_ox_emit.opt_annot
                type_annot (sprintf "%s%s" tick ocaml_cons));
            ]
          else
            [
              `Line "Yojson.Safe.read_space p lb;";
              `Line "Yojson.Safe.read_gt p lb;";
              `Line (Ag_ox_emit.opt_annot
                type_annot (sprintf "%s%s" tick ocaml_cons));
            ]
      | Some v ->
          if std then
            [
              `Line "Yojson.Safe.read_space p lb;";
              `Line "Yojson.Safe.read_comma p lb;";
              `Line "Yojson.Safe.read_space p lb;";
              `Line "let x = (";
              `Block [
                `Block (make_reader p None v);
                `Line ") p lb";
              ];
              `Line "in";
              `Line "Yojson.Safe.read_space p lb;";
              `Line "Yojson.Safe.read_rbr p lb;";
              `Line (Ag_ox_emit.opt_annot
                type_annot (sprintf "%s%s x" tick ocaml_cons));
            ]
          else
            [
              `Line "Ag_oj_run.read_until_field_value p lb;";
              `Line "let x = (";
              `Block [
                `Block (make_reader p None v);
                `Line ") p lb";
              ];
              `Line "in";
              `Line "Yojson.Safe.read_space p lb;";
              `Line "Yojson.Safe.read_gt p lb;";
              `Line (Ag_ox_emit.opt_annot
                type_annot (sprintf "%s%s x" tick ocaml_cons));
            ]
  in
  (json_cons, expr)

and make_deconstructed_reader p loc fields set_bit =
  let v_of_field field = "!" ^ field.field_ref in
  let reconstruct_field constrf payloadf =
    let ocaml_name = payloadf.ocamlf.Ag_ocaml.ocaml_fname in
    let mapping = payloadf.mapping in
    let set_bit = match payloadf.default with
      | Default _ -> []
      | Checked k -> [set_bit k]
    in
    match p.deref mapping.f_value with
    | `Sum (loc, a, `Sum x, `Sum) ->
      let s = string_expr_of_constr_field p v_of_field constrf in
      let tick =
        match x with
          `Classic -> ""
        | `Poly -> "`"
      in
      let cases =
        Array.to_list (
          Array.map (fun x ->
            let o, j =
              match x.var_arepr, x.var_brepr with
                `Variant o, `Variant j -> o, j
              | _ -> assert false
            in
            let ocaml_cons = o.Ag_ocaml.ocaml_cons in
            let json_cons = j.Ag_json.json_cons in
            let expr = match x.var_arg with
              | None -> [
                `Line (sprintf "let loc = raw_%s in" ocaml_name);
                `Line "if loc.Yojson.lnum <> -1";
                `Line "then (";
                (* TODO: should this be a different warning/error? *)
                (match p.unknown_field_handler with
                  None -> `Line "();"
                | Some f ->
                  `Line (sprintf "(%s) %S %S;"
                           f (Atd_ast.string_of_loc loc) mapping.f_name));
                `Line (sprintf "%s := %s%s"
                         payloadf.field_ref tick ocaml_cons);
                `Line ") else (";
                `Inline set_bit;
                `Line (sprintf "%s := %s%s);"
                         payloadf.field_ref tick ocaml_cons);
              ]
              | Some v -> [
                `Line (sprintf "let loc = raw_%s in" ocaml_name);
                `Line "if loc.Yojson.lnum <> -1";
                `Line "then (let raw = Bi_outbuf.contents loc.Yojson.buf in";
                `Line "Bi_outbuf.clear loc.Yojson.buf;";
                `Line "let lb = Lexing.from_string raw in";
                `Line "let x = (";
                `Block [
                  `Block (make_reader p None v);
                  `Line ") loc lb";
                ];
                `Line "in";
                `Line (sprintf "%s := %s%s x);"
                         payloadf.field_ref tick ocaml_cons);
              ]
            in
            (json_cons, expr)
          ) a)
      in
      let error_expr1 =
        [ `Line "Ag_oj_run.invalid_variant_tag p s" ]
      in

      let int_mapping_function, int_matching =
        Ag_string_match.make_ocaml_int_mapping
          ~error_expr1
          cases
      in [
        `Line "let s = (";
        `Block s;
        `Line ") in";
        `Line "let f = (";
        `Block int_mapping_function;
        `Line ") in";
        `Line "let i = f s 0 (String.length s) in (";
        `Block int_matching;
        `Line ");";
        `Line "let constr =";
        `Inline (string_expr_of_constr_field p v_of_field payloadf);
        `Line "in if s <> constr";
        (match p.constr_mismatch_handler with
          None -> `Line "then ()"
        | Some f ->
          `Line (sprintf "then (%s) %S %s %S %s;"
                   f constrf.mapping.f_name "s"
                   mapping.f_name "constr"));
      ]
    | _ -> (* reconstructing a non-sum, undefined *)
      error loc "can't reconstruct a non-sum"
  in

  let rec toposort_fields order = function
    | [] ->
      if List.length order = Array.length fields
      then order
      else error loc "recursive constructors not allowed"
    | n::s ->
      toposort_fields (n::order) (List.rev_append fields.(n).payloads s)
  in

  let toposorted_fields =
    toposort_fields [] (fst (Array.fold_left (fun (s, i) -> function
    | { constructor = None   } -> (i :: s, i + 1)
    | { constructor = Some _ } -> (s,      i + 1)
    ) ([], 0) fields)) in

  List.fold_left (fun updates i ->
    let field = fields.(i) in
    match field.constructor with
    | None -> updates
    | Some constr_i ->
      let constr = fields.(constr_i) in
      match constr.default with
      | Default _ ->
        (`Block [
          `Line "(";
          `Block (reconstruct_field constr field);
          `Line ");";
        ])::updates
      | Checked k ->
        let i = k / 31 in
        let j = 1 lsl (k mod 31) in
        (`Block [
          `Line (sprintf "if !bits%i land 0x%x = 0x%x" i j j);
          `Line "then (";
          `Block (reconstruct_field constr field);
          `Line ")";
          match field.default with
          | Default _ when constr.implicit ->
            `Block [
              `Line "else (";
              set_bit k;
              `Line ");";
            ]
          | Default _ | Checked _ -> `Line ";"
        ])::updates
  ) [] toposorted_fields

and make_record_reader p type_annot loc a record_kind =
  let fields = get_fields p a in
  let init_fields, init_bits, set_bit, check_bits, create_record =
    study_record p.deref fields
  in

  let read_field =
    let cases =
      Array.mapi (fun i field ->
        let { ocamlf = ocamlf; jsonf = jsonf; mapping = x } = field in
        let unwrapped = jsonf.Ag_json.json_unwrapped in
        let f_value =
          if unwrapped then Ag_ocaml.unwrap_option p.deref x.f_value
          else x.f_value
        in
          let wrap l =
            if unwrapped then
              [
                `Line "Some (";
                `Block l;
                `Line ")"
              ]
            else l
          in
          let read_value =
            [
              `Line "(";
              `Block (make_reader p None f_value);
              `Line ") p lb";
            ]
          in
          let ocaml_fname = ocamlf.Ag_ocaml.ocaml_fname in
          let expr = match jsonf.Ag_json.json_tag_field with
            | Some _ -> [
                (* Defer parsing until we have read the whole record including
                   the constructor tag. *)
              `Line (sprintf "(let loc = raw_%s in" ocaml_fname);
              `Line "let cnum = lb.Lexing.lex_curr_pos in";
              `Line "loc.Yojson.lnum <- p.Yojson.lnum;";
              `Line "loc.Yojson.bol <- p.Yojson.bol - cnum;";
              `Line "loc.Yojson.fname <- p.Yojson.fname;";
              `Line "Bi_outbuf.clear p.Yojson.buf;";
              `Line "Yojson.Safe.buffer_json p lb;";
              `Line "let raw = Bi_outbuf.contents p.Yojson.buf in";
              `Line "Bi_outbuf.clear p.Yojson.buf;";
              `Line "Bi_outbuf.clear loc.Yojson.buf;";
              `Line "Bi_outbuf.add_string loc.Yojson.buf raw";
              `Line ");";
              match field.default with
              | Checked k -> set_bit k
              | Default _ -> `Inline []
            ]
            | None -> [
              `Line (sprintf "field_%s := (" ocaml_fname);
              `Block (wrap read_value);
              `Line ");";
              match field.default with
              | Checked k -> set_bit k
              | Default _ -> `Inline []
            ]
          in
          let opt_expr = match field.default with
            | Default _ ->
              [
                `Line "if not (Yojson.Safe.read_null_if_possible p lb) then (";
                `Block expr;
                `Line ")"
              ]
            | Checked _ ->
              expr
          in
          (jsonf.Ag_json.json_fname, opt_expr)
      ) fields
    in
    let int_mapping_function, int_matching =
      let error_expr1 =
        match p.unknown_field_handler with
            None -> [ `Line "-1" ]
          | Some f ->
              [ `Line (sprintf "(%s) %S (String.sub s pos len); -1"
                         f (Atd_ast.string_of_loc loc)) ]
      in
      Ag_string_match.make_ocaml_int_mapping
        ~exit_with: `Expr
        ~error_expr1
        ~error_expr2: [ `Line "Yojson.Safe.skip_json p lb" ]
        (Array.to_list cases)
    in
    [
      `Line "Yojson.Safe.read_space p lb;";
      `Line "let f =";
      `Block int_mapping_function;
      `Line "in";
      `Line "let i = Yojson.Safe.map_ident p f lb in";
      `Line "Ag_oj_run.read_until_field_value p lb;";
      `Line "(";
      `Block int_matching;
      `Line ");";
    ]
  in

  let update_deconstructed_fields =
    if List.exists (function
    | { constructor = Some _ } -> true
    | { constructor = None   } -> false
    ) (Array.to_list fields)
    then make_deconstructed_reader p loc fields set_bit
    else []
  in

  [
    `Line "Yojson.Safe.read_space p lb;";
    `Line "Yojson.Safe.read_lcurl p lb;";
    `Inline init_fields;
    `Inline init_bits;
    `Line "try";
    `Block [
      `Line "Yojson.Safe.read_space p lb;";
      `Line "Yojson.Safe.read_object_end lb;";
      `Inline read_field;
      `Line "while true do";
      `Block [
        `Line "Yojson.Safe.read_space p lb;";
        `Line "Yojson.Safe.read_object_sep p lb;";
        `Inline read_field;
      ];
      `Line "done;";
      `Line "assert false;";
    ];
    `Line "with Yojson.End_of_object -> (";
    `Block [
      `Block [
        `Inline update_deconstructed_fields;
        `Inline check_bits;
        `Line "(";
        `Block create_record;
        `Line (sprintf "%s)" (Ag_ox_emit.insert_annot type_annot));
      ];
      `Line ")";
    ];
  ]


and make_tuple_reader p a =
  let cells =
    Array.map (
      fun x ->
        match x.cel_arepr with
            `Cell f -> x, f.Ag_ocaml.ocaml_default
          | _ -> assert false
    ) a
  in
  let min_length =
    let n = ref (Array.length cells) in
    (try
       for i = Array.length cells - 1 downto 0 do
         let x, default = cells.(i) in
         if default = None then (
           n := i + 1;
           raise Exit
         )
       done
     with Exit -> ());
    !n
  in

  let read_cells =
    List.flatten (
      Array.to_list (
        Array.mapi (
          fun i (x, default) ->
            let read_value =
              [
                `Line "(";
                `Block (make_reader p None x.cel_value);
                `Line ") p lb";
              ]
            in
            let get_value =
              if i < min_length - 1 then
                [
                  `Line "let x =";
                  `Block read_value;
                  `Line "in";
                  `Line "incr len;";
                  `Line "Yojson.Safe.read_space p lb;";
                  `Line "Yojson.Safe.read_tuple_sep2 p std_tuple lb;";
                  `Line "x"
                ]
              else if i = min_length - 1 then
                [
                  `Line "let x =";
                  `Block read_value;
                  `Line "in";
                  `Line "incr len;";
                  `Line "(try";
                  `Block [
                    `Line "Yojson.Safe.read_space p lb;";
                    `Line "Yojson.Safe.read_tuple_sep2 p std_tuple lb;";
                  ];
                  `Line "with Yojson.End_of_tuple -> end_of_tuple := true);";
                  `Line "x"
                ]
              else
                let default_value =
                  match default with
                      None -> assert false
                    | Some s -> s
                in
                [
                  `Line (sprintf "if !end_of_tuple then (%s)" default_value);
                  `Line "else (";
                  `Block [
                    `Line "let x = (";
                    `Block read_value;
                    `Line ") in";
                    `Line "incr len;";
                    `Line "(try";
                    `Block [
                      `Line "Yojson.Safe.read_space p lb;";
                      `Line "Yojson.Safe.read_tuple_sep2 p std_tuple lb;";
                    ];
                    `Line "with Yojson.End_of_tuple ->";
                    `Block [
                      `Line "end_of_tuple := true);";
                    ];
                    `Line "x";
                  ];
                  `Line ")";
                ]
            in
            [
              `Line (sprintf "let x%i =" i);
              `Block get_value;
              `Line "in";
            ]
        ) cells
      )
    )
  in

  let make_tuple =
    sprintf "(%s)"
      (String.concat ", "
         (Array.to_list (Array.mapi (fun i _ -> sprintf "x%i" i) a)))
  in
  let req_fields =
    let acc = ref [] in
    for i = Array.length cells - 1 downto 0 do
      let _, default = cells.(i) in
      if default = None then
        acc := string_of_int i :: !acc
    done;
    sprintf "[ %s ]" (String.concat "; " !acc)
  in

  let finish_empty_tuple =
    if min_length = 0 then
      [
        `Line "(try Yojson.Safe.read_tuple_end2 p std_tuple lb";
        `Line "with Yojson.End_of_tuple -> end_of_tuple := true)";
      ]
    else []
  in

  let skip_remaining_cells =
    [
      `Line "if not !end_of_tuple then (";
      `Block [
        `Line "try";
        `Block [
          `Line "while true do";
          `Block [
            `Line "Yojson.Safe.skip_json p lb;";
            `Line "Yojson.Safe.read_space p lb;";
            `Line "Yojson.Safe.read_tuple_sep2 p std_tuple lb;";
          ];
          `Line "done";
        ];
        `Line "with Yojson.End_of_tuple -> ()";
      ];
      `Line ");"
    ]
  in

  [
    `Line "Yojson.Safe.read_space p lb;";
    `Line "let std_tuple = Yojson.Safe.start_any_tuple p lb in";
    `Line "let len = ref 0 in";
    `Line "let end_of_tuple = ref false in";
    `Inline finish_empty_tuple;
    `Line "(try";
    `Block [
      `Inline read_cells;
      `Inline skip_remaining_cells;
      `Line make_tuple;
    ];
    `Line "with Yojson.End_of_tuple ->";
    `Block [
      `Line (sprintf
               "Ag_oj_run.missing_tuple_fields p !len %s);"
               req_fields);
    ];
  ]

let make_ocaml_json_writer p ~original_types is_rec let1 let2 def =
  let x = match def.def_value with None -> assert false | Some x -> x in
  let name = def.def_name in
  let type_constraint = Ag_ox_emit.get_type_constraint ~original_types def in
  let param = def.def_param in
  let write = get_left_writer_name p name param in
  let to_string = get_left_to_string_name p name param in
  let writer_expr = make_writer p x in
  let eta_expand = is_rec && not (Ag_ox_emit.is_function writer_expr) in
  let needs_annot = Ag_ox_emit.needs_type_annot x in
  let extra_param, extra_args, type_annot =
    match eta_expand, needs_annot with
    | true, false -> " ob x", " ob x", None
    | true, true -> sprintf " ob (x : %s)" type_constraint, " ob x", None
    | false, false -> "", "", None
    | false, true -> "", "", Some (sprintf "_ -> %s -> _" type_constraint)
  in
  [
    `Line (sprintf "%s %s = ("
             let1 (Ag_ox_emit.opt_annot_def type_annot (write ^ extra_param)));
    `Block (List.map Ag_indent.strip writer_expr);
    `Line (sprintf ")%s" extra_args);
    `Line (sprintf "%s %s ?(len = 1024) x =" let2 to_string);
    `Block [
      `Line "let ob = Bi_outbuf.create len in";
      `Line (sprintf "%s ob x;" write);
      `Line "Bi_outbuf.contents ob"
    ]
  ]

let make_ocaml_json_reader p ~original_types is_rec let1 let2 def =
  let x = match def.def_value with None -> assert false | Some x -> x in
  let name = def.def_name in
  let type_constraint = Ag_ox_emit.get_type_constraint ~original_types def in
  let param = def.def_param in
  let read = get_left_reader_name p name param in
  let of_string = get_left_of_string_name p name param in
  let type_annot =
    match Ag_ox_emit.needs_type_annot x with
    | true -> Some type_constraint
    | false -> None
  in
  let reader_expr = make_reader p type_annot x in
  let eta_expand = is_rec && not (Ag_ox_emit.is_function reader_expr) in
  let extra_param, extra_args =
    if eta_expand then " p lb", " p lb"
    else "", ""
  in
  let pp =
    match p.preprocess_input with
        None -> []
      | Some f -> [ `Line (sprintf "let s = ( %s ) s in" f) ]
  in
  [
    `Line (sprintf "%s %s%s = (" let1 read extra_param);
    `Block (List.map Ag_indent.strip reader_expr);
    `Line (sprintf ")%s" extra_args);
    `Line (sprintf "%s %s s =" let2 of_string);
    `Block [
      `Inline pp;
      `Line (
        sprintf "%s (Yojson.Safe.init_lexer ()) \
                       (Lexing.from_string s)" read);
    ]
  ]


let map f = function
    [] -> []
  | x :: l ->
      let y = f true x in
      y :: List.map (f false) l

let get_let ~is_rec ~is_first =
  if is_first then
    if is_rec then "let rec", "and"
    else "let", "let"
  else "and", "and"

let make_ocaml_json_impl
    ~std ~unknown_field_handler ~constr_mismatch_handler
    ~with_create ~force_defaults ~preprocess_input ~original_types
    ~ocaml_version
    buf deref defs =
  let p = {
    deref = deref;
    std = std;
    unknown_field_handler = unknown_field_handler;
    constr_mismatch_handler = constr_mismatch_handler;
    force_defaults = force_defaults;
    preprocess_input;
    ocaml_version;
  } in
  let ll =
    List.map (
      fun (is_rec, l) ->
        let l = List.filter (fun x -> x.def_value <> None) l in
        let writers =
          map (
            fun is_first def ->
              let let1, let2 = get_let ~is_rec ~is_first in
              make_ocaml_json_writer p ~original_types is_rec let1 let2 def
          ) l
        in
        let readers =
          map (
            fun is_first def ->
              let let1, let2 = get_let ~is_rec ~is_first in
              make_ocaml_json_reader p ~original_types is_rec let1 let2 def
          ) l
        in
        List.flatten (writers @ readers)
  ) defs
  in
  Atd_indent.to_buffer buf (List.flatten ll);

  if with_create then
    List.iter (
      fun (is_rec, l) ->
        let l = List.filter Ag_ox_emit.is_exportable l in
        List.iter (
          fun x ->
            let intf, impl = Ag_ox_emit.make_record_creator deref x in
            Buffer.add_string buf impl
        ) l
    ) defs




(*
  Glue
*)

let translate_mapping (l : (bool * Atd_ast.module_body) list) =
  defs_of_atd_modules l

let write_opens buf l =
  List.iter (fun s -> bprintf buf "open %s\n" s) l;
  bprintf buf "\n"

let make_mli
    ~header ~opens ~with_typedefs ~with_create ~with_fundefs
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_json_intf ~with_create buf deref defs;
  Buffer.contents buf

let make_ml
    ~header ~opens ~with_typedefs ~with_create ~with_fundefs
    ~std ~unknown_field_handler ~constr_mismatch_handler
    ~force_defaults ~preprocess_input ~original_types
    ~ocaml_version
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_json_impl
      ~std ~unknown_field_handler ~constr_mismatch_handler
      ~with_create ~force_defaults ~preprocess_input ~original_types
      ~ocaml_version
      buf deref defs;
  Buffer.contents buf

let make_ocaml_files
    ~opens
    ~with_typedefs
    ~with_create
    ~with_fundefs
    ~all_rec
    ~std
    ~unknown_field_handler
    ~constr_mismatch_handler
    ~pos_fname
    ~pos_lnum
    ~type_aliases
    ~force_defaults
    ~preprocess_input
    ~name_overlap
    ~ocaml_version
    ~pp_convs
    atd_file out =
  let ((head, m0), _) =
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
  let tsort =
    if all_rec then
      function m -> [ (true, m) ]
    else
      Atd_util.tsort
  in
  let m1 = tsort m0 in
  let defs1 = translate_mapping m1 in
  if not name_overlap then Ag_ox_emit.check defs1;
  let (m1', original_types) =
    Atd_expand.expand_module_body ~keep_poly:true m0
  in
  let m2 = tsort m1' in
  (* m0 = original type definitions
     m1 = original type definitions after dependency analysis
     m2 = monomorphic type definitions after dependency analysis *)
  let ocaml_typedefs =
    Ag_ocaml.ocaml_of_atd ~pp_convs ~target:`Json ~type_aliases (head, m1) in
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
    make_mli ~header ~opens ~with_typedefs ~with_create ~with_fundefs
      ocaml_typedefs (Ag_mapping.make_deref defs1) defs1
  in
  let ml =
    make_ml ~header ~opens ~with_typedefs ~with_create ~with_fundefs
      ~std ~unknown_field_handler ~constr_mismatch_handler
      ~force_defaults ~preprocess_input ~original_types
      ~ocaml_version
      ocaml_typedefs (Ag_mapping.make_deref defs) defs
  in
  Ag_ox_emit.write_ocaml out mli ml
