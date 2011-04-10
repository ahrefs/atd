(* $Id: ag_oj_emit.ml 53195 2011-01-28 23:38:12Z martin $ *)

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
}


let make_ocaml_json_intf ~with_create buf deref defs =
  List.iter (
    fun x ->
      let s = x.def_name in
      if s <> "" && s.[0] <> '_' && x.def_value <> None then (
        let full_name = Ag_ocaml.get_full_type_name x in
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

        if with_create then
          let create_record_intf, create_record_impl =
            Ag_ocaml.make_record_creator deref x
          in
          bprintf buf "%s" create_record_intf;
          bprintf buf "\n";
      )
  )
    (flatten defs)

let get_assoc_type deref loc x =
  match deref x with
      `Tuple (loc2, [| k; v |], `Tuple, `Tuple) ->
        (match deref k.cel_value with
             `String _ -> ()
           | _ ->
               error loc "Due to <json repr=\"object\"> keys must be strings");
        v.cel_value
    | _ ->
        error loc "Expected due to <json repr=\"object\">: (string * _) list"


let nth name i len =
  let l =
    Array.to_list (Array.init len (fun j -> if i = j then name else "_")) in
  String.concat ", " l

let get_fields p a =
  List.map (
    fun x ->
      let ocaml_fname, ocaml_default, json_fname, optional, unwrapped =
	match x.f_arepr, x.f_brepr with
	    `Field o, `Field j ->
              let ocaml_default =
		match x.f_kind with
                    `With_default ->
                      (match o.Ag_ocaml.ocaml_default with
                           None ->
                             let d = 
                               Ag_ocaml.get_implicit_ocaml_default
                                 p.deref x.f_value in
                             if d = None then
                               error x.f_loc "Missing default field value"
                             else
                               d
                         | Some _ as default -> default
                      )
                  | `Optional -> Some "None"
                  | `Required -> None
              in
              let optional =
		match x.f_kind with
                    `Optional | `With_default -> true
                  | `Required -> false
              in
	      o.Ag_ocaml.ocaml_fname,
	      ocaml_default,
              j.Ag_json.json_fname,
	      optional,
	      j.Ag_json.json_unwrapped
	  | _ -> assert false
      in
      (x, ocaml_fname, ocaml_default, json_fname, optional, unwrapped)
  )
    (Array.to_list a)


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
	)

    | `Float (loc, `Float, `Float) ->
        if p.std then "Yojson.Safe.write_std_float"
        else "Yojson.Safe.write_float"

    | `String (loc, `String, `String) ->
	"Yojson.Safe.write_string"

    | `Tvar (loc, s) -> "write_" ^ name_of_var s

    | `Name (loc, s, args, None, None) ->
        let l = List.map (get_writer_name ~paren:true p) args in
        let s = String.concat " " (name_f s :: l) in
        if paren && l <> [] then "(" ^ s ^ ")"
        else s

    | `External (loc, s, args, `External (module_path, ext_name), `External) ->
        let f = module_path ^ "." ^ name_f ext_name in
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
	)

    | `Float (loc, `Float, `Float) -> "Ag_oj_run.read_number"

    | `String (loc, `String, `String) -> "Ag_oj_run.read_string"

    | `Tvar (loc, s) -> "read_" ^ name_of_var s

    | `Name (loc, s, args, None, None) ->
        let l = List.map (get_reader_name ~paren:true p) args in
        let s = String.concat " " (name_f s :: l) in
        if paren && l <> [] then "(" ^ s ^ ")"
        else s

    | `External (loc, s, args, `External (module_path, ext_name), `External) ->
        let f = module_path ^ "." ^ name_f ext_name in
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

    | `Sum (loc, a, `Sum x, `Sum) ->
	let tick =
	  match x with
	      `Classic -> ""
	    | `Poly -> "`"
	in
	let body : Ag_indent.t list =
	  [
	    `Line "match x with";
	    `Block (
	      Array.to_list (
		Array.map
		  (fun x -> `Inline (make_variant_writer p tick x))
		  a
	      )
	    )
	  ]
	in
	[
	  `Annot ("fun", `Line "fun ob x ->");
	  `Block body
	]

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
               let x = get_assoc_type p.deref loc x in
               let write =
                 match o with
                     `List -> "Ag_oj_run.write_assoc_list ("
                   | `Array -> "Ag_oj_run.write_assoc_array ("
               in
               [
                 `Line write;
                 `Block (make_writer p x);
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

    | `Shared (loc, _, _, _, _) ->
        error loc "Sharing is not supported by the JSON interface"

    | _ -> assert false



and make_variant_writer p tick x : 
    Ag_indent.t list =
  let o, j =
    match x.var_arepr, x.var_brepr with
	`Variant o, `Variant j -> o, j
      | _ -> assert false
  in
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

and make_record_writer p a record_kind =
  let dot =
    match record_kind with
	`Record -> "."
      | `Object -> "#"
  in
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
  let write_fields =
    List.map (
      fun (x, ocaml_fname, ocaml_default, json_fname, optional, unwrapped) ->
	let f_value =
	  if unwrapped then Ag_ocaml.unwrap_option p.deref x.f_value
	  else x.f_value
	in
	let write_field_tag =
	  sprintf "Bi_outbuf.add_string ob %S;"
	    (make_json_string json_fname ^ ":")
	in
	let app v =
	  [
            `Inline sep;
	    `Line write_field_tag;
	    `Line "(";
	    `Block (make_writer p f_value);
	    `Line ")";
	    `Block [`Line (sprintf "ob %s;" v)]
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
              `Line (sprintf "(match %s with None -> () | Some x ->" v);
              `Block (app "x");
              `Line ");"
	    ]
	  else if optional then
	    [
              `Line (sprintf "if %s != %s then (" v (unopt ocaml_default));
              `Block (app v);
              `Line ");"
            ]
	  else
	    app v
	in
	`Inline l
    ) fields
  in
  [
    `Line "Bi_outbuf.add_char ob '{';";
    `Line "let is_first = ref true in";
    `Inline write_fields;
    `Line "Bi_outbuf.add_char ob '}';";    
  ]



let study_record deref fields =
  let maybe_constant =
    List.for_all (function (_, _, Some _, _, _, _) -> true | _ -> false) fields
  in
  let _, init_fields = 
    List.fold_right (
      fun (x, oname, default, jname, opt, unwrap) (maybe_constant, l) ->
	let maybe_constant, v =
	  match default with
	      None ->
		assert (not opt);
		(*
                  The initial value is a float because the record may be 
                  represented as a double_array (unboxed floats).
                  Float values work in all cases.
		*)
		let v = "Obj.magic 0.0" in
		maybe_constant, v
	    | Some s ->
		false, (if maybe_constant then sprintf "(fun x -> x) (%s)" s
			else s)
	in
	(maybe_constant, `Line (sprintf "%s = %s;" oname v) :: l)
    ) fields (maybe_constant, [])
  in
  let n, mapping =
    List.fold_left (
      fun (i, acc) (x, oname, default, jname, opt, unwrap) ->
	if not opt then
	  (i+1, (Some i :: acc))
	else
	  (i, (None :: acc))
    ) (0, []) fields
  in
  let mapping = Array.of_list (List.rev mapping) in

  let init_val = [ `Line "{"; `Block init_fields; `Line "}" ] in

  let k = n / 31 + (if n mod 31 > 0 then 1 else 0) in
  let init_bits = 
    Array.to_list (
      Array.init k (
	fun i -> `Line (sprintf "let bits%i = ref 0 in" i)
      )
    )
  in
  let final_bits = Array.make k 0 in
  for z0 = 0 to List.length fields - 1 do
    match mapping.(z0) with
	None -> ()
      | Some z ->
	  let i = z / 31 in
	  let j = z mod 31 in
	  final_bits.(i) <- final_bits.(i) lor (1 lsl j);
  done;
  let set_bit z0 =
    match mapping.(z0) with
	None -> []
      | Some z ->    
	  let i = z / 31 in
	  let j = z mod 31 in
	  [ `Line (sprintf "bits%i := !bits%i lor 0x%x;" i i (1 lsl j)) ]
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
      let l =
	List.fold_right (
	  fun (x, oname, default, jname, opt, unwrap) acc ->
	    if default = None && not opt then
	      sprintf "%S" x.f_name :: acc
	    else
	      acc
	) fields []
      in
      sprintf "[| %s |]" (String.concat "; " l)
    in
    if k = 0 then []
    else
      [ `Line (sprintf "if %s then Ag_oj_run.missing_fields %s %s;"
		 bool_expr bit_fields field_names) ]
  in
  init_val, init_bits, set_bit, check_bits


let rec make_reader p (x : oj_mapping) : Ag_indent.t list =
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
	      (make_variant_reader p tick false)
	      a
	  )
	in
        let l0, l1 =
          List.partition (fun x -> x.var_arg = None) (Array.to_list a)
        in
	let cases0 = List.map (make_variant_reader p tick true) l0 in
        let cases1 = List.map (make_variant_reader p tick true) l1 in

        let error_expr1 =
          [ `Line "Ag_oj_run.invalid_variant_tag (String.sub s pos len)" ]
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
	  `Block (make_record_reader p loc a o)
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
                     `List -> "Yojson.Safe.read_list ("
                   | `Array -> "Yojson.Safe.read_array ("
               in
	       [
		 `Line read;
		 `Block (make_reader p x);
		 `Line ")";
	       ]

           | `Object ->
               let x = get_assoc_type p.deref loc x in
               let read =
                 match o with
                     `List -> "Ag_oj_run.read_assoc_list ("
                   | `Array -> "Ag_oj_run.read_assoc_array ("
               in
               [
                 `Line read;
                 `Block (make_reader p x);
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
	make_reader p (`Sum (loc, a, `Sum `Classic, `Sum))

    | `Shared (loc, _, _, _, _) ->
        error loc "Sharing is not supported by the JSON interface"

    | _ -> assert false


and make_variant_reader p tick std x : (string * Ag_indent.t list) =
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
	      `Line (sprintf "%s%s" tick ocaml_cons);
	    ]
          else
	    [
	      `Line "Yojson.Safe.read_space p lb;";
	      `Line "Yojson.Safe.read_gt p lb;";
	      `Line (sprintf "%s%s" tick ocaml_cons);
	    ]
      | Some v ->
          if std then
            [
              `Line "Yojson.Safe.read_space p lb;";
	      `Line "Yojson.Safe.read_comma p lb;";
	      `Line "Yojson.Safe.read_space p lb;";
	      `Line "let x = (";
	      `Block [
	        `Block (make_reader p v);
	        `Line ") p lb";
	      ];
	      `Line "in";
	      `Line "Yojson.Safe.read_space p lb;";
	      `Line "Yojson.Safe.read_rbr p lb;";
	      `Line (sprintf "%s%s x" tick ocaml_cons);
            ]
          else
	    [
	      `Line "Yojson.Safe.read_space p lb;";
	      `Line "Yojson.Safe.read_colon p lb;";
	      `Line "Yojson.Safe.read_space p lb;";
	      `Line "let x = (";
	      `Block [
	        `Block (make_reader p v);
	        `Line ") p lb";
	      ];
	      `Line "in";
	      `Line "Yojson.Safe.read_space p lb;";
	      `Line "Yojson.Safe.read_gt p lb;";
	      `Line (sprintf "%s%s x" tick ocaml_cons);
	    ]
  in
  (json_cons, expr)


and make_record_reader p loc a record_kind =
  let fields = get_fields p a in  
  let init_val, init_bits, set_bit, check_bits = study_record p.deref fields in

  let read_field =
    let a = Array.of_list fields in
    let cases =
      Array.mapi (
	fun i (x, ocaml_fname, ocaml_default, json_fname, opt, unwrapped) ->
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
              `Block (make_reader p f_value);
              `Line ") p lb";
            ]
	  in
	  let expr =
            [
              `Line "let v =";
	      `Block (wrap read_value);
	      `Line "in";
	      `Line (sprintf "Obj.set_field (Obj.repr x) %i (Obj.repr v);" i);
	      `Inline (set_bit i);
            ]
          in
          (json_fname, expr)
      ) a
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
      `Line "Yojson.Safe.read_space p lb;";
      `Line "Yojson.Safe.read_colon p lb;";
      `Line "Yojson.Safe.read_space p lb;";
      `Line "(";
      `Block int_matching;
      `Line ");";
    ]
  in

  [
    `Line "Yojson.Safe.read_space p lb;";
    `Line "Yojson.Safe.read_lcurl p lb;";
    `Line "let x =";
    `Block init_val;
    `Line "in";
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
        `Inline check_bits;
        `Line "x";
      ];
      `Line ")";
    ];
  ]


and make_tuple_reader deref a =
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
                `Block (make_reader deref x.cel_value);
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
	       "Ag_oj_run.missing_tuple_fields !len %s);"
	       req_fields);
    ];
  ]


let rec is_function (l : Ag_indent.t list) =
  match l with
      [] -> false
    | x :: _ ->
        match x with
            `Line _ -> false
          | `Block l -> is_function l
          | `Inline l -> is_function l
          | `Annot ("fun", _) -> true
          | `Annot _ -> false

let make_ocaml_json_writer p is_rec let1 let2 def =
  let x = match def.def_value with None -> assert false | Some x -> x in
  let name = def.def_name in
  let param = def.def_param in
  let write = get_left_writer_name p name param in
  let to_string = get_left_to_string_name p name param in
  let writer_expr = make_writer p x in
  let extra_param, extra_args =
    if is_function writer_expr || not is_rec then "", ""
    else " ob x", " ob x"
  in
  [
    `Line (sprintf "%s %s%s = (" let1 write extra_param);
    `Block (List.map Ag_indent.strip writer_expr);
    `Line (sprintf ")%s" extra_args);
    `Line (sprintf "%s %s ?(len = 1024) x =" let2 to_string);
    `Block [
      `Line "let ob = Bi_outbuf.create len in";
      `Line (sprintf "%s ob x;" write);
      `Line "Bi_outbuf.contents ob"
    ]
  ]

let make_ocaml_json_reader p is_rec let1 let2 def =
  let x = match def.def_value with None -> assert false | Some x -> x in
  let name = def.def_name in
  let param = def.def_param in
  let read = get_left_reader_name p name param in
  let of_string = get_left_of_string_name p name param in
  let reader_expr = make_reader p x in
  let extra_param, extra_args =
    if is_function reader_expr || not is_rec then "", ""
    else " p lb", " p lb"
  in
  [
    `Line (sprintf "%s %s%s = (" let1 read extra_param);
    `Block (List.map Ag_indent.strip reader_expr);
    `Line (sprintf ")%s" extra_args);
    `Line (sprintf "%s %s s =" let2 of_string);
    `Block [
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
    ~std ~unknown_field_handler ~with_create
    buf deref defs =
  let p = {
    deref = deref;
    std = std;
    unknown_field_handler = unknown_field_handler;
  } in
  let ll =
    List.map (
      fun (is_rec, l) ->
	let l = List.filter (fun x -> x.def_value <> None) l in
	let writers =
	  map (
	    fun is_first def ->
	      let let1, let2 = get_let ~is_rec ~is_first in
	      make_ocaml_json_writer p is_rec let1 let2 def
	  ) l
	in
	let readers =
	  map (
	    fun is_first def ->
	      let let1, let2 = get_let ~is_rec ~is_first in
	      make_ocaml_json_reader p is_rec let1 let2 def
	  ) l
	in
	List.flatten (writers @ readers)
  ) defs
  in
  Atd_indent.to_buffer buf (List.flatten ll);

  if with_create then
    List.iter (
      fun (is_rec, l) ->
        List.iter (
          fun x ->
            let intf, impl = Ag_ocaml.make_record_creator deref x in
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
    ~std ~unknown_field_handler
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
      ~std ~unknown_field_handler ~with_create buf deref defs;
  Buffer.contents buf

let make_ocaml_files
    ~opens
    ~with_typedefs
    ~with_create
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
    Ag_ocaml.ocaml_of_atd ~target:`Json ~type_aliases (head, m1) in
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
      ~std ~unknown_field_handler
      ocaml_typedefs (Ag_mapping.make_deref defs) defs
  in
  Ag_ox_emit.write_ocaml out mli ml
