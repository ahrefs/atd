(*
   A minimal OCaml AST used to pretty-print OCaml type definitions.

   It used to be a confusing part of ocaml.ml.
   Perhaps it should be removed completely because it's complicated and
   doesn't add much value.
*)

open Atd.Stdlib_extra
module A = Atd.Ast
module An = Ocaml_annot
module R = Ocaml_repr

(*
   OCaml syntax tree used to represent type expressions before
   pretty-printing.
*)
type ocaml_type_param = string list

type ocaml_expr =
  [ `Sum of (R.atd_ocaml_sum * ocaml_variant list)
  | `Record of (R.atd_ocaml_record * ocaml_field list)
  | `Tuple of ocaml_expr list
  | `Name of ((* OCaml type name Foo.bar or bar *) string * ocaml_expr list)
  | `Tvar of string
  ]

and ocaml_variant =
    string * ocaml_expr option * Atd.Doc.doc option

and ocaml_field =
    (string * bool (* is mutable? *)) * ocaml_expr * Atd.Doc.doc option

(*
   OCaml type definition:

     type foo = Baz_t.foo = bar list [@@what ever]
          ^^^   ^^^^^^^^^   ^^^^^^^^ ^^^^^^^^^^^^^
          name  alias       expr     ppx attrs

  A useful definition in the context of ATD would have at least an expr
  or an alias.
*)
type ocaml_def = {
  o_def_name : (string * ocaml_type_param);
  o_def_alias : (string * ocaml_type_param) option;
  o_def_expr : ocaml_expr option;
  o_def_doc : Atd.Doc.doc option;
  o_def_attrs : string list;
}

type ocaml_module_body = ocaml_def list

(* https://ocaml.org/manual/lex.html#sss:keywords *)
let is_ocaml_keyword = function
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint"
  | "do" | "done" | "downto" | "else" | "end" | "exception" | "external"
  | "false" | "for" | "fun" | "function" | "functor" | "if" | "in" | "include"
  | "inherit" | "initializer" | "land" | "lazy" | "let" | "lor" | "lsl"
  | "lsr" | "lxor" | "match" | "method" | "mod" | "module" | "mutable" | "new"
  | "nonrec" | "object" | "of" | "open" | "or" | "private" | "rec" | "sig"
  | "struct" | "then" | "to" | "true" | "try" | "type" | "val" | "virtual"
  | "when" | "while" | "with" -> true
  | _ -> false

(*
  Mapping from ATD to OCaml
*)

let rec map_expr (env : R.env)
    (type_param: A.type_param) (x : A.type_expr) : ocaml_expr =
  let target = env.target in
  match x with
    Atd.Ast.Sum (_, l, an) ->
      let kind = An.get_ocaml_sum target an in
      `Sum (kind, List.map (map_variant env ~kind) l)
  | Record (loc, l, an) ->
      let kind = An.get_ocaml_record target an in
      let field_prefix = An.get_ocaml_field_prefix target an in
      if l = [] then
        Error.error loc "Empty record (not valid in OCaml)"
      else
        `Record (kind, List.map (map_field env field_prefix) l)
  | Tuple (_, l, _) ->
      `Tuple (List.map (fun (_, x, _) -> (map_expr env []) x) l)
  | List (_, x, an) ->
      let s = R.string_of_ocaml_list (An.get_ocaml_list target an) in
      `Name (s, [map_expr env [] x])
  | Option (_, x, _) ->
      `Name ("option", [map_expr env [] x])
  | Nullable (_, x, _) ->
      `Name ("option", [map_expr env [] x])
  | Shared (_, _, _) ->
      failwith "Sharing is not supported"
  | Wrap (loc, x, a) ->
      (match An.get_ocaml_wrap ~type_param target loc a with
         None -> map_expr env [] x
       | Some { ocaml_wrap_t ; _ } -> `Name (ocaml_wrap_t, [])
      )
  | Name (_, (loc2, name, l), an) ->
      let s = An.get_ocaml_type_path env loc2 name an in
      `Name (s, List.map (map_expr env []) l)
  | Tvar (_, s) ->
      `Tvar s

and map_variant (env : R.env) ~kind (x : A.variant) : ocaml_variant =
  match kind, x with
  | _, Inherit _ -> assert false
  | Poly, Variant (loc, _, Some (Record _)) ->
      Error.error loc
        "Inline records are not allowed in polymorphic variants \
         (not valid in OCaml)"
  | _, Variant (loc, (s, an), o) ->
      let s = An.get_ocaml_cons env.target s an in
      (s, Option.map (map_expr env []) o, Atd.Doc.get_doc loc an)

and map_field (env : R.env) ocaml_field_prefix (x : A.field) : ocaml_field =
  let target = env.target in
  match x with
    Inherit _ -> assert false
  | Field (loc, (atd_fname, _, an), x) ->
      let ocaml_fname =
        An.get_ocaml_fname target (ocaml_field_prefix ^ atd_fname) an in
      if is_ocaml_keyword ocaml_fname then
        Error.error loc
          ("\"" ^ ocaml_fname ^
           "\" cannot be used as field name (reserved OCaml keyword)");
      let fname =
        if ocaml_fname = atd_fname then ocaml_fname
        else sprintf "%s (*atd %s *)" ocaml_fname atd_fname
      in
      let is_mutable = An.get_ocaml_mutable target an in
      ((fname, is_mutable), map_expr env [] x, Atd.Doc.get_doc loc an)

(* hack to deal with legacy behavior *)
let lhs_has_possibly_relevant_annotation
    (x : A.type_def) =
  List.exists
    (fun target ->
       let name =
         match x.name with
         | TN [x] -> x
         | TN _ -> assert false
       in
       Ocaml_annot.get_ocaml_module_and_t target name x.annot <> None)
    R.all_targets

(* hack to deal with legacy behavior *)
let rhs_is_just_abstract (x : A.type_def) =
  match x.value with
  | Atd.Ast.Name (_, (loc, TN ["abstract"], type_params), an2) ->
      if type_params <> [] then
        Error.error loc "\"abstract\" takes no type parameters";
      true
  | _ ->
      false

(*
   This is an ATD definition of the form

     type foo <...> = abstract

   e.g.

     type foo <ocaml from="Foo"> = abstract

   where the right-hand side is exactly 'abstract' and is ignored.
   This is weird and will be deprecated as soon as we implement
   a clean module system allowing us to import whole modules without
   special annotations.

   The annotation <...> on the left-hand side specifies the type name and
   readers/writers to be used. They are placed there rather than
   directly on 'abstract' for "historical reasons". We preserve the legacy
   behavior unless there's no suitable left-hand side annotation.

   The following is valid and follows the more recent convention that
   'abstract' means "untyped data". It is NOT considered an abstract
   definition:

     type foo = abstract
                ^^^^^^^^
                JSON or biniou AST representing raw data
*)
let is_abstract_def (x : A.type_def) =
  lhs_has_possibly_relevant_annotation x
  && rhs_is_just_abstract x

let map_def
    ~(env : R.env)
    ~(type_aliases : string option)
    (td : A.type_def) : ocaml_def option =
  let name =
    match td.name with
    | TN [x] -> x
    | _ -> assert false
  in
  let an1 = td.annot in
  let loc = td.loc in
  if is_ocaml_keyword name then
    Error.error loc
      (sprintf {|"%s" cannot be used as type name (reserved OCaml keyword)|}
         name);
  let is_predef = An.get_ocaml_predef env.target an1 in
  let is_abstract = is_abstract_def td in
  let define_alias =
    if is_predef || is_abstract || type_aliases <> None then
      match An.get_ocaml_module_and_t env.target name an1, type_aliases with
          Some (types_module, _, s), _ -> Some (types_module, s)
        | None, Some types_module -> Some (types_module, name)

        | None, None -> None
    else
      None
  in
  if is_predef && define_alias = None then
    None
  else
    let an2 = Atd.Ast.annot_of_type_expr td.value in
    let an = an1 @ an2 in
    let doc = Atd.Doc.get_doc loc an in
    let alias, x =
      match define_alias with
          None ->
            (* Ordinary type definitions or aliases:
                 type foo = string * int
                 type foo = bar
                 type foo = { hello: string }
            *)
            if is_abstract then (None, None)
            else (None, Some (map_expr env td.param td.value))
        | Some (module_path, ext_name) ->
            (*
                 type foo = Bar_t.foo = { hello: string }
               or
                 type foo = Bar_t.foo = Alpha | Beta of int
            *)
            let alias = Some (module_path ^ "." ^ ext_name, td.param) in
            let x =
              match map_expr env td.param td.value with
                  `Sum (Classic, _)
                | `Record (Record, _) as x -> Some x
                | _ -> None
            in
            (alias, x)
    in
    if x = None && alias = None then
      None
    else
      Some {
        o_def_name = (name, td.param);
        o_def_alias = alias;
        o_def_expr = x;
        o_def_doc = doc;
        o_def_attrs = An.get_type_attrs an1;
      }

(*
  Mapping from Mapping to OCaml
*)

let rec ocaml_of_expr_mapping
    (env : R.env) (x : (R.t, _) Mapping.t) : ocaml_expr =
  match x with
  | Unit (_, Unit, _) -> `Name ("unit", [])
  | Bool (_, Bool, _) -> `Name ("bool", [])
  | Int (_, Int x, _) -> `Name (R.string_of_ocaml_int x, [])
  | Float (_, Float, _) -> `Name ("float", [])
  | String (_, String, _) -> `Name ("string", [])
  | Sum (_, a, Sum kind, _) ->
      let l = Array.to_list a in
      `Sum (kind, List.map (ocaml_of_variant_mapping env) l)
  | Record (_, a, Record _, _) ->
      let l = Array.to_list a in
      `Record (Record, List.map (ocaml_of_field_mapping env) l)
  | Tuple (_, a, _, _) ->
      let l = Array.to_list a in
      `Tuple (List.map (fun (x : _ Mapping.cell_mapping) ->
        ocaml_of_expr_mapping env x.cel_value) l)
  | List (_, x, List kind, _) ->
      `Name (R.string_of_ocaml_list kind, [ocaml_of_expr_mapping env x])
  | Option (_, x, Option, _) ->
      `Name ("option", [ocaml_of_expr_mapping env x])
  | Nullable (_, x, Nullable, _) ->
      `Name ("option", [ocaml_of_expr_mapping env x])
  | Wrap _ ->
      assert false
  | Name (_, s, l, _, _) ->
      `Name (s, List.map (ocaml_of_expr_mapping env) l)
  | Tvar (_, s) ->
      `Tvar s
  | Abstract _ -> `Name ("Yojson.Safe.t", [])
  | _ -> assert false

and ocaml_of_variant_mapping env x : ocaml_variant =
  let o =
    match x.var_arepr with
        Variant o -> o
      | _ -> assert false
  in
  (o.ocaml_cons,
   Option.map (ocaml_of_expr_mapping env) x.var_arg,
   o.ocaml_vdoc)

and ocaml_of_field_mapping env x : ocaml_field =
  let o =
    match x.f_arepr with
        Field o -> o
      | _ -> assert false
  in
  let v = ocaml_of_expr_mapping env x.f_value in
  ((o.ocaml_fname, o.ocaml_mutable), v, o.ocaml_fdoc)


(*
  Pretty-printing
*)

module Format_types = struct

  open Easy_format

  let rlist = { list with
                wrap_body = `Force_breaks;
                indent_body = 0;
                align_closing = false;
                space_after_opening = false;
                space_before_closing = false
              }

  let plist = { list with
                align_closing = false;
                space_after_opening = false;
                space_before_closing = false }

  let hlist = { list with wrap_body = `No_breaks }
  let shlist = { hlist with
                 stick_to_label = false;
                 space_after_opening = false;
                 space_before_closing = false }

  let llist = {
    list with
    separators_stick_left = false;
    space_before_separator = true;
    space_after_separator = true
  }

  let lplist = {
    llist with
    space_after_opening = false;
    space_before_closing = false
  }

  let vlist1 = { list with stick_to_label = false }

  let vlist = {
    vlist1 with
    wrap_body = `Force_breaks;
  }



let make_atom s = Atom (s, atom)

let horizontal_sequence l = Easy_format.List (("", "", "", shlist), l)

let vertical_sequence ?(skip_lines = 0) l =
  let l =
    if skip_lines = 0 then l
    else
      let sep = List.init skip_lines (fun _ -> (Atom ("", atom))) in
      List.insert_sep l ~sep
  in
  Easy_format.List (("", "", "", rlist), l)

let escape f s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match f c with
        None -> Buffer.add_char buf c
      | Some s -> Buffer.add_string buf s
  done;
  Buffer.contents buf

let ocamldoc_escape s =
  let esc = function
      '{' | '}' | '[' | ']' | '@' | '\\' as c -> Some (sprintf "\\%c" c)
    | _ -> None
  in
  escape esc s

let ocamldoc_verbatim_escape s =
  let esc = function
      '{' | '}' | '\\' as c -> Some (sprintf "\\%c" c)
    | _ -> None
  in
  escape esc s

let split = Re.Str.split (Re.Str.regexp " ")


let make_ocamldoc_block = function
  | Atd.Doc.Pre s -> Atom ("\n{v\n" ^ ocamldoc_verbatim_escape s ^ "\nv}", atom)
  | Paragraph l ->
      let l = List.map (function
        | Atd.Doc.Text s -> ocamldoc_escape s
        | Code s -> "[" ^ ocamldoc_escape s ^ "]"
      ) l
      in
      let words = split (String.concat "" l) in
      let atoms = List.map (fun s -> Atom (s, atom)) words in
      List (("", "", "", plist), atoms)

let rec make_ocamldoc_blocks = function
  | []
  | [_] as l -> List.map make_ocamldoc_block l
  | x :: (y :: _ as xs) ->
      let rest = make_ocamldoc_blocks xs in
      let rest =
        match y with
        | Atd.Doc.Paragraph _ -> Atom ("", atom) :: rest
        | Pre _ -> rest in
      make_ocamldoc_block x :: rest

let make_ocamldoc_comment l =
  let blocks = make_ocamldoc_blocks l in
  let xlist =
    match l with
      [] | [_] -> vlist1
    | _ -> vlist
  in
  Easy_format.List (("(**", "", "*)", xlist), blocks)

let prepend_ocamldoc_comment doc x =
  match doc with
      None -> x
    | Some y ->
        let comment = make_ocamldoc_comment y in
        Easy_format.List (("", "", "", rlist), [comment;x])

let append_ocamldoc_comment x doc =
  match doc with
      None -> x
    | Some y ->
        let comment = make_ocamldoc_comment y in
        Label ((x, label), comment)

let format_pp_conv_node node = function
  | Camlp4 []
  | Ppx_deriving []
  | Ppx [] -> node
  | converters ->
    let attr value = "[@@" ^ value ^ "]" in
    let converters =
      match converters with
      | Ppx_deriving cs -> attr ("deriving " ^ (String.concat ", " cs))
      | Camlp4 cs -> "with " ^ (String.concat ", " cs)
      | Ppx cs -> List.map attr cs |> String.concat "" in
    Label ((node, label), make_atom converters)

let rec format_module_item pp_convs
    is_first (def : ocaml_def) =
  let type_ = if is_first then "type" else "and" in
  let s, param = def.o_def_name in
  let alias = def.o_def_alias in
  let expr = def.o_def_expr in
  let doc = def.o_def_doc in
  (* TODO: currently replacing, globally set pp_convs, maybe should merge? *)
  let pp_convs =
    match def.o_def_attrs with
    | [] -> pp_convs
    | attrs -> Ppx attrs
  in
  let append_if b s1 s2 =
    if b then s1 ^ s2
    else s1
  in
  let part1 =
    horizontal_sequence (
      make_atom type_ ::
        prepend_type_param param
        [ make_atom (append_if (alias <> None || expr <> None) s " =") ]
    )
  in
  let part12 =
    match alias with
        None -> part1
      | Some (name, param) ->
          let right =
            horizontal_sequence (
              prepend_type_param param
                [ make_atom (append_if (expr <> None) name " =") ]
            )
          in
          Label (
            (part1, label),
            right
          )
  in
  let part123 =
    match expr with
        None -> part12

      | Some t ->
          Label (
            (part12, label),
            format_type_expr t
          )
  in
  format_pp_conv_node (prepend_ocamldoc_comment doc part123) pp_convs


and prepend_type_param l tl =
  match l with
      [] -> tl
    | _ ->
        let make_var s = make_atom ("'" ^ s) in
        let x =
          match l with
              [s] -> make_var s
            | l -> List (("(", ",", ")", plist), List.map make_var l)
        in
        x :: tl

and prepend_type_args l tl =
  match l with
      [] -> tl
    | _ ->
        let x =
          match l with
              [t] -> format_type_expr t
            | l -> List (("(", ",", ")", plist), List.map format_type_expr l)
        in
        x :: tl

and format_type_expr x =
  match x with
      `Sum (kind, l) ->
        let op, cl =
          match kind with
              Classic -> "", ""
            | Poly -> "[", "]"
        in
        List (
            (op, "|", cl, llist),
            List.map (format_variant kind) l
          )
    | `Record (kind, l) ->
        let op, cl =
          match kind with
              Record -> "{", "}"
            | Object -> "<", ">"
        in
        List (
          (op, ";", cl, list),
          List.map format_field l
        )
    | `Tuple l ->
        List (
          ("(", "*", ")", lplist),
          List.map format_type_expr l
        )
    | `Name (name, args) ->
        format_type_name name args

    | `Tvar name ->
        make_atom ("'" ^ name)

and format_type_name name args =
  horizontal_sequence (prepend_type_args args [ make_atom name ])

and format_field ((s, is_mutable), t, doc) =
  let l =
    let l = [make_atom (s ^ ":")] in
    if is_mutable then
      make_atom "mutable" :: l
    else l
  in
  let field =
    Label (
      (horizontal_sequence l, label),
      format_type_expr t
    )
  in
  append_ocamldoc_comment field doc

and format_variant kind (s, o, doc) =
  let s = tick kind ^ s in
  let cons = make_atom s in
  let variant =
    match o with
        None -> cons
      | Some t ->
          Label (
            (cons, label),
            Label (
              (make_atom "of", label),
              format_type_expr t
            )
          )
  in
  append_ocamldoc_comment variant doc

let format_module_items pp_convs (l : ocaml_module_body) =
  match l with
      x :: l ->
        format_module_item pp_convs true x ::
          List.map (fun x -> format_module_item pp_convs false x) l
    | [] -> []

let format_module_bodies pp_conv (l : (bool * ocaml_module_body) list) =
  List.concat_map (fun (_, x) -> format_module_items pp_conv x) l

let format_head (loc, an) =
  match Atd.Doc.get_doc loc an with
      None -> []
    | Some doc -> [make_ocamldoc_comment doc]

let format_all l =
  vertical_sequence ~skip_lines:1 l


let ocaml_of_expr x : string =
  Easy_format.Pretty.to_string (format_type_expr x)

let ocaml_of_atd ?(pp_convs=Ppx_deriving []) ~target ~type_aliases
    (head, imports, (defs : (bool * type_def list) list)) : string =
  let head = format_head head in
  (* TODO: figure out what to do with the imports. Add them to a table
     and check that all references to external modules were imported. *)
(*
  let imports = check_imports imports in
*)
  let bodies =
    List.map (fun (is_rec, type_defs) ->
      let type_defs =
        List.filter_map (map_def ~target ~type_aliases) type_defs
      in
      (is_rec,type_defs)
    ) defs
  in
  let body = format_module_bodies pp_convs bodies in
  let x = format_all (head @ body) in
  Easy_format.Pretty.to_string x
end
