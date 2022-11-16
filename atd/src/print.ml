open Easy_format
open Ast

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
let shlist0 = { shlist with space_after_separator = false }

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

let label0 = { label with space_after_label = false }

let make_atom s = Atom (s, atom)

let horizontal_sequence l = Easy_format.List (("", "", "", shlist), l)
let horizontal_sequence0 l = Easy_format.List (("", "", "", shlist0), l)

let quote_string s = Printf.sprintf "%S" s

let format_prop (k, (_, opt)) =
  match opt with
    None -> make_atom k
  | Some s ->
      Label (
        (make_atom (k ^ "="), label0),
        (make_atom (quote_string s))
      )

let default_format_annot (s, (_, l)) =
  match l with
    [] -> make_atom ("<" ^ s ^ ">")
  | l ->
      List (
        ("<", "", ">", plist),
        [
          Label (
            (make_atom s, label),
            List (
              ("", "", "", plist),
              List.map format_prop l
            )
          )
        ]
      )


let string_of_field k = function
    Required -> k
  | Optional -> "?" ^ k
  | With_default -> "~" ^ k


let format ?(format_annot = default_format_annot) any =

  let append_annots (l : annot) x =
    match l with
      [] -> x
    | _ ->
        Label (
          (x, label),
          List (("", "", "", plist), List.map format_annot l)
        )
  in

  let prepend_colon_annots l x =
    match l with
      [] -> x
    | _ ->
        Label (
          (Label (
             (List (("", "", "", plist), List.map format_annot l), label0),
             make_atom ":"
           ),
           label),
          x
        )
  in

  let rec prepend_type_param l tl =
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
        let x : t =
          match l with
            [t] -> format_type_expr t
          | l -> List (("(", ",", ")", plist), List.map format_type_expr l)
        in
        x :: tl

  and format_type_expr x =
    match x with
      Sum (_, l, a) ->
        append_annots a (
          List (
            ("[", "|", "]", llist),
            List.map format_variant l
          )
        )
    | Record (_, l, a) ->
        append_annots a (
          List (
            ("{", ";", "}", list),
            List.map format_field l
          )
        )
    | Tuple (_, l, a) ->
        append_annots a (
          List (
            ("(", "*", ")", lplist),
            List.map format_cell l
          )
        )

    | List (_, t, a) ->
        format_type_name "list" [t] a

    | Option (_, t, a) ->
        format_type_name "option" [t] a

    | Nullable (_, t, a) ->
        format_type_name "nullable" [t] a

    | Shared (_, t, a) ->
        format_type_name "shared" [t] a

    | Wrap (_, t, a) ->
        format_type_name "wrap" [t] a

    | Name (_, (_, name, args), a) ->
        format_type_name name args a

    | Tvar (_, name) ->
        make_atom ("'" ^ name)

  and format_type_name name args a =
    append_annots a (
      horizontal_sequence (prepend_type_args args [ make_atom name ])
    )

  and format_inherit t =
    horizontal_sequence [ make_atom "inherit"; format_type_expr t ]

  and format_cell (_, x, a) =
    prepend_colon_annots a (format_type_expr x)

  and format_field x =
    match x with
    | Field (_, (k, fk, a), t) ->
        Label (
          (horizontal_sequence0 [
             append_annots a (make_atom (string_of_field k fk));
             make_atom ":"
           ], label),
          format_type_expr t
        )
    | Inherit (_, t) -> format_inherit t

  and format_variant x =
    match x with
      Variant (_, (k, a), opt) ->
        let cons = append_annots a (make_atom k) in
        (match opt with
           None -> cons
         | Some t ->
             Label (
               (cons, label),
               Label (
                 (make_atom "of", label),
                 format_type_expr t
               )
             )
        )
    | Inherit (_, t) -> format_inherit t
  in

  let format_import ({ loc = _; name; alias; annot } : import) =
    let opt_alias =
      match alias with
      | None -> []
      | Some local_name -> [make_atom ("as " ^ local_name)]
    in
    make_atom "import" :: make_atom name :: opt_alias
    |> horizontal_sequence
    |> append_annots annot
  in

  let format_type_def (x : type_def) =
    let left =
      if x.annot = [] then
        let l =
          make_atom "type" ::
          prepend_type_param x.param
            [ make_atom (x.name ^ " =") ]
        in
        horizontal_sequence l
      else
        let l =
          make_atom "type"
          :: prepend_type_param x.param [ make_atom x.name ]
        in
        let x = append_annots x.annot (horizontal_sequence l) in
        horizontal_sequence [ x; make_atom "=" ]
    in
    Label (
      (left, label),
      format_type_expr x.value
    )
  in

  let format_module (x : module_) =
    Easy_format.List (
      ("", "", "", rlist),
      List.map format_annot (snd x.module_head)
      @ List.map format_import x.imports
      @ List.map format_type_def x.type_defs
    )
  in

  let format_any (x : any) =
    match x with
    | Module x -> format_module x
    | Import x -> format_import x
    | Type_def x -> format_type_def x
    | Type_expr x -> format_type_expr x
    | Variant x -> format_variant x
    | Cell x -> format_cell x
    | Field x -> format_field x
  in

  format_any any

let to_string ?format_annot x =
  format ?format_annot x
  |> Easy_format.Pretty.to_string

let string_of_type_name name args an =
  let loc = dummy_loc in
  to_string (Type_expr (Name (loc, (loc, name, args), an)))
