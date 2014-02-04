open Easy_format
open Atd_ast

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

let horizontal_sequence l = List (("", "", "", shlist), l)
let horizontal_sequence0 l = List (("", "", "", shlist0), l)

let quote_string s = Printf.sprintf "%S" s

let list_map_filter f g l =
  let rec aux acc = function
    | [] -> acc
    | x::q -> if f x then aux ((g x)::acc) q else aux acc q
  in
  List.rev (aux [] l)

let format_prop (k, (_, opt)) =
  match opt with
      None -> make_atom k
    | Some s ->
        Label (
          (make_atom (k ^ "="), label0),
          (make_atom (quote_string s))
        )

let default_annot (s, (_, l)) =
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


let string_of_field k fk =
  match fk with
      `Required -> k
    | `Optional -> "?" ^ k
    | `With_default -> "~" ^ k


let make_closures format_annot filter_names =

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

  let rec format_module_item (x : module_item) =
    match x with
        `Type (_, (s, param, a), t) ->
          let left =
            if a = [] then
              let l =
                make_atom "type" ::
                  prepend_type_param param
                  [ make_atom (s ^ " =") ]
              in
              horizontal_sequence l
            else
              let l =
                make_atom "type"
                :: prepend_type_param param [ make_atom s ]
              in
              let x = append_annots a (horizontal_sequence l) in
              horizontal_sequence [ x; make_atom "=" ]
          in
          Label (
            (left, label),
            format_type_expr t
          )



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
        `Sum (_, l, a) ->
          append_annots a (
            List (
              ("[", "|", "]", llist),
              List.map format_variant l
            )
          )
      | `Record (_, l, a) ->
          append_annots a (
            List (
              ("{", ";", "}", list),
              List.map format_field l
            )
          )
      | `Tuple (_, l, a) ->
          append_annots a (
            List (
              ("(", "*", ")", lplist),
              List.map format_tuple_field l
            )
          )

      | `List (loc, t, a) ->
          format_type_name "list" [t] a

      | `Option (loc, t, a) ->
          format_type_name "option" [t] a

      | `Nullable (loc, t, a) ->
          format_type_name "nullable" [t] a

      | `Shared (loc, t, a) ->
          format_type_name "shared" [t] a

      | `Wrap (loc, t, a) ->
          format_type_name "wrap" [t] a

      | `Name (_, (_, name, args), a) ->
          format_type_name name args a

      | `Tvar (_, name) ->
          make_atom ("'" ^ name)

  and format_type_name name args a =
    append_annots a (
      horizontal_sequence (prepend_type_args args [ make_atom name ])
    )

  and format_inherit t =
    horizontal_sequence [ make_atom "inherit"; format_type_expr t ]

  and format_tuple_field (loc, x, a) =
    prepend_colon_annots a (format_type_expr x)

  and format_field x =
    match x with
        `Field (_, (k, fk, a), t) ->
          Label (
            (horizontal_sequence0 [
               append_annots a (make_atom (string_of_field k fk));
               make_atom ":"
             ], label),
            format_type_expr t
          )
      | `Inherit (_, t) -> format_inherit t

  and format_variant x =
    match x with
        `Variant (_, (k, a), opt) ->
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
      | `Inherit (_, t) -> format_inherit t
  in

  let format_full_module ((loc, an), l) =
    List (
      ("", "", "", rlist),
      List.map format_annot an @ list_map_filter (fun (`Type (_, (s, _, _), _)) -> filter_names s) format_module_item l
    )
  in

  format_full_module, format_type_name

let default_filter = fun s -> true

let format ?(annot = default_annot) ?(filter_names = default_filter) x =
  let f, _ = make_closures annot filter_names in
  f x

let default_format, default_format_type_name = make_closures default_annot default_filter

let string_of_type_name name args an =
  let x = default_format_type_name name args an in

  Easy_format.Pretty.to_string x
