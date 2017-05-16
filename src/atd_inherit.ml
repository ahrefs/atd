(*
  Perform inheritance.
*)


open Printf

open Atd_ast

module S = Set.Make (String)


let load_defs l =
  let tbl = Atd_predef.make_table () in
  List.iter (
    fun ((_, (k, pl, _), _) as td) ->
      Hashtbl.add tbl k (List.length pl, Some td)
  ) l;
  tbl

let keep_last_defined get_name l =
  let set, l =
    List.fold_right (
      fun x (set, l) ->
        let k = get_name x in
        if S.mem k set then (set, l)
        else (S.add k set, x :: l)
    ) l (S.empty, [])
  in
  l

let get_field_name : field -> string = function
    `Field (loc, (k, _, _), _) -> k
  | `Inherit _ -> assert false

let get_variant_name : variant -> string = function
    `Variant (loc, (k, _), _) -> k
  | `Inherit _ -> assert false


let expand ?(inherit_fields = true) ?(inherit_variants = true) tbl t0 =

  let rec subst deref param (t : type_expr) : type_expr =
    match t with
        `Sum (loc, vl, a) ->
          let vl = List.flatten (List.map (subst_variant param) vl) in
          let vl =
            if inherit_variants then
              keep_last_defined get_variant_name vl
            else
              vl
          in
          `Sum (loc, vl, a)

      | `Record (loc, fl, a) ->
          let fl = List.flatten (List.map (subst_field param) fl) in
          let fl =
            if inherit_fields then
              keep_last_defined get_field_name fl
            else
              fl
          in
          `Record (loc, fl, a)

      | `Tuple (loc, tl, a) ->
          `Tuple (
            loc,
            List.map (fun (loc, x, a) -> (loc, subst false param x, a)) tl, a
          )

      | `List (loc, t, a)
      | `Name (loc, (_, "list", [t]), a) ->
          `List (loc, subst false param t, a)

      | `Option (loc, t, a)
      | `Name (loc, (_, "option", [t]), a) ->
          `Option (loc, subst false param t, a)

      | `Nullable (loc, t, a)
      | `Name (loc, (_, "nullable", [t]), a) ->
          `Nullable (loc, subst false param t, a)

      | `Shared (loc, t, a)
      | `Name (loc, (_, "shared", [t]), a) ->
          `Shared (loc, subst false param t, a)

      | `Wrap (loc, t, a)
      | `Name (loc, (_, "wrap", [t]), a) ->
          `Wrap (loc, subst false param t, a)

      | `Tvar (loc, s) ->
          (try List.assoc s param
           with Not_found -> t)

      | `Name (loc, (loc2, k, args), a) ->
          let expanded_args = List.map (subst false param) args in
          if deref then
            let k, vars, a, t =
              try
                match Hashtbl.find tbl k with
                    n, Some (_, (k, vars, a), t) -> k, vars, a, t
                  | n, None -> failwith ("Cannot inherit from type " ^ k)
              with Not_found ->
                failwith ("Missing type definition for " ^ k)
            in
            let param = List.combine vars expanded_args in
            subst true param t
          else
            `Name (loc, (loc2, k, expanded_args), a)

  and subst_field param = function
      `Field (loc, k, t) -> [ `Field (loc, k, subst false param t) ]
    | `Inherit (loc, t) as x ->
        (match subst true param t with
             `Record (loc, vl, a) ->
               if inherit_fields then vl
               else [ x ]
           | _ -> failwith "Not a record type"
        )

  and subst_variant param = function
      `Variant (loc, k, opt_t) as x ->
        (match opt_t with
             None -> [ x ]
           | Some t -> [ `Variant (loc, k, Some (subst false param t)) ]
        )
    | `Inherit (loc, t) as x ->
        (match subst true param t with
             `Sum (loc, vl, a) ->
               if inherit_variants then vl
               else [ x ]
           | _ -> failwith "Not a sum type"
        )
  in
  subst false [] t0


let expand_module_body
    ?inherit_fields
    ?inherit_variants
    (l : Atd_ast.module_body) =
  let td_list = List.map (function `Type td -> td) l in
  let tbl = load_defs td_list in
  let td_list =
    List.map (
      fun (loc, name, t) ->
        (loc, name, expand ?inherit_fields ?inherit_variants tbl t)
    ) td_list in
  List.map (fun td -> `Type td) td_list
