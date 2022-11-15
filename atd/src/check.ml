(* Semantic verification *)

open Import

open Ast

let add_name accu = function
    Name (_, (_, k, _), _) -> k :: accu
  | _ -> accu

let get_kind = function
    Sum _ -> `Sum
  | Record _ -> `Record
  | _ -> `Other

let check_inheritance def_tbl (t0 : type_expr) =
  let not_a kind _ =
    let msg =
      sprintf "Cannot inherit from non-%s type"
        (match kind with
           `Sum -> "variant"
         | `Record -> "record"
         | _ -> assert false)
    in
    error_at (loc_of_type_expr t0) msg
  in

  let rec check kind inherited (t : type_expr) =
    match t with
      Sum (_, vl, _) when kind = `Sum ->
        List.iter (fun (x : variant) ->
          match x with
          | Inherit (_, t) -> check kind inherited t
          | Variant _ -> ()
        ) vl

    | Record (_, fl, _) when kind = `Record ->
        List.iter (fun (x : field) ->
          match x with
            | Inherit (_, t) -> check kind inherited t
            | Field _ -> ()
        ) fl

    | Sum _
    | Record _
    | Tuple _
    | List _
    | Option _
    | Nullable _
    | Shared _
    | Wrap _ as x -> not_a kind x

    | Name (_, (loc, k, _), _) ->
        if List.mem k inherited then
          error_at (loc_of_type_expr t0) "Cyclic inheritance"
        else
          let (_arity, opt_def) =
            try Hashtbl.find def_tbl k
            with Not_found -> error_at loc ("Undefined type " ^ k)
          in
          (match opt_def with
             None -> ()
           | Some x -> check kind (x.name :: inherited) x.value
          )

    | Tvar _ ->
        error_at (loc_of_type_expr t0) "Cannot inherit from a type variable"

  in

  check (get_kind t0) (add_name [] t0) t0


let check_type_expr def_tbl tvars (t : type_expr) =
  let rec check : type_expr -> unit = function
      Sum (_, vl, _) as x ->
        List.iter (check_variant (Hashtbl.create 10)) vl;
        check_inheritance def_tbl x

    | Record (_, fl, _) as x ->
        List.iter (check_field (Hashtbl.create 10)) fl;
        check_inheritance def_tbl x

    | Tuple (_, tl, _) -> List.iter (fun (_, x, _) -> check x) tl
    | List (_, t, _) -> check t
    | Option (_, t, _) -> check t
    | Nullable (_, t, _) -> check t
    | Shared (loc, t, _) ->
        if Ast.is_parametrized t then
          error_at loc "Shared type cannot be polymorphic";
        check t
    | Wrap (_, t, _) -> check t

    | Name (_, (loc, k, tal), _) ->
        assert (k <> "list" && k <> "option"
                && k <> "nullable" && k <> "shared" && k <> "wrap");
        let (arity, _opt_def) =
          try Hashtbl.find def_tbl k
          with Not_found -> error_at loc ("Undefined type " ^ k)
        in
        let n = List.length tal in
        if arity <> n then
          error_at loc (sprintf "Type %s was defined to take %i parameters, \
                                 but %i argument%s."
                          k arity n (if n > 1 then "s are given"
                                     else " is given")
                       );

        List.iter check tal

    | Tvar (loc, s) ->
        if not (List.mem s tvars) then
          error_at loc (sprintf "Unbound type variable '%s" s)


  and check_variant accu = function
      Variant (loc, (k, _), opt_t) ->
        if Hashtbl.mem accu k then
          error_at loc
            (sprintf
               "Multiple definitions of the same variant constructor %s" k);
        Hashtbl.add accu k ();
        (match opt_t with
           None -> ()
         | Some t -> check t)

    | Inherit (_, t) ->
        (* overriding is allowed, for now without a warning *)
        check t

  and check_field accu = function
    | Field (loc, (k, _, _), t) ->
        if Hashtbl.mem accu k then
          error_at loc
            (sprintf "Multiple definitions of the same field %s" k);
        Hashtbl.add accu k ();
        check t

    | Inherit (_, t) ->
        (* overriding is allowed, for now without a warning *)
        check t
  in

  check t


let check (x : Ast.module_) =
  let predef = Predef.make_table () in
  let tbl = Hashtbl.copy predef in

  (* first pass: put all definitions in the table *)
  List.iter (fun (x : type_def) ->
    let name = x.name in
    let loc = x.loc in
    if Hashtbl.mem tbl name then
      if Hashtbl.mem predef name then
        error_at loc
          (sprintf "%s is a predefined type, it cannot be redefined." name)
      else
        error_at loc
          (sprintf "Type %s is defined for the second time." name)
    else
      Hashtbl.add tbl name (List.length x.param, Some x)
  ) x.type_defs;

  (* second pass: check existence and arity of types in type expressions,
     check that inheritance is not cyclic *)
  List.iter (fun (x : type_def) ->
    check_type_expr tbl x.param x.value
  ) x.type_defs
