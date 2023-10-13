(*
   Compare two ATD ASTs for incompatibilities

   Specifically, we're focused on incompatibilities in JSON data and we
   honor <json ...> annotations that affect the shape of JSON data.

   Types are compared structurally i.e. changes in type names don't matter
   as long as they don't affect the JSON representation of data.
   For example, 'int list' and 'nums' are equivalent if 'nums' is defined
   as 'type nums = int list'.

   Comparing two type expressions occurring within a definition requires
   replacing the type names by the type expression they stand for
   (if possible). Instantiating type arguments needs to be done carefully.
   We should report no incompatibility for the following example:

   Old:
     type 'a item = [ Thing of 'a ]
     type 'a items = ('a item * int item)

   New:
     type 'label item = [ Thing of 'label ]
     type thingie = [ Thing of int ]
     type 'a items = ('a item * thingie)

   See the test files for a complete list of expected results for different
   scenarios.
*)

open Printf
open Types
module A = Atd.Ast

(* Sets of names: type names, variant names, field names.
   It's used to detect names that are in common between the two ATD files,
   as well as names that occur only in one of the files. *)
module Strings = Set.Make(String)

(* This is used for instanciating type variables. *)
module Env = Map.Make(String)

type options = {
  (* Are fields with defaults always populated in old JSON data? *)
  json_defaults_old : bool;
  (* Are fields with defaults always populated in new JSON data? *)
  json_defaults_new : bool;
}

let is_builtin_name =
  let tbl = Atd.Predef.make_table () in
  fun name -> Hashtbl.mem tbl name

let kind_of_expr (e : A.type_expr) =
  match e with
  | Name (_, (_, name, _args), _) ->
      if is_builtin_name name then
        name
      else
        "unresolved type name"
  | Sum _ -> "sum type or enum"
  | Record _ -> "record/object"
  | Tuple _ -> "tuple"
  | List _ -> "list/array"
  | Option _ -> "option"
  | Nullable _ -> "nullable"
  | Shared _ -> "shared"
  | Wrap _ -> "wrap"
  | Tvar _ -> "type variable"

let a noun =
  match noun with
  | "" -> "??"
  | s ->
      match s.[0] with
      | 'a' | 'e' | 'i' | 'o' | 'u' -> "an"
      | _ -> "a"

let a_kind_of_expr e =
  let noun = kind_of_expr e in
  (a noun) ^ " " ^ noun

let get_type_names defs =
  List.map (fun (_loc, (name, _param, _an), expr) -> name) defs

(* Split type names into:
   - left-only
   - intersection
   - right-only
*)
let split_types types1 types2 =
  let a = Strings.of_list types1 in
  let b = Strings.of_list types2 in
  Strings.diff a b, Strings.inter a b, Strings.diff b a

let report_deleted_types defs name_set =
  List.filter_map (fun (loc, (name, _param, _an), _expr) ->
    if Strings.mem name name_set then
      Some (name, {
        direction = Backward;
        kind = Deleted_type;
        location_old = Some loc;
        location_new = None;
        description =
          sprintf "The definition for type '%s' no longer exists."
            name
      })
    else
      None
  ) defs

let report_added_types defs name_set =
  List.filter_map (fun (loc, (name, _param, _an), _expr) ->
    if Strings.mem name name_set then
      Some (name, {
        direction = Forward;
        kind = Deleted_type;
        location_old = None;
        location_new = Some loc;
        description = sprintf "There is a new type named '%s'." name
      })
    else
      None
  ) defs

(* Produce a read-only table of type definitions *)
let make_def_table (defs : A.type_def list) : (string, A.type_def) Hashtbl.t =
  let tbl = Hashtbl.create 100 in
  List.iter (fun ((loc, (name, _param, _an), _expr) as def) ->
    Hashtbl.replace tbl name def
  ) defs;
  tbl

let get_def def_tbl name : A.type_def =
  match Hashtbl.find_opt def_tbl name with
  | None -> invalid_arg ("get_def: " ^ name)
  | Some def -> def

(*
   Create an environment that normalizes the names for the type variables:
     ('k, 'v) t
   produces the environment:
     'k -> '0
     'v -> '1

   This allows the following normalization:
     type ('k, 'v) t = ('k * 'v foo) list
   ->
     type ('0, '1) t = ('0 * '1 foo) list
*)
let create_normalized_environment loc type_params =
  (* type_params is the list of named type parameters for a parametrized
     type definition:
       type ('k, 'v) t = ...
     -> ["k"; "v"]
  *)
  let numbered = List.mapi (fun i var -> (string_of_int i, var)) type_params in
  let new_params = List.map fst numbered in
  let env =
    List.fold_left (fun env (new_var, var) ->
      Env.add var (A.Tvar (loc, new_var)) env
    ) Env.empty numbered
  in
  new_params, env

(*
   Replace all the type variables occurring in a type expression.
   Fail in case of an unbound variable.
*)
let replace_type_variables env e =
  let replace_var (e : A.type_expr) =
    match e with
    | Tvar (loc, var) ->
        (match Env.find_opt var env with
         | None ->
             A.error_at loc (sprintf "Unbound type parameter '%s" var)
         | Some e -> e)
    | e -> e
  in
  A.Map.type_expr { type_expr = replace_var } e

let create_env_from_pairs bindings =
  List.fold_left (fun env (k, v) -> Env.add k v env) Env.empty bindings

(*
   This normalization allows comparing parametrized type definitions e.g.

     type 'a t = 'a list   vs.  type 'elt t = 'elt list

   becomes

     type '0 t = ' list   vs.  type '0 t = '0 list
*)
let normalize_type_params_in_definition (def : A.type_def) : A.type_def =
  let loc, (name, params, an), e = def in
  let new_params, env = create_normalized_environment loc params in
  let new_e = replace_type_variables env e in
  (loc, (name, new_params, an), new_e)

(*
   Resolve the type expression into one that is not a name, if possible.

   The result may only be a name if the name can't be resolved.
   It occurs in the following cases:
   - cyclic type definition
   - predefined types (bool, int, list, ...)
   We could revise this to guarantee that the returned node is never
   a 'Name' if it helps.
*)
let deref_expr def_tbl orig_e =
  let rec deref_expr env e =
    match (e : A.type_expr) with
    | Name (_loc, (loc2, name, args), _an) ->
        if is_builtin_name name then
          e
        else
          let (_loc, (_name, param, _an), e) = get_def def_tbl name in
          let e = replace_type_variables env e in
          let n_param = List.length param in
          let n_args = List.length args in
          if n_param <> n_args then
            A.error_at loc2
              (sprintf "Type '%s' expects %i arguments but %i were given"
                 name n_param n_args)
          else
            let bindings = List.combine param args in
            let new_env = create_env_from_pairs bindings in
            check_deref_expr new_env e
    | Sum _
    | Record _
    | Tuple _
    | List _
    | Option _
    | Nullable _
    | Shared _
    | Tvar _ as e -> e
    | Wrap _ -> assert false
  and check_deref_expr env e =
    if e = orig_e then
      (* cycle *)
      e
    else
      deref_expr env e
  in
  deref_expr Env.empty orig_e

let unwrap_option (kind : A.field_kind) (e : A.type_expr) : A.type_expr =
  match kind with
  | Optional ->
      (match e with
      | Option (_, e, _)
      | Name (_, (_, "option", [e]), _) -> e
      | Sum _
      | Record _
      | Tuple _
      | List _
      | Nullable _
      | Shared _
      | Wrap _
      | Name _
      | Tvar _ ->
          (* It's an error but we let it be.
             Maybe atdgen supports having a name that's
             an alias for an option type; we shouldn't support this in
             new implementations. *)
          e
      )
  | Required
  | With_default -> e

let report_structural_mismatches options def_tbl1 def_tbl2 shared_types =
  let get_expr1 e = deref_expr def_tbl1 e in
  let get_expr2 e = deref_expr def_tbl2 e in
  let findings : (string * finding) list ref = ref [] in
  (*
     Table of pairs of type expressions that were already compared or whose
     comparison is in progress. This prevent against infinite expansion
     of recursive types.
  *)
  let visited_tbl = Hashtbl.create 100 in
  let was_visited def_name e1 e2 =
    let key = (def_name, e1, e2) in
    Hashtbl.mem visited_tbl key
  in
  let mark_visited def_name e1 e2 =
    let key = (def_name, e1, e2) in
    Hashtbl.replace visited_tbl key ()
  in
  let cmp_expr def_name e1 e2 =
    let add x =
      findings := (def_name, x) :: !findings in
    let rec cmp_expr e1 e2 : unit =
      if not (was_visited def_name e1 e2) then (
        mark_visited def_name e1 e2;
        really_cmp_expr e1 e2
      )
    and really_cmp_expr e1 e2 : unit =
      match get_expr1 e1, get_expr2 e2 with
      | Sum (loc1, cases1, _an1), Sum (loc2, cases2, _an2) ->
          (* TODO: compare JSON annotations an1 and an2 *)
          cmp_variants loc1 loc2 cases1 cases2
      | Record (loc1, fields1, an1), Record (loc2, fields2, an2) ->
          (* TODO: compare JSON annotations an1 and an2 *)
          cmp_fields loc1 loc2 fields1 fields2
      | Tuple (loc1, cells1, an1), Tuple (loc2, cells2, an2) ->
          (* TODO: compare JSON annotations an1 and an2? *)
          cmp_tuple_cells loc1 loc2 cells1 cells2
      | List (loc1, e1, an1), List (loc2, e2, an2) ->
          (* TODO: compare JSON annotations an1 and an2? *)
          cmp_expr e1 e2
      | Option (loc1, e1, an1), Option (loc2, e2, an2)
      | Nullable (loc1, e1, an1), Nullable (loc2, e2, an2)
      | Shared (loc1, e1, an1), Shared (loc2, e2, an2) ->
          (* TODO: compare JSON annotations an1 and an2? *)
          cmp_expr e1 e2
      | Name (_, (loc1, name1, args1), _an1),
        Name (_, (loc2, name2, args2), _an2) ->
          if name1 <> name2 then
            add {
              direction = Both;
              kind = Incompatible_type;
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Type names '%s' and '%s' are not the same and \
                         may not be compatible."
                  name1 name2;
            }
          else
            ()
      | Wrap _, _ | _, Wrap _ -> assert false
      | Tvar (loc1, var1), Tvar (loc2, var2) ->
          (* Type variables were normalized so they can be compared
             directly. *)
          if var1 <> var2 then
            add {
              direction = Both;
              kind = Incompatible_type;
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Incompatible type variables are being used.";
            }
      | (Sum _
        | Record _
        | Tuple _
        | List _
        | Option _
        | Nullable _
        | Shared _
        | Name _
        | Tvar _) as e1, e2 ->
          add {
            direction = Both;
            kind = Incompatible_type;
            location_old = Some (A.loc_of_type_expr e1);
            location_new = Some (A.loc_of_type_expr e2);
            description =
              sprintf "Incompatible kinds of types: %s is now %s."
                (kind_of_expr e1) (a_kind_of_expr e2);
          }
    and cmp_variants loc1 loc2 cases1 cases2 =
      let with_names cases =
        (* TODO: apply annotations *)
        cases
        |> List.map (fun (x : A.variant) ->
          match x with
          | Variant (loc, (name, an), opt_e) -> name, (loc, (name, an), opt_e)
          | Inherit _ -> assert false
        )
      in
      let named1 = with_names cases1 in
      let named2 = with_names cases2 in
      let names1 = List.map fst named1 |> Strings.of_list in
      let names2 = List.map fst named2 |> Strings.of_list in
      let left_only = Strings.diff names1 names2 |> Strings.elements in
      let shared = Strings.inter names1 names2 |> Strings.elements in
      let right_only = Strings.diff names2 names1 |> Strings.elements in
      left_only
      |> List.iter (fun name ->
        let loc, (_name, _an), _opt_e = List.assoc name named1 in
        add {
          direction = Backward;
          kind = Missing_variant { variant_name = name };
          location_old = Some loc;
          location_new = None;
          description = sprintf "Case '%s' disappeared." name
        }
      );
      right_only
      |> List.iter (fun name ->
        let loc, (_name, _an), _opt_e = List.assoc name named2 in
        add {
          direction = Backward;
          kind = Missing_variant { variant_name = name };
          location_old = None;
          location_new = Some loc;
          description = sprintf "Case '%s' is new." name
        }
      );
      shared
      |> List.iter (fun name ->
        let loc1, _, e1 = List.assoc name named1 in
        let loc2, _, e2 = List.assoc name named2 in
        match e1, e2 with
        | None, None -> ()
        | Some e1, Some e2 ->
            cmp_expr e1 e2
        | Some _, None ->
            add {
              direction = Both;
              kind = Missing_variant_argument { variant_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Case '%s' no longer has an argument." name
            }
        | None, Some _ ->
            add {
              direction = Both;
              kind = Missing_variant_argument { variant_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Case '%s' used to not have an argument." name
            }
      )
    and cmp_fields loc1 loc2 fields1 fields2 =
      let with_names fields =
        (* TODO: apply annotations *)
        fields
        |> List.map (fun (field : A.field) ->
          match field with
          | `Field ((_, (name, _, _), _) as x) -> name, x
          | `Inherit _ -> assert false
        )
      in
      let named1 = with_names fields1 in
      let named2 = with_names fields2 in
      let names1 = List.map fst named1 |> Strings.of_list in
      let names2 = List.map fst named2 |> Strings.of_list in
      let left_only = Strings.diff names1 names2 |> Strings.elements in
      let shared = Strings.inter names1 names2 |> Strings.elements in
      let right_only = Strings.diff names2 names1 |> Strings.elements in
      left_only
      |> List.iter (fun name ->
        let loc, (_name, kind, _an), _e = List.assoc name named1 in
        match kind with
        | Optional
        | With_default -> ()
        | Required ->
            add {
              direction = Forward;
              kind = Missing_field { field_name = name };
              location_old = Some loc;
              location_new = None;
              description = sprintf "Required field '%s' disappeared." name
            }
      );
      right_only
      |> List.iter (fun name ->
        let loc, (_name, kind, _an), _opt_e = List.assoc name named2 in
        match kind with
        | Optional
        | With_default -> ()
        | Required ->
            add {
              direction = Backward;
              kind = Missing_field { field_name = name };
              location_old = None;
              location_new = Some loc;
              description = sprintf "Required field '%s' is new." name
            }
      );
      shared
      |> List.iter (fun name ->
        let loc1, (_, kind1, _), e1 = List.assoc name named1 in
        let loc2, (_, kind2, _), e2 = List.assoc name named2 in
        (* The type of an optional field '?foo: int option' is considered
           to be 'int' rather than 'int option' *)
        let e1 = unwrap_option kind1 e1 in
        let e2 = unwrap_option kind2 e2 in
        (match kind1, kind2 with
         | (Optional | With_default), (Optional | With_default) -> ()
         | Required, Required -> ()
         | Required, With_default
           when options.json_defaults_new -> ()
         | Required, With_default ->
             add {
              direction = Forward;
              kind = Default_required { field_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "\
Formerly required field '%s' is now optional but has a default value.
You must ensure that new implementations always populate the JSON field
with a value (using atdgen's option -j-defaults or equivalent) so that older
implementations can read newer data. If this is already the case, use
'atddiff --json-defaults-new' to disable this warning."
                  name
            }
         | Required, Optional ->
             add {
              direction = Forward;
              kind = Missing_field { field_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Formerly required field '%s' is now optional." name
            }
         | With_default, Required
           when options.json_defaults_old -> ()
         | With_default, Required ->
             add {
              direction = Backward;
              kind = Default_required { field_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "\
Newly required field '%s' was optional but had a default value.
If old implementations in use always populate the JSON field
with a value (using atdgen's option -j-defaults or equivalent),
then there's no problem and you should use
'atddiff --json-defaults-old' to disable this warning."
                  name
            }
         | Optional, Required ->
             add {
              direction = Backward;
              kind = Missing_field { field_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Formerly optional field '%s' is now required." name
            }
        );
        cmp_expr e1 e2
      )

    and cmp_tuple_cells loc1 loc2 cells1 cells2 =
      let n1 = List.length cells1 in
      let n2 = List.length cells2 in
      if n1 <> n2 then
        add {
          direction = Both;
          kind = Incompatible_type;
          location_old = Some (A.loc_of_type_expr e1);
          location_new = Some (A.loc_of_type_expr e2);
          description = sprintf "Incompatible tuple lengths";
        }
      else
        List.iter2 (fun (_loc1, e1, _an1) (_loc2, e2, _an2) ->
          (* TODO: honor annotations? *)
          cmp_expr e1 e2
        ) cells1 cells2
    in
    cmp_expr e1 e2
  in
  List.iter (fun name ->
    let (loc1, (_name, param, _an), e1) = get_def def_tbl1 name in
    let (loc2, (_name, param, _an), e2) = get_def def_tbl2 name in
    cmp_expr name e1 e2
  ) shared_types;
  !findings

let finding_group (a : finding) =
  match a.location_old, a.location_new with
  | None, None -> (* should probably not exist *) 1
  | Some _, None -> 2
  | None, Some _ -> 3
  | Some _, Some _ -> 4

let compare_opt_location (a : Atd.Loc.t option) (b : Atd.Loc.t option) =
    match a, b with
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0
  | Some a, Some b -> Atd.Loc.compare a b

(* Compare results so as to sort them in the following order:
   1. Exists only in the old file
   2. Exists only in the new file
   3a. Position in the old file
   3b. Position in the new file
   4. Everything else (expected to not occur)
*)
let compare_findings (a : finding) (b : finding) =
  (* sort by left-only, right-only, both *)
  let c = Int.compare (finding_group a) (finding_group b) in
  if c <> 0 then c
  else
    (* within a group, sort normally by location *)
    let c = compare_opt_location a.location_old b.location_old in
    if c <> 0 then c
    else
      let c = compare_opt_location a.location_new b.location_new in
      if c <> 0 then c
      else Stdlib.compare a b

let group_and_sort_findings xs =
  let tbl = Hashtbl.create 100 in
  xs
  |> List.iter (fun (name, finding) ->
    match Hashtbl.find_opt tbl finding with
    | None ->
        Hashtbl.add tbl finding (ref (Strings.singleton name))
    | Some names ->
        names := Strings.add name !names
  );
  Hashtbl.fold (fun finding names acc ->
    (finding, Strings.elements !names) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> compare_findings a b)

(*
   Expectations:
   - the ASTs have been monomorphized. Any polymorphic type definition
     was removed. The only remaining parametrized types are the builtins
     'list', 'option', etc.
   - where a node substitution occurred in the AST, the location was preserved
     so as to point accurately to the source code location.
*)
let asts options (ast1 : A.full_module) (ast2 : A.full_module) : result =
  let _head1, body1 = ast1 in
  let _head2, body2 = ast2 in
  let extract_normalized_defs body =
    body
    |> List.map (function A.Type x -> x)
    |> List.map normalize_type_params_in_definition
  in
  let defs1 = extract_normalized_defs body1 in
  let defs2 = extract_normalized_defs body2 in
  let def_tbl1 = make_def_table defs1 in
  let def_tbl2 = make_def_table defs2 in
  let type_names1 = get_type_names defs1 in
  let type_names2 = get_type_names defs2 in
  let left_only, shared, right_only =
    split_types type_names1 type_names2
  in
  let findings = [
    report_deleted_types defs1 left_only;
    report_added_types defs2 right_only;
  ] in
  let findings =
    report_structural_mismatches options def_tbl1 def_tbl2
      (Strings.elements shared)
    :: findings
  in
  findings
  |> List.flatten
  |> group_and_sort_findings
