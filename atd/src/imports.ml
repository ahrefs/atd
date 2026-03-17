(*
   Manage external definitions via 'from ... import' statements.
*)

open Printf
open Ast

(* Map local module name to import info. *)
type t = (string, import) Hashtbl.t

(* Use a type-safe hash table keyed on string list (module path). *)
module PathTbl = Hashtbl.Make (struct
  type t = string list
  let equal = ( = )
  let hash = Hashtbl.hash
end)

let load imports =
  (* keep track of full module paths that were already loaded *)
  let globals : unit PathTbl.t = PathTbl.create 16 in
  (* our main table: local module name -> import *)
  let locals = Hashtbl.create 100 in
  imports
  |> List.iter (fun (x : import) ->
    let name = x.name in
    if Hashtbl.mem locals name then
      error_at x.loc
        (sprintf
{|Local module name '%s' is already used by another import.
Consider using 'as' to give it a non-conflicting name.|}
          name
        )
    else if PathTbl.mem globals x.path then
      error_at x.loc
        (sprintf "Module '%s' is imported twice." (String.concat "." x.path))
    else begin
      (* Check for duplicate type names within this import statement. *)
      let seen_types = Hashtbl.create 16 in
      List.iter (fun (it : imported_type) ->
        if Hashtbl.mem seen_types it.it_name then
          error_at x.loc
            (sprintf "Type '%s' appears more than once in the import of module '%s'."
               it.it_name (String.concat "." x.path))
        else
          Hashtbl.add seen_types it.it_name ()
      ) x.types;
      Hashtbl.add locals name x;
      PathTbl.add globals x.path ()
    end
  );
  locals

(* Resolve a qualified or unqualified type name.
   Returns (Some (import, imported_type), base_name) for qualified names
   that were explicitly imported, or (None, base_name) for unqualified names.
   Validation that qualified names are in the import list is done separately
   by check_type_refs; this function is used during codegen after validation. *)
let resolve locals loc (x : type_name) =
  match Type_name.split x with
  | None, base_name -> None, base_name
  | Some module_name, base_name ->
      (match Hashtbl.find_opt locals module_name with
       | None ->
           error_at loc (sprintf
{|Unknown module name '%s'.
Hint: add 'from %s import %s' at the top of the file.|}
             module_name module_name base_name)
       | Some import ->
           let it_opt =
             List.find_opt (fun (it : imported_type) -> it.it_name = base_name)
               import.types
           in
           Some (import, it_opt), base_name
      )

(* Warn about imported type names that are never referenced in any type
   expression. Warnings go to stderr and do not abort parsing. *)
let warn_unused_imports locals type_defs =
  (* Collect (local_module_name, base_type_name) pairs used in type exprs. *)
  let used = Hashtbl.create 16 in
  List.iter (fun (def : type_def) ->
    ignore (fold (fun type_expr () ->
      (match type_expr with
       | Name (_, (_, TN path, _), _) ->
           (match Type_name.split (TN path) with
            | None, _ -> ()
            | Some module_name, base_name ->
                Hashtbl.replace used (module_name, base_name) ())
       | _ -> ())
    ) def.value ())
  ) type_defs;
  (* Collect warnings, then sort by source position for stable output. *)
  let warnings = ref [] in
  Hashtbl.iter (fun _local_name (import : import) ->
    List.iter (fun (it : imported_type) ->
      if not (Hashtbl.mem used (import.name, it.it_name)) then
        warnings :=
          (import.loc, it.it_name, String.concat "." import.path) :: !warnings
    ) import.types
  ) locals;
  List.sort
    (fun (loc1, name1, _) (loc2, name2, _) ->
       let c = Loc.compare loc1 loc2 in
       if c <> 0 then c else compare name1 name2)
    !warnings
  |> List.iter (fun (loc, type_name, module_path) ->
    eprintf "%s:\nWarning: Type '%s' was imported from module '%s' but is never used.\n"
      (string_of_loc loc) type_name module_path
  )

(* Walk all type expressions in type_defs and verify that:
   - every qualified type reference 'a.b' has module 'a' in the import table;
   - type 'b' was listed in the 'from a import ...' statement;
   - the arity used matches the declared arity. *)
let check_type_refs locals type_defs =
  List.iter (fun (def : type_def) ->
    let check_expr type_expr () =
      match type_expr with
      | Name (loc, (_, TN path, args), _) ->
          (match Type_name.split (TN path) with
           | None, _ -> ()   (* unqualified: local type, fine *)
           | Some module_name, base_name ->
               (match Hashtbl.find_opt locals module_name with
                | None ->
                    error_at loc (sprintf
{|Unknown module name '%s'.
Hint: add 'from %s import %s' at the top of the file.|}
                      module_name module_name base_name)
                | Some import ->
                    (match List.find_opt (fun (it : imported_type) ->
                               it.it_name = base_name) import.types with
                     | None ->
                         error_at loc (sprintf
{|Type '%s' was not imported from module '%s'.
Hint: add '%s' to the import list: from %s import ..., %s|}
                           base_name module_name
                           base_name (String.concat "." import.path) base_name)
                     | Some it ->
                         let declared_arity = List.length it.it_params in
                         let used_arity = List.length args in
                         if declared_arity <> used_arity then
                           error_at loc (sprintf
{|Type '%s.%s' was imported with arity %d but used with arity %d.|}
                             module_name base_name declared_arity used_arity)
                    )
               )
          )
      | _ -> ()
    in
    ignore (fold check_expr def.value ())
  ) type_defs
