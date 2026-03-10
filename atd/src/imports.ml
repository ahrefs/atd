(*
   Manage external definitions via 'import' statements.
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
  (* keep track of full module names that were already loaded *)
  let globals : unit PathTbl.t = PathTbl.create 16 in
  (* our main table *)
  let locals = Hashtbl.create 100 in
  imports
  |> List.iter (fun (x : import) ->
    let name = x.name in
    if Hashtbl.mem locals name then
      error_at x.loc
        (sprintf
{|Local module name %s is shadowing another module of the same local name.
Consider using 'as' to give it a non-conflicting name.|}
          name
        )
    else if PathTbl.mem globals x.path then
      error_at x.loc
        (sprintf "Module %s is loaded twice." (String.concat "." x.path))
    else (
      Hashtbl.add locals name x;
      PathTbl.add globals x.path ()
    )
  );
  locals

let resolve locals loc (x : type_name) =
  match Type_name.split x with
  | None, base_name -> None, base_name
  | Some module_name, base_name ->
      (match Hashtbl.find_opt locals module_name with
       | None ->
           error_at loc (sprintf
{|Unknown module name %s.
Hint:
  import %s
or
  import xxx as %s
|}
             module_name module_name module_name)
       | Some import ->
           Some import, base_name
      )
